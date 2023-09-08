# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of CohortGeneratorModule
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Module methods -------------------------
getModuleInfo <- function() {
  checkmate::assert_file_exists("MetaData.json")
  return(ParallelLogger::loadSettingsFromJson("MetaData.json"))
}

getSharedResourceByClassName <- function(sharedResources, className) {
  returnVal <- NULL
  for (i in 1:length(sharedResources)) {
    if (className %in% class(sharedResources[[i]])) {
      returnVal <- sharedResources[[i]]
      break
    }
  }
  invisible(returnVal)
}

createCohortDefinitionSetFromJobContext <- function(sharedResources, settings) {
  cohortDefinitions <- list()
  if (length(sharedResources) <= 0) {
    stop("No shared resources found")
  }
  cohortDefinitionSharedResource <- getSharedResourceByClassName(
    sharedResources = sharedResources,
    class = "CohortDefinitionSharedResources"
  )
  if (is.null(cohortDefinitionSharedResource)) {
    stop("Cohort definition shared resource not found!")
  }
  cohortDefinitions <- cohortDefinitionSharedResource$cohortDefinitions
  if (length(cohortDefinitions) <= 0) {
    stop("No cohort definitions found")
  }
  cohortDefinitionSet <- CohortGenerator::createEmptyCohortDefinitionSet()
  for (i in 1:length(cohortDefinitions)) {
    cohortJson <- cohortDefinitions[[i]]$cohortDefinition
    cohortDefinitionSet <- rbind(cohortDefinitionSet, data.frame(
      cohortId = as.integer(cohortDefinitions[[i]]$cohortId),
      cohortName = cohortDefinitions[[i]]$cohortName,
      json = cohortJson,
      stringsAsFactors = FALSE
    ))
  }
  return(cohortDefinitionSet)
}

setCovariateSchemaTable <- function(
    modelDesignList, 
    cohortDatabaseSchema,
    cohortTable
){
  
  if(inherits(modelDesignList, 'modelDesign')){
    modelDesignList <- list(modelDesignList)
  }
  
  for(i in 1:length(modelDesignList)){
    covariateSettings <- modelDesignList[[i]]$covariateSettings
    
    if(inherits(covariateSettings, 'covariateSettings')){
      covariateSettings <- list(covariateSettings)
    }
    
    for(j in 1:length(covariateSettings)){
      
      if('cohortDatabaseSchema' %in% names(covariateSettings[[j]])){
        covariateSettings[[j]]$cohortDatabaseSchema <- cohortDatabaseSchema
      }
      if('cohortTable' %in% names(covariateSettings[[j]])){
        covariateSettings[[j]]$cohortTable <- cohortTable
      }
      
    }
    
    modelDesignList[[i]]$covariateSettings <- covariateSettings
  }
  
  return(modelDesignList)
}

# Module methods -------------------------
execute <- function(jobContext) {
  library(DeepPatientLevelPrediction)
  rlang::inform("Validating inputs")
  inherits(jobContext, "list")

  if (is.null(jobContext$settings)) {
    stop("Analysis settings not found in job context")
  }
  if (is.null(jobContext$sharedResources)) {
    stop("Shared resources not found in job context")
  }
  if (is.null(jobContext$moduleExecutionSettings)) {
    stop("Execution settings not found in job context")
  }

  workFolder <- jobContext$moduleExecutionSettings$workSubFolder
  resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

  rlang::inform("Executing deepPLP")
  moduleInfo <- getModuleInfo()

  # Creating database details
  databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
    connectionDetails = jobContext$moduleExecutionSettings$connectionDetails,
    cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
    cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    cdmDatabaseName = jobContext$moduleExecutionSettings$connectionDetailsReference,
    cdmDatabaseId = jobContext$moduleExecutionSettings$databaseId,
    # tempEmulationSchema =  , is there s temp schema specified anywhere?
    cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
    outcomeDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    outcomeTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
  )

  # find where cohortDefinitions are as sharedResources is a list
  cohortDefinitionSet <- createCohortDefinitionSetFromJobContext(
    sharedResources = jobContext$sharedResources,
    settings = jobContext$settings
  )
  
  # set the covariate settings schema and tables 
  jobContext$settings <- setCovariateSchemaTable(
    modelDesignList = jobContext$settings, 
    cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
  )
  
  # run the models
  PatientLevelPrediction::runMultiplePlp(
    databaseDetails = databaseDetails,
    modelDesignList = jobContext$settings,
    cohortDefinitions = cohortDefinitionSet,
    saveDirectory = workFolder
  )

  # Export the results
  rlang::inform("Export data to csv files")

  sqliteConnectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = file.path(workFolder, "sqlite", "databaseFile.sqlite")
  )

  PatientLevelPrediction::extractDatabaseToCsv(
    connectionDetails = sqliteConnectionDetails,
    databaseSchemaSettings = PatientLevelPrediction::createDatabaseSchemaSettings(
      resultSchema = "main", # sqlite settings
      tablePrefix = "", # sqlite settings
      targetDialect = "sqlite",
      tempEmulationSchema = NULL
    ),
    csvFolder = file.path(resultsFolder),
    fileAppend = NULL
  )
}

uploadResultsCallback <- function(jobContext) {
  connectionDetails <- jobContext$moduleExecutionSettings$resultsConnectionDetails
  moduleInfo <- ParallelLogger::loadSettingsFromJson("MetaData.json")
  tablePrefix <- moduleInfo$TablePrefix
  schema <- jobContext$moduleExecutionSettings$resultsDatabaseSchema
  
  resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
  
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  
  databaseSchemaSettings <- PatientLevelPrediction::createDatabaseSchemaSettings(
    resultSchema = schema, 
    tablePrefix = tablePrefix, 
    targetDialect = connectionDetails$dbms
  )
  
  PatientLevelPrediction::insertCsvToDatabase(
    csvFolder = resultsFolder,
    conn = conn, 
    databaseSchemaSettings = databaseSchemaSettings,
    modelSaveLocation = file.path(resultsFolder, 'dbmodels'),
    csvTableAppend = ''
  )
  
}

createDataModelSchema <- function(jobContext) {
  connectionDetails <- jobContext$moduleExecutionSettings$resultsConnectionDetails
  moduleInfo <- ParallelLogger::loadSettingsFromJson("MetaData.json")
  tablePrefix <- moduleInfo$TablePrefix
  schema <- jobContext$moduleExecutionSettings$resultsDatabaseSchema
  
  PatientLevelPrediction::createPlpResultTables(
      connectionDetails = connectionDetails, 
        targetDialect = connectionDetails$dbms, 
        resultSchema = schema, 
        deleteTables = F,
        createTables = T, 
        tablePrefix = tablePrefix
      )
}

