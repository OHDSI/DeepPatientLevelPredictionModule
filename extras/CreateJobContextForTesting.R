# Create a job context for testing purposes
library(Strategus)
library(dplyr)
library(DeepPatientLevelPrediction)
source("SettingsFunctions.R")

# Generic Helpers ----------------------------
getModuleInfo <- function() {
  checkmate::assert_file_exists("MetaData.json")
  return(ParallelLogger::loadSettingsFromJson("MetaData.json"))
}

# Sample Data Helpers ----------------------------
getSampleCohortDefintionSet <- function() {
  sampleCohorts <- CohortGenerator::createEmptyCohortDefinitionSet()
  cohortJsonFiles <- list.files(path = system.file("testdata/name/cohorts", package = "CohortGenerator"), full.names = TRUE)
  for (i in 1:length(cohortJsonFiles)) {
    cohortJsonFileName <- cohortJsonFiles[i]
    cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
    cohortJson <- readChar(cohortJsonFileName, file.info(cohortJsonFileName)$size)
    sampleCohorts <- rbind(sampleCohorts, data.frame(
      cohortId = i,
      cohortName = cohortName,
      cohortDefinition = cohortJson,
      stringsAsFactors = FALSE
    ))
  }
  sampleCohorts <- apply(sampleCohorts, 1, as.list)
  return(sampleCohorts)
}

createCohortSharedResource <- function(cohortDefinitionSet) {
  sharedResource <- list(cohortDefinitions = cohortDefinitionSet)
  class(sharedResource) <- c("CohortDefinitionSharedResources", "SharedResources")
  return(sharedResource)
}

# Module Settings Spec ----------------------------
makeModelDesignSettings <- function(targetId, outcomeId, popSettings, covarSettings) {
  invisible(PatientLevelPrediction::createModelDesign(
    targetId = targetId,
    outcomeId = outcomeId,
    restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
    populationSettings = popSettings,
    covariateSettings = covarSettings,
    preprocessSettings = PatientLevelPrediction::createPreprocessSettings(),
    modelSettings = DeepPatientLevelPrediction::setResNet(
      numLayers = 2,
      sizeHidden = 64,
      hiddenFactor = 1,
      residualDropout = 0,
      hiddenDropout = 0,
      sizeEmbedding = 32,
      estimatorSettings = setEstimator(learningRate = 3e-4,
                                       weightDecay = 0.0,
                                       device = "cpu",
                                       batchSize = 256,
                                       epochs = 1),
      randomSample = 1
    ),
    splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
    runCovariateSummary = T
  ))
}

plpPopulationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  startAnchor = "cohort start",
  riskWindowStart = 1,
  endAnchor = "cohort start",
  riskWindowEnd = 365,
  minTimeAtRisk = 1
)
plpCovarSettings <- FeatureExtraction::createDefaultCovariateSettings()

modelDesignList <- list()
exposureOfInterestIds <- c(1, 2)
outcomeOfInterestIds <- c(3)
for (i in 1:length(exposureOfInterestIds)) {
  for (j in 1:length(outcomeOfInterestIds)) {
    modelDesignList <- append(
      modelDesignList,
      list(
        makeModelDesignSettings(
          targetId = exposureOfInterestIds[i],
          outcomeId = outcomeOfInterestIds[j],
          popSettings = plpPopulationSettings,
          covarSettings = plpCovarSettings
        )
      )
    )
  }
}

plpModuleSpecifications <- createDeepPatientLevelPredictionModuleSpecifications(modelDesignList = modelDesignList)


analysisSpecifications <- createEmptyAnalysisSpecificiations() %>%
  addSharedResources(createCohortSharedResource(getSampleCohortDefintionSet())) %>%
  addModuleSpecifications(plpModuleSpecifications)

executionSettings <- Strategus::createCdmExecutionSettings(
  connectionDetailsReference = "dummy",
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort"),
  workFolder = "dummy",
  resultsFolder = "dummy",
  minCellCount = 5
)

# Job Context ----------------------------
module <- "CohortGeneratorModule"
moduleIndex <- 1
moduleExecutionSettings <- executionSettings
moduleExecutionSettings$workSubFolder <- "dummy"
moduleExecutionSettings$resultsSubFolder <- "dummy"
moduleExecutionSettings$databaseId <- 123
jobContext <- list(
  sharedResources = analysisSpecifications$sharedResources,
  settings = analysisSpecifications$moduleSpecifications[[moduleIndex]]$settings,
  moduleExecutionSettings = moduleExecutionSettings
)
saveRDS(jobContext, "tests/testJobContext.rds")
