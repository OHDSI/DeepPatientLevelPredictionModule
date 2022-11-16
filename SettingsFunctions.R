createDeepPatientLevelPredictionModuleSpecifications <- function(
  modelDesignList
) {
  #analysis <- list()
  #for (name in names(formals(createCohortDiagnosticsModuleSpecifications))) {
  #  analysis[[name]] <- get(name)
  #}
  
  specifications <- list(
    module = "DeepPatientLevelPredictionModule",
    version = "0.0.1",
    remoteRepo = "github.com",
    remoteUsername = "ohdsi",
    settings = modelDesignList
  )
  class(specifications) <- c("DeepPatientLevelPredictionModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}