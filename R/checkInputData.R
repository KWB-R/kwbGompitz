# checkInputData ---------------------------------------------------------------
#' Check Input Data
#' 
#' @description Checks if input data is defined properly
#' @param input.data input.data
#'   
#' @return error in case input data was not defined properly
#' @importFrom kwb.utils checkForMissingColumns getAttribute selectElements 
#' stopIfNotMatrix stringList
#' @export
#' 

checkInputData <- function(input.data)
{
  stopifnot(is.list(input.data))
  
  get <- function(element) kwb.utils::selectElements(input.data, element)
  
  masterdata <- get("masterdata")
  covariates <- get("covariates")
  weight <- get("weight")
  covariates_status <- get("covariates.status")
  condition_labels <- get("condition.labels")
  
  stopifnot(is.data.frame(masterdata))
  
  kwb.utils::checkForMissingColumns(masterdata, c(
    "stratum", "pipeid", "instyear", "inspyear", "condition"
  ))
  
  stopifnot(all(stats::na.omit(masterdata$condition) %in% condition_labels))
  
  kwb.utils::stopIfNotMatrix(covariates_status)
  
  column_length <- kwb.utils::getAttribute(covariates_status, "column.length")

  stopifnot(is.data.frame(covariates))
  stopifnot(column_length %in% names(covariates))
  
  strata_occurring <- unique(masterdata$stratum)
  strata_defined <-  rownames(covariates_status)
  
  if (! all(is_defined <- strata_occurring %in% strata_defined)) {
    stop_(
      "The following strata occur in the data but are not configured in the ", 
      "status matrix: ", kwb.utils::stringList(strata_occurring[! is_defined])
    )
  }
}
