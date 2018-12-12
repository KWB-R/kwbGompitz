# composeGompitzInputData ------------------------------------------------------

#' Compose Input Data for Gompitz Functions
#' 
#' Compose input data for Gompitz functions \code{\link{runGompitzCalibration}},
#' \code{\link{runGompitzPrediction}}
#' 
#' @param masterdata data.frame containing master data as retrieved by 
#'   \code{\link{composeMasterData}}
#' @param covariates data.frame containing covariates in columns. Must have as
#'   many rows as masterdata
#' @param weight weight to be given for each inspection (each row) in
#'   \emph{masterdata}. Should be a vector of same length as there are rows in
#'   \emph{masterdata}. Default: 1
#' @param covariates.status matrix of covariate status as retrieved by 
#'   \code{\link{createStatusMatrix}}
#' @param condition.labels All possible condition labels, e.g. c("1", "2", "3",
#'   "4"). Default: unique values in column "condition" of \emph{masterdata}
#' @param warn if TRUE (default), a warning is given if \emph{weight} does not
#'   have the expected length
#'   
#' @return list with elements \emph{masterdata} (data.frame), \emph{covariates} 
#'   (data.frame), \emph{weight} (numeric vector), \emph{covariates.status} 
#'   (numeric matrix) and \emph{condition.labels} (character vector)
#' 
#' @export
#' 
composeGompitzInputData <- function(
  masterdata, covariates, weight, covariates.status, condition.labels = NULL,
  warn = FALSE
)
{
  n <- nrow(masterdata)

  stopifnot(nrow(covariates) == n)

  if (length(weight) != n) {

    weight <- rep(weight, length.out = nrow(masterdata))

    message_text <- sprintf(
      "Argument 'weight' has been recycled to a vector of length %d.", n
    )
    
    if (warn) {
      
      warning_(message_text)
      
    } else {
      
      message(message_text)
    }
  }

  if (is.null(condition.labels)) {
  
    labels <- as.character(kwb.utils::selectColumns(masterdata, "condition"))
    labels <- labels[! kwb.utils::isNaOrEmpty(labels)]
    
    condition.labels <- sort(unique(labels))
  }
  
  list(
    masterdata = masterdata,
    covariates = covariates,
    weight = weight,
    covariates.status = covariates.status,
    condition.labels = condition.labels
  )
}

# composeMasterData ------------------------------------------------------------

#' Prepare "Master Data" for Gompitz Functions
#' 
#' Prepare "master data" (stratum, pipe-ID, year of installation, year of 
#' inspection, condition class) for Gompitz functions
#' 
#' @param stratum Stratum alphanumerical label
#' @param pipeid Pipeline Identifier alphanumerical label
#' @param instyear Installation year (integer)
#' @param inspyear Inspection Year (integer - void field if not inspected)
#' @param condition Condition Class alphanumerical label
#'   
#' @return data.frame with columns \emph{stratum}, \emph{pipeid},
#'   \emph{instyear}, \emph{inspyear}, \emph{condition},
#'   
#' @seealso \code{\link{composeGompitzInputData}}
#'   
composeMasterData <- function(stratum, pipeid, instyear, inspyear, condition)
{
  data.frame(
    stratum = stratum,
    pipeid = pipeid,
    instyear = instyear,
    inspyear = inspyear,
    condition = condition
  )
}

# createStatusMatrix -----------------------------------------------------------

#' Default Covariate Status Matrix
#' 
#' Default covariate status matrix
#' 
#' @param strata names of strata (e.g. unique values in column \emph{stratum} of
#'   masterdata)
#' @param covariateNames names of covariates
#' @param default.other default status value to be used in any cell of the
#'   matrix, except the cells in column \emph{LENGTH}. Default: 3
#' @param default.length status value to be used in column \emph{LENGTH}.
#'   Default: 0
#' @param covariates names of columns in \code{Data} that shall be used as
#'   covariates
#' @param Data data frame with one row per inspection
#' @param column.strata name of column in \code{Data} containing the stratifying
#'   variable. Default: "Material_cat", i.e. strata are built by material
#' @param column.length name of column in \code{Data} containing the pipe
#'   lengths
#' @param note if \code{TRUE} (default) a note about the length column is given
#'
#' @export
#'  
createStatusMatrix <- function(
  strata = NULL, 
  covariateNames = NULL,
  default.other = 3,
  default.length = 0,
  covariates = "Length_num",
  Data = NULL,
  column.strata = "Material_cat",
  column.length = NULL, 
  note = TRUE
)
{
  if (is.null(strata) && ! is.null(Data) && ! is.null(column.strata)) {
    
    strata <- sort(unique(kwb.utils::selectColumns(Data, column.strata)))
  }

  if (length(strata) == 0) {
    
    stop_(
      "No stata defined. Either specify the argument 'strata' or the ",
      "arguments 'Data' and 'column.strata'. In the latter case the ",
      "different occurring strata will be looked up in Data."
    )
  }
  
  if (is.null(covariateNames)) {
    
    if (is.null(covariates)) {
      
      stop_(
        "No covariate names are given. Either set the argument ",
        "'covariateNames' or 'covariates' to a vector of covariate names ",
        "or set the argument 'covariates' to a named vector of integer ",
        "values (0, 1, 2, or 3) where each value represents the staus ",
        "(see gompcal documentation) for the covariates given by the ",
        "corresponding element names."
      )
      
    } else {
      
      names.covariates <- names(covariates)
      
      if (is.character(covariates)) {
        
        covariateNames <- covariates
        
      } else if (! is.null(names.covariates)) {
        
        covariateNames <- names.covariates
        
      } else {
        
        stop_(
          "covariates is neither a vector of character nor a named vector ",
          "of integer nor a named list of integer vetors!"
        )
      }
    }
  }
  
  status_matrix <- kwb.utils::createMatrix(
    rowNames = as.character(strata),
    colNames = as.character(covariateNames),
    value = default.other,
    name.row = kwb.utils::defaultIfNULL(column.strata, "stratum"),
    name.col = "Covariate"
  )

  # Which column represents the pipe length?
  if (is.null(column.length)) {

    column.length <- colnames(status_matrix)[1]

    if (isTRUE(note)) {
      message(
        sprintf("Note: the first covariate (%s) ",
                kwb.utils::hsQuoteChr(covariateNames[1])),
        "is assumed to refer to the column in Data containing the pipe lengths!"
      )
    }
  }

  status_matrix[, column.length] <- default.length

  # Adapt the status matrix to your needs
  if (! is.null(covariates) && ! is.character(covariates)) {
    
    assignments <- as.list(covariates)

    lengths <- sapply(assignments, length)

    if (! all(lengths == 1 | lengths == nrow(status_matrix))) {
      
      stop_(
        "The lengths of the vectors given in 'covariates' must be either ",
        "one or equal to the number of strata (", nrow(status_matrix), ": ",
        kwb.utils::stringList(as.character(strata)), ")."
      )
    }

    status_matrix <- kwb.utils::setMatrixColumns(
      m = status_matrix, columnValuePairs = assignments
    )
  }

  structure(status_matrix, column.length = column.length)
}
