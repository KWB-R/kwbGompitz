# readPredictionFile -----------------------------------------------------------

#' Read Prediction Result File
#' 
#' @param file full path to prediction result file (predict<n>.txt) with <n>
#'   being the strategy number
#' @param sep column separator, default: ";"
#' @param stringsAsFactors if \code{TRUE} character columns will appear as
#'   factors in the result, passed to \code{\link{data.frame}}. The default is 
#'   \code{FALSE}.
#' @export
readPredictionFile <- function(file, sep = ";", stringsAsFactors = FALSE)
{
  # Remove and report warnings in the file
  textlines <- .warningsRemoved(readLines(kwb.utils::safePath(file)))

  # Remove the header line if any
  textlines <- grep("^GompitZ", textlines, value = TRUE, invert = TRUE)

  fields <- strsplit(textlines, sep)

  indices <- seq_along(fields[[1]])

  columnList <- lapply(indices, function(i) .readTypeCastedColumn(fields, i))

  names(columnList) <- .predictionColNames(ncol = length(columnList))

  do.call(data.frame, c(columnList, stringsAsFactors = stringsAsFactors))
}

# readPredictionFileWithDataTable ----------------------------------------------
#' Read Predictions File with data.table
#'
#' @param file file 
#' @param sep sep (default: ";")
#'
#' @return ???
#' @export
#'
#' @keywords internal
#' @importFrom data.table fread
#' @importFrom stats setNames
readPredictionFileWithDataTable <- function(file, sep = ";")
{
  prediction <- data.table::fread(file, sep = sep, data.table = FALSE)
  
  stats::setNames(prediction, .predictionColNames(ncol = ncol(prediction)))
}

# .warningsRemoved -------------------------------------------------------------
.warningsRemoved <- function(x)
{
  pattern <- "^Warning:\\s*"
  
  isWarning <- grepl(pattern, x)

  if (any(isWarning)) {

    texts <- gsub(pattern, "", x[isWarning])

    warning_(sprintf(
      "Gompred reported %d warnings:\n- %s",
      sum(isWarning), kwb.utils::collapsed(texts, "\n- ")
    ))
  }

  x[! isWarning]
}

# .readTypeCastedColumn --------------------------------------------------------
.readTypeCastedColumn <- function(fields, i)
{
  values <- sapply(fields, FUN = "[", i)

  if (i != 2) {
    as.numeric(values)
  } else {
    values
  }
}

# .predictionColNames ----------------------------------------------------------
#' .predictionColNames
#'
#' @param ncol ncol  
#'
#' @return ???
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom kwb.utils moveToFront
.predictionColNames <- function(ncol)
{
  columns <- c("stratumNo", "PipeID", "PredictionYear", "index", "pipeLength")

  n.prob.columns <- ncol - length(columns)

  stopifnot(n.prob.columns > 0)

  prob.columns <- paste0("prob", seq_len(n.prob.columns))

  kwb.utils::moveToFront(c(prob.columns, columns), columns[1:3])
}
