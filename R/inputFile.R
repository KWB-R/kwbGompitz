# readObservations -------------------------------------------------------------

#' Read Observation Data from File in GompitZ-Format
#'
#' @param file full path to text file in the format that is required by GompitZ
#'   (see "The GompitZ Tool User's Guide")
#' @param sep column separator
#' @param dbg if \code{TRUE} debug messages are shown, else not.
#' 
readObservations <- function(
  file = exampleFile("obs.txt"), sep = ";", dbg = TRUE
)
{
  old_opt <- options(stringsAsFactors = FALSE)
  on.exit(options(old_opt))
  
  # Read three header lines containing condition, stratum and covariate labels
  names <- c("cond", "strat", "covar")
  rows <- readLines(file, length(names))
  labels <- structure(strsplit(rows, sep), names = names)
  
  if (! "LENGTH" %in% labels$covar) {
    
    stop_(
      "One of the covariates must be 'LENGTH' (see The GompitZ Tool\n",
      "User's Guide -- version 2.08). I found the following covariates:\n",
      kwb.utils::stringList(labels$covar)
    )
  }
  
  kwb.utils::catIf(dbg, sprintf("Reading data file '%s'... ", basename(file)))
  
  columns <- c(
    "stratum", "ident", "instyear", "inspyear", "condition", "weight", 
    labels$covar
  )
  
  skip <- length(names) + length(labels$strat)
  
  result <- utils::read.table(
    file = file, sep = sep, col.names = columns, skip = skip,
    check.names = FALSE
  )
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  statusMatrix <- toStatusMatrix(textlines = readLines(file, skip)[-(1:2)])
  attr(statusMatrix, "column.length") <- "LENGTH"
  
  structure(
    result, condition.labels = labels$cond, strata = labels$strat,
    covariates = labels$covar, statusMatrix = statusMatrix
  )
}

# toStatusMatrix ---------------------------------------------------------------

#' Convert Text Lines to Status Matrix
#'
#' Convert text lines to status matrix as required by 
#' \code{\link[kwb.rsproto]{configure}}
#'
#' @param textlines vector of character linees as they appear in the GompitZ
#'   input file header (only the lines defining the status matrix)
#' @param sep column separator. Default: ";"
#' @param order_rows if \code{TRUE} (default) the rows are ordered by their
#'   name
#'
#' @examples
#' \dontrun{
#' file <- kwbGompitz::exampleFile("obs.txt")
#' kwbGompitz:::toStatusMatrix(textlines = readLines(file, 8)[-(1:2)])
#' }
#' 
toStatusMatrix <- function(textlines, sep = ";", order_rows = FALSE)
{
  text <- kwb.utils::collapsed(textlines[-1], "\n")
  
  x <- utils::read.table(text = text, sep = sep)
  
  names(x)[-1] <- strsplit(textlines, sep)[[1]]
  statusMatrix <- as.matrix(x[, -1])
  rownames(statusMatrix) <- x[, 1]
  
  names(dimnames(statusMatrix)) <- c("stratum", "Covariate")
  
  # Order by row names
  if (order_rows) {
    
    statusMatrix <- statusMatrix[order(rownames(statusMatrix)), ]
  }
  
  statusMatrix
}

# getFileContentForInputFile ---------------------------------------------------

#' Get file Content for Input File
#' 
#' @param masterdata masterdata as returned by \code{\link{composeMasterData}}
#' @param covariates data.frame with as many rows as there are in masterdata and
#'   as many columns as there are covariates, containing the covariate values
#' @param covariates.status matrix of covariate status as retrieved by 
#'   \code{\link{createStatusMatrix}}
#' @param condition.labels condition labels
#' @param weight Weight of the pipeline for the calibration
#' @param sep column separator
#' @param file if a path to a file is given here, the content will be written
#'   to the file instead of returned by this function
#' 
getFileContentForInputFile <- function(
  masterdata, covariates, covariates.status, condition.labels, weight, sep,
  file = NULL
)
{
  header <- inputFileHeader(
    condition.labels = condition.labels,
    covariates = covariates,
    covariates.status = covariates.status,
    sep = sep
  )
  
  if (is.null(file)) {
    
    c(header, inputFileBody(masterdata, covariates, weight, sep))
    
  } else {
    
    writeLines(header, file)
    
    body <- cbind(masterdata, weight = weight, covariates)
    
    data.table::fwrite(body, file, append = TRUE, quote = FALSE, sep = sep)
  }
}

# inputFileHeader --------------------------------------------------------------

#' Calibration Input File Header
#' 
#' @param condition.labels condition labels
#' @param covariates data.frame containing covariates. Needed to determine if
#'   covariates are numeric or categorical
#' @param covariates.status matrix of covariate status as retrieved by 
#'   \code{\link{createStatusMatrix}}
#' @param sep column separator
#' 
#' @seealso \code{kwbGompitz:::inputFileBody}
#' 
inputFileHeader <- function(
  condition.labels, covariates, covariates.status, sep = ";"
)
{
  stopifnot(is.matrix(covariates.status))
  stopifnot(is.numeric(covariates.status))

  strata.labels <- rownames(covariates.status)
  covari.labels <- colnames(covariates.status)

  errorMessage <- function(subject, dimname, names) {
    sprintf(
      paste(
        "The %s must be given as (non-NA) %s names of covariates.status.",
        "The %s names are: %s"
      ),
      subject, dimname, dimname, kwb.utils::stringList(names)
    )
  }

  if (is.null(strata.labels) || any(is.na(strata.labels))) {

    stop_(errorMessage("strata", "row", strata.labels))
  }

  if (is.null(covari.labels) || any(is.na(covari.labels))) {

    stop_(errorMessage("covariates", "column", covari.labels))
  }

  isNumeric <- sapply(
    X = kwb.utils::selectColumns(covariates, covari.labels, drop = FALSE),
    FUN = is.numeric
  )

  # Prefix column name with underscore if it is not numeric and if it is not
  # already prefixed with underscore!
  covari.labels <- prefixSelected(covari.labels, ! isNumeric, "_")

  header.main <- c(
    kwb.utils::collapsed(condition.labels, sep),
    kwb.utils::collapsed(strata.labels, sep),
    kwb.utils::collapsed(covari.labels, sep)
  )

  header.strata <- sapply(strata.labels, function(stratum) {
    kwb.utils::collapsed(c(stratum, covariates.status[stratum, ]), sep)
  })

  c(header.main, header.strata)
}

# prefixSelected ---------------------------------------------------------------

prefixSelected <- function(x, selected, prefix = "_")
{
  stopifnot(length(selected) == length(x))

  selected <- selected & ! grepl(paste0("^", prefix), x)
  x[selected] <- paste0(prefix, x[selected])

  x
}

# inputFileBody ----------------------------------------------------------------

#' Calibration Input File Body
#' 
#' @param masterdata masterdata as returned by \code{\link{composeMasterData}}
#' @param covariates data.frame with as many rows as there are in masterdata and
#'   as many columns as there are covariates, containing the covariate values
#' @param weight Weight of the pipeline for the calibration
#' @param sep column separator
#'    
#' @seealso \code{kwbGompitz:::inputFileHeader}
#' 
inputFileBody <- function(masterdata, covariates, weight, sep)
{
  body <- cbind(masterdata, weight = weight, covariates)
  
  apply(body, 1, FUN = function(x) {
    
    # Replace NA with "" and trim leading or trailing spaces
    kwb.utils::collapsed(kwb.utils::hsTrim(kwb.utils::defaultIfNA(x, "")), sep)
  })
}
