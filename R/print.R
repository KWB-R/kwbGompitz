# print.gompitz.calibration ----------------------------------------------------

#' Print a GompitZ Calibration Structure
#' 
#' @param x list of class "gompitz.calibration" as returned by
#'   \code{\link{runGompitzCalibration}}
#' @param \dots further arguments (not used)
#' @return print GompitZ Calibration Structure 
#' @export  
#' @importFrom kwb.utils getAttribute stringList
print.gompitz.calibration <- function(x, ...)
{
  sourceFile <- kwb.utils::getAttribute(x, "source")

  cat(sprintf(
    "The result of gompcal.exe has been read from '%s' in:\n  '%s'\n",
    basename(sourceFile), dirname(sourceFile)
  ))

  for (stratum in names(x)) {

    catHeader(char = "=", sprintf(
      "\nGompitz Calibration for Stratum '%s'", stratum
    ))

    print.gompitz_stratum_calib(x[[stratum]])
  }

  cat(
    "\nAvailable user attributes:",
    kwb.utils::stringList(setdiff(names(attributes(x)), c("names", "class")))
  )
}

# print.gompitz_stratum_calib --------------------------------------------------

#' Print the Calibration for one Stratum
#' 
#' @param x list of class "gompitz_stratum_calib" as contained in a calibration
#'   object returned by \code{\link{runGompitzCalibration}}
#' @param \dots further arguments (not used)
#' @return print the Calibration for one Stratum
#' @export  
#' @importFrom kwb.utils defaultIfNULL moveColumnsToFront printIf rbindAll 
#' selectElements
print.gompitz_stratum_calib <- function(x, ...)
{
  get <- kwb.utils::selectElements
  
  stratum <- kwb.utils::defaultIfNULL(x$stratum, "stratum?")

  catHeader(char = "-", sprintf("\nInput Statistics ('%s')", stratum))

  elements <- c("pipes", "inspections")

  stats <- lapply(get(x, elements), as.data.frame)
  stats <- kwb.utils::rbindAll(stats, nameColum = "subject")
  stats <- kwb.utils::moveColumnsToFront(stats, "subject")

  kwb.utils::printIf(TRUE, stats, "\nTotal")

  caption <- "\nBy condition"
  kwb.utils::printIf(TRUE, get(x, "conditions"), caption)

  catHeader(char = "-", sprintf("\nConvergence ('%s')", stratum))

  printConvergence(x = get(x, "convergence"))
}

# printConvergence -------------------------------------------------------------

#' Print the Convergence Information
#' 
#' @param x \code{NULL} or a list with elements \code{num.iterations},
#'   \code{log.likelihood}, \code{covariances}, \code{estimates}, 
#' @return Print the convergence information
#' @export  
#' @importFrom kwb.utils catLines printIf selectElements 
printConvergence <- function(x)
{
  get <- kwb.utils::selectElements
  
  if (is.null(x)) {

    cat("The model did not converge for this stratum.\n")

  } else {

    kwb.utils::catLines(c(
      sprintf("Iterations:      %8d", get(x, "num.iterations")),
      sprintf("Log. likelihood: %8.2f", get(x, "log.likelihood"))
    ))

    kwb.utils::printIf(TRUE, get(x, "covariances"), "\nCovariances")
    kwb.utils::printIf(TRUE, get(x, "estimates"), "\nEstimates")
  }
}

# catStructure -----------------------------------------------------------------

#' Print the Structure of an Object
#' 
#' @param x any R object
#' @param max.level passed to \code{\link[utils]{capture.output}}
#' @return print the structure of an object
#' @export
#' @importFrom utils capture.output str
#' @importFrom kwb.utils catLines
catStructure <- function(x, max.level = NA)
{
  output <- utils::capture.output(utils::str(x, max.level = max.level))
  
  kwb.utils::catLines(output)
}

# catHeader --------------------------------------------------------------------

#' Print an underlined Header
#' 
#' @param x caption
#' @param char character used for the underline
#' @export
catHeader <- function(x, char)
{
  cat(sprintf("%s\n%s\n", x, underline(nchar(x), char)))
}

# underline --------------------------------------------------------------------

#' Create a String to be used as an Underline
#' 
#' @param n number of characters
#' @param char character used for the underline
#' @export
#' @importFrom kwb.utils collapsed
underline <- function(n = 10, char = "-")
{
  kwb.utils::collapsed(rep(char, n), "")
}
