# runGompitzCalibration --------------------------------------------------------

#' Perform GompitZ Calibration
#'
#' @param input.data prepared input data as retrieved by
#'   \code{\link{composeGompitzInputData}}
#' @param subset indexes of rows in \code{input.data} to be used for 
#'   calibration. If \code{NULL} (default), all rows are used.
#' @param verbose verbosity level
#' @param sep column separator
#' @param digits round estimates read from calibr.txt and param.txt,
#'   respectively, to this number of (significant) digits before comparing them
#' @param VERSION name of subdirectory in package containing the binary files to
#'   be executed. Possible values: "unix", "win32", "win32_kwb"
#'
#' @export
#' @examples
#' # For an example, see the Tutorial vignette "How to Use the Package"
#' 
runGompitzCalibration <- function(
  input.data, subset = NULL, verbose = 1, sep = ";", digits = 3,
  VERSION = getOperatingSystemType()
)
{
  gompitz.dir <- tempGompitzDir(verbose)

  files <- c(
    input = "observations_cal.txt",
    calib = "calibr.txt",
    param = "param.txt"
  )

  files <- structure(file.path(gompitz.dir, files), names = names(files))

  if (! is.null(subset)) {
    
    input.data$masterdata <- input.data$masterdata[subset, , drop = FALSE]
    
    input.data$covariates <- input.data$covariates[subset, , drop = FALSE]
    
    input.data$weight <- input.data$weight[subset]
  }

  textlines <- getFileContentForInputFile(
    masterdata = input.data$masterdata,
    covariates = input.data$covariates,
    weight = input.data$weight,
    covariates.status = input.data$covariates.status,
    condition.labels = input.data$condition.labels,
    sep = sep
  )

  writeInputFile(textlines, files["input"], verbose)

  runGompcalInDirectory(
    target.dir = gompitz.dir,
    input.file = files["input"],
    sep = sep,
    verbose = verbose,
    VERSION = VERSION
  )

  getCalibration(files["calib"], files["param"], digits = digits)
}

# writeInputFile ---------------------------------------------------------------

#' Write the Input File
#'
#' Write the given character vector into the input file at given path
#'
#' @param textlines vector of text lines to be written to \code{file}
#' @param file path to the file to be written
#' @param verbose integer number specifying the verbosity level. If this is a 
#'   positive value, debug messages are shown.
#' 
writeInputFile <- function(textlines, file, verbose = 1)
{
  kwb.utils::safePath(dirname(file))

  .catLogMessageIf(
    verbose > 0,
    sprintf("Writing input file %s... ", basename(file)),
    eol = FALSE
  )

  writeLines(textlines, file)    

  .catLogMessageIf(verbose > 0, "ok.")
}

# getCalibration ---------------------------------------------------------------

#' Get a full Calibration Object from File
#'
#' Get a full calibration object as returned by
#' \code{\link{runGompitzCalibration}}, but not by calling this function but by
#' just reading the file \code{calibr.txt}
#'
#' @param file_calib full path to calibration file \code{calibr.txt}
#' @param file_param full path to file \code{param.txt}
#' @param digits passed to \code{kwbGompitz:::compareEstimates}
#'
getCalibration <- function(file_calib, file_param = NULL, digits = 3)
{
  calibration <- readCalibration(kwb.utils::safePath(file_calib))

  if (! is.null(file_param)) {

    parameters <- readParameters(file = kwb.utils::safePath(file_param))

    # Write a copy of param_new.txt, generated from parameters, just to
    # be able to check if the read and write functions work fine
    file <- sub("param", "param_new", file_param)

    writeParameters(parameters, file = file, dbg = FALSE)

    # Compare the estimates
    compareEstimates(calibration, parameters, digits)

  } else {

    parameters <- NULL
  }

  # In each element, set the name of the stratum (in list element "stratum") and
  # add the class "gompitz_stratum_calib"
  for (stratum in names(calibration)) {
    calibration[[stratum]]$stratum <- stratum
    calibration[[stratum]] <- kwb.utils::addClass(
      calibration[[stratum]], "gompitz_stratum_calib"
    )
  }

  # Extend the class attribute and set the parameters as attribute
  structure(
    kwb.utils::addClass(calibration, "gompitz.calibration"),
    parameters = parameters
  )
}

# compareEstimates -------------------------------------------------------------

#' Compare Estimates in calib.txt and param.txt
#'
#' @param calibration list structure as returned by
#'   \code{kwbGompitz:::readCalibration}
#' @param parameters list structure as returned by
#'   \code{kwbGompitz:::readParameters}
#' @param digits round the estimates to this number of significant digits before
#'   comparing
#' @param warn if \code{TRUE} (the default is \code{FALSE}) a warning is given
#'   if the strata read from \code{calibr.txt} do not correspond to the strata
#'   read from \code{param.txt} (containing only the successfully calibrated
#'   strata!?).
#'
#' @return number of warnings that occurred
#'
compareEstimates <- function(calibration, parameters, digits, warn = FALSE)
{
  n.warnings <- 0

  names.calib <- names(calibration)
  names.param <- names(parameters$byStratum)

  if (warn && ! identical(names.calib, names.param)) {

    messageText <- sprintf(
      "The strata read from 'calibr.txt' (%s)\nand 'param.txt' (%s) differ!",
      kwb.utils::stringList(names.calib), kwb.utils::stringList(names.param)
    )

    warning_(messageText)
  }

  # If there is no calibration for a stratum in calibration there must not
  # be an entry in parameters
  converged <- checkConvergence(calibration, do.warn = warn)

  strata.converged <- names(which(converged))

  param.available <- (strata.converged %in% names.param)

  if (! all(param.available)) {

    stop_(
      "There are no parameters in 'param.txt' for these strata that ",
      "converged according to 'calibr.txt': ",
      kwb.utils::stringList(strata.converged[! param.available])
    )
  }

  #stratum <- strata.converged[1]
  for (stratum in strata.converged) {

    estim1 <- .getEstimatesFromCalibration(calibration, stratum)
    estim2 <- .getEstimatesFromParameters(parameters, stratum)

    if (.differs(x = estim1, y = estim2, digits)) {

      n.warnings <- n.warnings + 1

      warning_(
        sprintf("The estimates read from 'calibr.txt'\n(%s)\n",
                kwb.utils::collapsed(estim1)),
        sprintf("  differ from those read from 'param.txt'\n(%s)\n",
                kwb.utils::collapsed(estim2)),
        "for stratum ", kwb.utils::hsQuoteChr(stratum)
      )
    }
  }

  n.warnings
}

# checkConvergence -------------------------------------------------------------

#' Warn if Model did not converge for all Strata
#'
#' Warn if the model did not converge for all strata
#'
#' @param calibration calibration object as returned by
#'   \code{kwbGompitz:::readCalibration}
#' @param do.warn if \code{TRUE} warnings are shown for non-calibrated strata
#'
#' @return named logical vector with element names corresponding to the names of
#'   the elements of \code{calibration} that represent the different strata.
#'
#' @export
#'
checkConvergence <- function(calibration, do.warn = TRUE)
{
  converged <- .calibrationAvailable(calibration)

  if (do.warn && ! all(converged)) {

    warning_(
      "The model did not converge for these strata:\n",
      kwb.utils::stringList(names(which(! converged)))
    )
  }

  converged
}

# .getEstimatesFromCalibration -------------------------------------------------

.getEstimatesFromCalibration <- function(calibration, stratum)
{
  get <- kwb.utils::selectElements

  x <- get(get(calibration, stratum), "convergence")

  get(get(x, "estimates"), "Estimate")
}

# .getEstimatesFromParameters --------------------------------------------------

.getEstimatesFromParameters <- function(parameters, stratum)
{
  get <- kwb.utils::selectElements

  get(get(get(parameters, "byStratum"), stratum), "estimates")
}

# .differs ---------------------------------------------------------------------

.differs <- function(x, y, digits)
{
  z <- FALSE

  z <- z || (  is.null(x) && ! is.null(y))
  z <- z || (! is.null(x) &&   is.null(y))

  rounded <- lapply(list(x = x, y = y), signif, digits = digits)

  diffs <- abs(rounded$x - rounded$y)

  z || ! all(diffs < 1e-10)
}

#===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===
#
# Functions to retrieve information from a gompitz calibration
#
#===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===

# .getObservationByCondition ---------------------------------------------------

#' Number of observations
#'
#' Extract number of observations from calibration result
#'
#' @param calibration calibration result as retrieved by
#'   \code{\link{runGompitzCalibration}}
#'
.getObservationByCondition <- function(calibration)
{
  FUN <- function(x) {x$conditions$condition}

  dframe <- data.frame(
    condition = unique(as.character(unlist(sapply(calibration, FUN = FUN))))
  )

  for (i in seq_along(calibration)) {

    stratum.name <- names(calibration)[i]
    stratum <- calibration[[i]]

    if (!is.null(stratum$conditions)) {

      dframe2 <- data.frame(
        stratum$conditions$condition, stratum$conditions$number
      )

      names(dframe2) <- c("condition", stratum.name)
      dframe <- merge(dframe, dframe2, all.x = TRUE)

    } else {

      dframe <- cbind(dframe, NA)
      names(dframe)[ncol(dframe)] <- stratum.name
    }
  }

  return(dframe)
}

# .calibrationAvailable --------------------------------------------------------

#' Check for Convergence in Calibration
#'
#' @param calibration result of Gompitz calibration as provided by
#'   \code{\link{runGompitzCalibration}}
#' @param strata vector of strata for which convergence is checked
#' @param param if not NULL, this structure, representing the content of the
#'   "param.txt" file, is used instead of \code{calibration} to check for
#'   convergence
#'
#' @return named logical vector with as many elements as there are in
#'   \emph{strata} each of which indicates if convergence was achieved for the
#'   corresponding stratum
#'
.calibrationAvailable <- function(calibration, strata = NULL, param = NULL)
{
  if (is.null(strata)) {

    strata <- if (is.null(param)) names(calibration) else param$strata
  }

  get <- kwb.utils::selectElements

  sapply(strata, FUN = function(stratum) {

    info <- if (is.null(param)) {

      get(calibration, stratum)$convergence

    } else {

      get(param, "byStratum")[[stratum]]
    }

    ! is.null(info)
  })
}

# getCalibrationFile -----------------------------------------------------------

#' Source File of Calibration Result
#'
#' @param calib.result calibration result of Gompitz calibration as provided by
#'   \code{\link{runGompitzCalibration}}
#'
getCalibrationFile <- function(calib.result)
{
  attr(calib.result, "source")
}
