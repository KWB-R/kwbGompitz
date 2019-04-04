# runGompitzPrediction ---------------------------------------------------------

#' Run Gompitz Predicion
#' 
#' @param input.data prepared input data as retrieved by
#'   \code{\link{composeGompitzInputData}}
#' @param subset indexes of rows in \code{input.data} to be used for prediction.
#'   if \code{NULL}, all rows in \code{input.data} will be used.
#' @param calibration result of Gompitz calibration as retrieved by 
#'   \code{\link{runGompitzCalibration}}
#' @param strategy strategy identifier. Must be one of 0 (do nothing), 1
#'   (length-driven strategy), 2 (budget-driven strategy), or 3 (optimised
#'   strategy).")
#' @param \dots arguments passed to the corresponding
#'   \code{.fileContentStrategy} functions, such as \code{range.years}
#'   (two-element vector with first and last year of prediction),
#'   rehabilitation.costs (needed for strategy = 1 or 2 or 3, see
#'   \code{kwbGompitz:::.fileContentStrategy1} or 
#'   \code{kwb.gomiptz:::.fileContentStrategy2} or 
#'   \code{kwb.gomiptz:::.fileContentStrategy3}), annual.total.length (needed 
#'   for strategy = 1, see \code{kwbGompitz:::.fileContentStrategy1}), 
#'   annual.total.budget (needed for strategy = 2, see 
#'   \code{kwb.gomiptz:::.fileContentStrategy2}), max.tol.prop.of.length (needed
#'   for strategy = 3, see \code{kwb.gomiptz:::.fileContentStrategy3}), 
#'   target.year (needed for strategy = 3, see 
#'   \code{kwb.gomiptz:::.fileContentStrategy3})
#' @param verbose verbosity level, default: 1
#' @param do.stop if \code{TRUE} the program stops in case of inconsistencies
#' @param clear.observations if \code{TRUE} (default) the columns containing the
#'   inspection year and the observed condition class, respectively, are cleared
#'   in the input file given to \code{gompred}. Otherwise the observed condition
#'   classes are kept in the input file and thus considered by \code{gompred}.
#' @param VERSION name of subdirectory in package containing the binary files to
#'   be executed. Possible values: "unix", "win32", "win32_kwb"
#' @param use.data.table if \code{TRUE}, \code{\link[data.table]{fread}} is used
#'   to read the result file
#' @export
#' @examples
#' # For an example, see the Tutorial vignette "How to Use the Package"
#' 
runGompitzPrediction <- function(
  input.data, subset = NULL, calibration, strategy = 0, ..., verbose = 1, 
  do.stop = TRUE, clear.observations = TRUE, 
  VERSION = getOperatingSystemType(), 
  use.data.table = getOption("kwbGompitz.use.data.table", FALSE)
)
{
  #kwb.utils::assignPackageObjects("kwbGompitz")
  #kwb.utils::assignArgumentDefaults(kwbGompitz::runGompitzPrediction)
  #input.data <- kwb.fakin:::restore("input.data")
  #file <- system.file("extdata", "calibration_start_0_097_002_001.RData", package = "sema.berlin")
  #calibration <- kwb.utils::loadObject(file, "calibration")
  
  sep <- ";"

  target_dir <- dirname(getCalibrationFile(calibration))

  if (! file.exists(target_dir)) {
    
    target_dir <- tempGompitzDir()
  }
  
  paths <- getDefaultPaths(target.dir = target_dir, strategy = strategy)

  n_data <- nrow(input.data$masterdata)

  # If subset is given, filter all elements in input.data  
  if (! is.null(subset)) {

    n_subset <- length(subset)
    
    if (do.stop && n_subset > n_data) {
      
      stop_(
        "In runGompitzPrediction(): subset contains more elements (", n_subset,
        ") than are provided in input.data (", n_data, ")!"
      )
    }
    
    n_invalid <- sum(! subset %in% seq_len(n_data))
    
    if (do.stop && n_invalid > 0) {
      
      stop_(
        "In runGompitzPrediction(): There are ", n_invalid, " indices in subset ",
        "that are out of the range of rows in input.data (1:", n_data, ")!"
      )
    }
        
    # Filter for the subset
    input.data$masterdata <- input.data$masterdata[subset, ]
    input.data$covariates <- input.data$covariates[subset, , drop = FALSE]
    input.data$weight <- input.data$weight[subset]
  }

  # Indicate that sewers haven't been inspected if the actual observations are
  # not to be considered
  if (clear.observations) {

    input.data$masterdata <- clearInspectionColumns(input.data$masterdata)
  }

  kwb.utils::catIf(verbose > 0, sprintf(
    "Observed condition classes %s considered.\n", 
    ifelse(clear.observations, "are NOT", "ARE")
  ))

  # Write the input file
  time_write <- system.time(if (identical(use.data.table, 2L)) {

    kwb.utils::catAndRun(
      dbg = verbose, paste("Writing input file", paths$input.file),
      getFileContentForInputFile(
        masterdata = input.data$masterdata,
        covariates = input.data$covariates,
        weight = input.data$weight,
        covariates.status = input.data$covariates.status,
        condition.labels = input.data$condition.labels,
        sep = sep,
        file = paths$input.file
      ))
    
  } else {
    
    writeInputFile(
      textlines = getFileContentForInputFile(
        masterdata = input.data$masterdata,
        covariates = input.data$covariates,
        weight = input.data$weight,
        covariates.status = input.data$covariates.status,
        condition.labels = input.data$condition.labels,
        sep = sep
      ),
      file = paths$input.file,
      verbose = verbose
    )
  })
  
  # Write the strategy file
  content <- strategyFileContent(strategy, input.data = input.data, ...)

  writeLines(text = content, con = paths$strategy.file, sep = "")

  # Get the calibration parameters from the calibration object
  parameters <- getCalibrationParameters(calibration)
  
  # Write the parameter file param.txt containing the calibration parameters
  writeParameters(parameters, paths$parameter.file, dbg = FALSE)

  # Run gompred
  time_run <- system.time(runGompredInDirectory(
    target.dir = target_dir,
    input.file = paths$input.file,
    sep = sep,
    strategy = strategy,
    verbose = verbose,
    VERSION = VERSION
  ))

  # Read the result file
  time_read <- system.time(prediction <- if (use.data.table) {
    
    readPredictionFileWithDataTable(file = paths$prediction.file)
    
  } else {
    
    readPredictionFile(file = paths$prediction.file)
  })

  # Provide the stratum labels (as a factor in column "stratum")
  strata.labels <- getStataLabels(paths$input.file, sep)
  
  prediction <- appendStratumNumberColumn(prediction, strata.labels)

  runtime_data <- as.data.frame(rbind(time_write, time_run, time_read))
  
  runtime_data <- kwb.utils::resetRowNames(kwb.utils::setColumns(
    runtime_data, 
    use_data_table = use.data.table, 
    n_years = diff(list(...)$range.years) + 1,
    n_rows_in = nrow(input.data$masterdata),
    n_rows_out = nrow(prediction),
    action = c("write", "run", "read"),
    dbg = FALSE
  ))
  
  attr(prediction, "runtime") <- runtime_data
  
  # Extend the class attribute
  kwb.utils::addClass(prediction, "gompitz.prediction")
}

# clearInspectionColumns -------------------------------------------------------
clearInspectionColumns <- function(masterdata)
{
  masterdata <- kwb.utils::setColumns(masterdata, inspyear = "", dbg = FALSE)
  
  masterdata <- kwb.utils::setColumns(masterdata, condition = "", dbg = FALSE)
  
  masterdata
}

# getStataLabels ---------------------------------------------------------------

#' Read Strata Labels from Observation File
#' 
#' @param input.file path to input fule
#' @param sep column separator
#' 
getStataLabels <- function(input.file, sep)
{
  strsplit(readLines(input.file, 2)[2], sep)[[1]]
}

# appendStratumNumberColumn ----------------------------------------------------
appendStratumNumberColumn <- function(prediction, strata.labels)
{
  strata <- factor(
    kwb.utils::selectColumns(prediction, "stratumNo"),
    levels = seq_along(strata.labels) - 1,
    labels = strata.labels
  )

  cbind(stratum = strata, prediction)
}
