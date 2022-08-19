# exampleCalibration -----------------------------------------------------------

#' Calibration Object of the Gompitz Example
#' 
#' This function returns a calibration object (as returned by
#'   \code{\link{runGompitzCalibration}}), corresponding to the example provided
#'   with the Gompitz software package
#' 
#' @param VERSION one of "unix", "unix_home", "win32", "win32_kwb"
#' 
#' @export
#' 
exampleCalibration <- function(
  VERSION = getOperatingSystemType()
)
{
  files <- exampleFile(c("calibr.txt", "param.txt"), VERSION = VERSION)

  getCalibration(files["calibr.txt"], files["param.txt"])
}

# exampleFile ------------------------------------------------------------------

#' Get Path to Example File
#' 
#' Get the full path to one of the example files provided with the GompitZ
#'   software package of which copies are available in this R package.
#' 
#' @param filename name of the example file
#' @param \dots passed to \code{\link{getDefaultPaths}}
#' @param dbg if \code{TRUE}, debug messages are shown, else not.
#' 
#' @return full path to the example file
#' 
#' @export
#' @importFrom kwb.utils catIf safePath selectElements
#' @importFrom utils unzip
exampleFile <- function(filename = "none", ..., dbg = TRUE)
{
  # Call this function recursively if more than one file name is given
  if (length(filename) > 1) {

    return (sapply(filename, exampleFile, ..., dbg = dbg))
  }

  exampledir <- kwb.utils::selectElements(getDefaultPaths(...), "EXAMPLE.DIR")

  # If the folder does not exist, run different executables of gompcal and
  # gompred (as provided in subfolders "bin_<version>" with version in ["win32",
  # "win32_kwb", "unix_home"]) on the example input file (obs.txt) and store the
  # results in folders "example_<version>".

  if (! file.exists(exampledir)) {

    createExampleFiles()
  }

  # If the file does not exist, extract "example.zip"
  if (! file.exists(file.path(exampledir, filename))) {

    zip <- kwb.utils::safePath(exampledir, "example.zip")

    kwb.utils::catIf(dbg, "Extracting", basename(zip), "in", exampledir, "... ")
    utils::unzip(zip, exdir = exampledir)
    kwb.utils::catIf(dbg, "ok.\n")
  }

  kwb.utils::safePath(exampledir, filename)
}

# runGompcalExample ------------------------------------------------------------

#' Run Calibration Example
#' 
#' Run calibration example provided with GompitZ
#' 
runGompcalExample <- function()
{
  file <- kwb.utils::selectElements(getDefaultPaths(), "EXAMPLE.OBS.FILE")
  
  runGompcalInDirectory(input.file = file, sep = ";")
}

# getExampleStatusMatrix -------------------------------------------------------

#' Example Covariate Status Matrix
#' 
#' @seealso \code{\link{createStatusMatrix}}
#' @importFrom kwb.utils createMatrix 
getExampleStatusMatrix <- function()
{
  CONST <- getConstants()

  strata.labels <- c("Cast Iron", "Brick")
  covariate.labels <- c("DIAM", "TYPE", "LENGTH", "ROAD")

  M <- kwb.utils::createMatrix(
    rowNames = strata.labels,
    colNames = covariate.labels,
    value = CONST$COVAR_NOT_USED
  )

  M["Cast Iron", "DIAM"] <- CONST$COVAR_INFLUENCING_BOTH
  M["Cast Iron", "TYPE"] <- CONST$COVAR_INFLUENCING_DETERIOR_SPEED
  M["Cast Iron", "ROAD"] <- CONST$COVAR_INFLUENCING_INITIAL_STATE

  M["Brick", "DIAM"] <- CONST$COVAR_INFLUENCING_BOTH
  M["Brick", "TYPE"] <- CONST$COVAR_INFLUENCING_BOTH
  M["Brick", "ROAD"] <- CONST$COVAR_INFLUENCING_DETERIOR_SPEED

  M
}

# getConstants -----------------------------------------------------------------

#' Gompitz Covariate Status Constants
#' 
#' Gompitz constants regarding the status of covariates (not used, influencing
#'   the initial state, influencint the deterioration speed, influencing both)
#' 
#'  @export
#'   
getConstants <- function()
{
  return(list(
    COVAR_NOT_USED = 0,
    COVAR_INFLUENCING_INITIAL_STATE = 1,
    COVAR_INFLUENCING_DETERIOR_SPEED = 2,
    COVAR_INFLUENCING_BOTH = 3
  ))
}

# getSampleObservations --------------------------------------------------------

#' Random Observation Data
#' 
#' @param n number of rows to be generated
#' 
#' @return data frame with columns MATERIAL, Baujahr, DN, Pipe, Note,
#'   Inspection, PipeLength, before1970
#' 
getSampleObservations <- function(n = 100)
{
  this.date <- as.character(Sys.Date())
  this.year <- as.integer(substr(this.date, 1, 4))
  years <- seq(1900, this.year)
  insp.years <- seq(1990, this.year)

  insp.year <- as.POSIXct(sprintf(
    "%4d-%02d-%02d 00:00:00",
    .sampleWithDuplicates(insp.years, n),
    .sampleWithDuplicates(1:12, n),
    .sampleWithDuplicates(1:28, n)
  ))

  mark <- .sampleWithDuplicates(c(NA, paste0("C", 1:4)), n)
  insp.year[is.na(mark)] <- NA
  baujahr <- .sampleWithDuplicates(years, n)

  data.frame(
    MATERIAL = .sampleWithDuplicates(c("A", "B", "C"), n),
    Baujahr = baujahr,
    DN = .sampleWithDuplicates(c(150, 200, 250, 300, 350, 400, 500), n),
    Pipe = paste0("pipe", 1:n),
    Note = mark,
    Inspection = insp.year,
    PipeLength = .sampleWithDuplicates(seq(10, 200), n),
    before1970 = (baujahr < 1970)
  )
}

# .sampleWithDuplicates --------------------------------------------------------

.sampleWithDuplicates <- function(x, n)
{
  sample(x, n, replace = TRUE)
}
