# getDefaultPaths --------------------------------------------------------------

#' Default Paths to GompitZ Files
#' 
#' Default paths to GompitZ executable and example files
#' 
#' @param VERSION one of "unix", "win32", "win32_kwb"
#' @param \dots arguments passed to resolve
#' 
getDefaultPaths <- function(VERSION = getOperatingSystemType(), ...)
{
  # Get a path dictionary (list of "keyword = path" assignments with
  # path possibly containing placeholders in angle brackets <>. Use
  # kwb.utils::resolve to resolve the placeholders and to receive
  # a list of full paths
  extdata <- system.file("extdata", package = "kwbGompitz")
  file <- kwb.utils::safePath(extdata, "pathDictionary.txt")
  dictionary <- kwb.utils::readDictionary(file)

  kwb.utils::resolve(
    dictionary,
    extdata = extdata,
    EXTENSION = ifelse(kwb.utils::.OStype() == "unix", "", ".exe"),
    VERSION = VERSION,
    ...
  )
}

# getOperatingSystemType -------------------------------------------------------
getOperatingSystemType <- function()
{
  if (kwb.utils::.OStype() == "unix") {
    
    ifelse(grepl("64", Sys.getenv("R_PLATFORM")), "unix64", "unix")
    
  } else {
    
    "win32"
  }
}

# tempGompitzDir ---------------------------------------------------------------

#' Create temporary gompitz directory
#'
#' @param verbose integer value determining the level of verbosity
#'  
tempGompitzDir <- function(verbose = 1)
{
  directory <- file.path(tempdir(), "gompitz")

  logdir <- file.path(directory, "log")

  folder.name <- "Temporary gompitz folder"

  if (file.exists(directory)) {

    .catLogMessageIf(verbose > 1, paste(folder.name, "already exists."))
    
  } else {
    
    if (! dir.create(directory)) {
      
      stop_(.toLogMessage(paste(folder.name, "could not be created.")))
    }

    .catLogMessageIf(verbose > 1, paste(folder.name, "created."))
  }

  # Create a subfolder "log"
  if (! file.exists(logdir) && ! dir.create(logdir)) {
    
    stop_(.toLogMessage(paste(
      "log subdirectory could not be created in", directory
    )))
  }

  kwb.utils::rStylePath(directory)
}
