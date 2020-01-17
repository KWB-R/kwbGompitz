# runGompcalInDirectory --------------------------------------------------------

#' Run gompcal.exe in given Directory
#' 
#' @param target.dir path to target directory
#' @param input.file path to input file
#' @param sep column separator
#' @param \dots arguments that are passed to .runModuleInDirectory, such as
#'   \code{verbose} or \code{show.error}
#' 
runGompcalInDirectory <- function(
  target.dir = tempGompitzDir(), input.file = exampleFile("obs.txt"),
  sep = ";", ...
)
{
  .runModuleInDirectory(module = "gompcal", target.dir, input.file, sep, ...)
}

# runGompredInDirectory --------------------------------------------------------

#' Run gompred.exe in given Directory
#' 
#' @param target.dir path to target directory
#' @param input.file path to input file
#' @param sep column separator
#' @param strategy integer number specifying the strategy to be applied
#' @param \dots arguments that are passed to .runModuleInDirectory, such as
#'   \code{verbose} or \code{show.error}
#' 
runGompredInDirectory <- function(
  target.dir = tempdir(), input.file = exampleFile("obs.txt"), sep = ";",
  strategy = 0, ...
)
{
  error_code <- .runModuleInDirectory(
    "gompred", target.dir, input.file, sep, ..., 
    options = as.character(strategy)
  )

  # Check the output for an error message  
  filename <- kwb.utils::getAttribute(error_code, "stdout")
  
  file_stdout <- kwb.utils::safePath(target.dir, filename)
  
  lines_stdout <- readLines(file_stdout)
  
  # "No valid data rows - Gompred stops processing"
  if (length(grep("Gompred stops processing", lines_stdout))) {
    
    log_dir <- dirname(file_stdout)
    
    if (grepl("win", getOperatingSystemType())) {
      log_dir <- kwb.utils::windowsPath(log_dir)
    } 
    
    msg_stopped <- "Gompred stopped processing!"
    
    message(sprintf(
      "\n\n%s See '%s' in '%s':\n", msg_stopped, basename(file_stdout), log_dir
    ))
    
    writeLines(lines_stdout)
    
    stop_(msg_stopped, " See the output above for possible reasons.")
  }
}

# .runModuleInDirectory --------------------------------------------------------

.runModuleInDirectory <- function
(
  module, target.dir, input.file, sep, ..., verbose = 1, show.error = TRUE
)
{
  # We need a log folder within the target directory
  kwb.utils::createDirectory(file.path(target.dir, "log"), dbg = FALSE)

  error_code <- kwb.utils::runInDirectory(
    target.dir = target.dir,
    FUN = .runModule,
    module = module,
    input.file = input.file,
    sep = sep,
    ...,
    verbose = verbose,
    .dbg = (verbose > 1)
  )

  if (show.error && error_code != 0) {

    stderr <- kwb.utils::getAttribute(error_code, "stderr")
    
    messageLines <- c(
      paste0("There was an error (error_code: ", error_code, ") given below. ",
             "Please inspect the input file (opened in your text editor?):"),
      input.file,
      "*****",
      readLines(kwb.utils::safePath(target.dir, stderr)),
      "*****"
    )

    open_file_if_not_on_unix(file = input.file)

    stop_(kwb.utils::collapsed(messageLines, "\n"))
  }

  # Return error code having files containing stdout and stderr as attributes 
  error_code
}

# open_file_if_not_on_unix -----------------------------------------------------
open_file_if_not_on_unix <- function(file)
{
  if (! grepl("^unix", getOperatingSystemType())) {
    
    kwb.utils::hsOpenWindowsExplorer(file)
  }
}

# .runModule -------------------------------------------------------------------

.runModule <- function (
  module, input.file, sep, verbose = 1, options = NULL, qchar = "'", ...
)
{
  .catLogMessageIf(verbose > 0, paste("Running", module, "..."))

  paths <- getDefaultPaths(...)
  
  command <- kwb.utils::selectElements(paths, paste0(toupper(module), ".EXE"))

  set_mode_executable(command, dbg = FALSE)

  args <- c(
    kwb.utils::hsQuoteChr(kwb.utils::safePath(input.file), '"'),
    kwb.utils::hsQuoteChr(sep, '"'),
    options
  )

  files <- file.path("log", sprintf("%s_%s.txt", module, c("out", "err")))

  .catLogMessageIf(verbose > 1, sprintf("command: '%s'\n", command))
  kwb.utils::printIf(verbose > 1, args)

  error_code <- system2(command, args, stdout = files[1], stderr = files[2])

  .catLogMessageIf(verbose > 0, paste0("Back from ", module, "."))

  structure(error_code, stdout = files[1], stderr = files[2])
}

# set_mode_executable ----------------------------------------------------------

set_mode_executable <- function(file, mask = as.octmode("111"), dbg = TRUE)
{
  kwb.utils::catIf(dbg, sprintf(
    "Setting file properties of '%s' to executable ... ", basename(file)
  ))

  # Save the riginal file mode
  mode <- as.octmode(file.info(kwb.utils::safePath(file))$mode)

  # Change the file mode
  Sys.chmod(file, mode = mode | mask)

  # Check the new file mode
  if ((as.octmode(file.info(file)$mode) & mask) != mask) {

    stop_("file properties could not be set to executable! file: ", file)
  }

  kwb.utils::catIf(dbg, "ok.\n")

  # Return the old mode
  mode
}
