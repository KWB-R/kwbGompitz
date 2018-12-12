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
  .runModuleInDirectory(
    "gompred", target.dir, input.file, sep, ..., 
    options = as.character(strategy)
  )
}

# .runModuleInDirectory --------------------------------------------------------

.runModuleInDirectory <- function
(
  module, target.dir, input.file, sep, ..., verbose = 1, show.error = TRUE
)
{
  # We need a log folder within the target directory
  kwb.utils::createDirectory(file.path(target.dir, "log"), dbg = FALSE)

  errorCode <- kwb.utils::runInDirectory(
    target.dir = target.dir,
    FUN = .runModule,
    module = module,
    input.file = input.file,
    sep = sep,
    ...,
    verbose = verbose,
    .dbg = (verbose > 1)
  )

  if (show.error && errorCode != 0) {

    stderr <- attr(errorCode, "stderr")

    messageLines <- c(
      paste0("There was an error (errorCode: ", errorCode, ") given below. ",
             "Please inspect the input file (opened in your text editor?):"),
      input.file,
      "*****",
      readLines(kwb.utils::safePath(target.dir, stderr)),
      "*****"
    )

    if (grepl("^unix", getOperatingSystemType())) {
      
      kwb.utils::hsOpenWindowsExplorer(input.file)
    }

    stop_(kwb.utils::collapsed(messageLines, "\n"))
  }
}

# .runModule -------------------------------------------------------------------

.runModule <- function
(
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

  errorCode <- system2(command, args, stdout = files[1], stderr = files[2])

  .catLogMessageIf(verbose > 0, paste0("Back from ", module, "."))

  structure(errorCode, stdout = files[1], stderr = files[2])
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
