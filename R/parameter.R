# copyParameters ---------------------------------------------------------------

#' Copy Model Parameters from one Stratum to another
#'
#' @param calibration calibration object as returned by
#'   \code{\link{runGompitzCalibration}}
#' @param from name of stratum to copy parameters from
#' @param to vector of names of strata to which parameters are to be
#'   copied to. If not given, the parameters of \code{from} are copied
#'   to all strata for which model parameters did not converge.
#' @param dbg if \code{TRUE} (default) debug messages are shown
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get an example calibration
#' calibration <- kwbGompitz::exampleCalibration()
#'
#' # Check for which strata the model parameters converged
#' checkConvergence(calibration, do.warn = FALSE)
#'
#' # Copy parameters from a stratum for which the model parameters converged
#' # to a stratum for which the model parameters did not converge
#' calibration <- copyParameters(calibration, from = "Cast Iron", to = "Concrete")
#'
#' # Check again
#' checkConvergence(calibration, do.warn = FALSE)
#'
#' # The following gives a warning (no convergence for source stratum) and returns
#' # the calibration unchanged
#' calibration <- copyParameters(calibration, "Clay", "Brick")
#'
#' # The following gives a warning (differing distinct condition classes) and
#' # returns the calibration unchanged
#' calibration <- copyParameters(calibration, "Cast Iron", "Brick")
#' }
#'
copyParameters <- function(
  calibration, from = NULL, to = NULL, dbg = TRUE
)
{
  getele <- kwb.utils::selectElements

  converged <- checkConvergence(calibration, do.warn = FALSE)

  # If no target strata are given, use all strata for which parameters did
  # not converge
  if (is.null(to)) {

    to <- names(converged)[! converged]

    if (length(to) == 0) {

      warning("There are no strata for which the model did not converge!")

      return (calibration)
    }
  }

  # If more than one target stratum is given call this function recursively
  # for each of the target strata
  if (length(to) > 1) {

    for (stratum in to) {

      calibration <- copyParameters(calibration, from, stratum, dbg)
    }

    return (calibration)
  }

  # Stop if the (given) stratum does not exist
  .check_stratum(to, names(calibration))

  # If no source stratum is given, use the first stratum in calibration for
  # which the model parameters converged and which has the same properties
  # (the same set of distinct observed conditions)

  if (is.null(from)) {

    if (! any(converged)) {

      warning("No converged parameters available at all!")

      return (calibration)
    }

    from <- find_matching(
      calibration, to, candidates = names(converged)[converged]
    )

    if (length(from) == 0) {

      warning(
        "No stratum available from which I could copy the parameters to\n",
        "stratum '", to, "' (not all condition classes available).\n",
        "You could try to reduce the number of condition classes."
      )

      return (calibration)

    } else {

      from <- from[1]
    }
  }

  # Stop if the (given) stratum does not exist
  .check_stratum(from, names(calibration))

  if (from == to) {

    warning("from is identical to to -> Nothing to do!")

    return (calibration)
  }

  if (! converged[from]) {

    warning(
      sprintf("The model did not converge for stratum '%s'. ", from),
      "I cannot copy the parameters of this stratum!"
    )

    return (calibration)
  }

  # 1. Copy the parameters
  parameters <- kwb.utils::getAttribute(calibration, "parameters")

  x <- parameters$byStratum[[from]]

  y <- kwb.utils::defaultIfNULL(parameters$byStratum[[to]], x)
  y$estimates <- x$estimates

  parameters$byStratum[[to]] <- y

  attr(calibration, "parameters") <- parameters

  # 2. Set the parameters in the "calibration" object
  x <- calibration[[from]]$convergence

  calibration[[to]]$convergence <- structure(x, meta = paste(
    "Copied from", from
  ))

  # Create and show debug message or warning
  status <- ifelse(converged[to], "existing ", "")

  message_text <- paste0(
    sprintf("The %smodel parameters for stratum '%s' were overwritten\n",
            status, to),
    sprintf(" with the parameters for stratum '%s'\n", from)
  )

  if (converged[to]) {

    warning_(message_text)

  } else {

    kwb.utils::catIf(dbg, message_text)
  }

  calibration
}

# find_matching ----------------------------------------------------------------
find_matching <- function(calibration, to_stratum, candidates)
{
  get_conditions <- function(x) {
    kwb.utils::selectElements(calibration, x)$conditions$condition
  }

  param <- kwb.utils::getAttribute(calibration, "parameters")

  all_conditions <- param$conditions

  same_conditions <- sapply(candidates, function(candidate) {
    identical(get_conditions(candidate), all_conditions)
  })

  candidates[same_conditions]
}

# .check_stratum ---------------------------------------------------------------

.check_stratum <- function(stratum, strata) {

  if (! stratum %in% strata) {
    
    stop_(sprintf(
      "There is no such stratum: '%s'. Available strata: %s\n",
      stratum, kwb.utils::stringList(strata)
    ))
  }
}

# getCalibrationParameters -----------------------------------------------------

#' Get parameters from Calibration Object
#'
#' Get the calibration parameters as they are required to generate the
#'   param.txt file for the gompred program, from the calibration object
#'
#' @param calibration calibration object as returned by
#'   \code{\link{runGompitzCalibration}}
#' @param remove_non_calibrated if \code{TRUE} (default) parameters for
#'   non-calibrated strata are removed
#'
getCalibrationParameters <- function(
  calibration = exampleCalibration(), remove_non_calibrated = TRUE
)
{
  parameters <- kwb.utils::getAttribute(calibration, "parameters")

  # If there are strata for which no convergence was achieved (as documented in
  # the calibration object), clear the parameters for these strata in the
  # parameters object as well!
  #.calibrationAvailable <- kwbGompitz:::.calibrationAvailable
  calibration_ok <- .calibrationAvailable(calibration)
  
  strata_ok <- names(calibration)[calibration_ok]

  by_stratum <- kwb.utils::selectElements(parameters, "byStratum")

  strata_to_remove <- setdiff(names(by_stratum), strata_ok)

  if (remove_non_calibrated && length(strata_to_remove)) {

    parameters$byStratum <- kwb.utils::selectElements(by_stratum, strata_ok)

    warning(sprintf(
      "Parameters for non-calibrated strata (%s) were removed!",
      kwb.utils::stringList(strata_to_remove)
    ))
  }

  parameters
}

# readParameters ---------------------------------------------------------------

#' Read Parameters from "param.txt"
#'
#' Read parameters from param.txt into list structure
#'
#' @param file full path to parameter file "param.txt", generated by gompcal.exe
#' @param sep column separator, default: ";"
#' @param dbg if \code{TRUE} (default) debug messages are shown
#' @return list with sections \code{conditions}, /code{strata},
#'   /code{covariates}, /code{categoryLevels}, /code{byStratum},
#'
readParameters <- function(file, sep = ";", dbg = FALSE)
{
  kwb.utils::catIf(dbg, sprintf(
    "Reading parameters from '%s'... ", basename(file)
  ))

  textlines <- readLines(kwb.utils::safePath(file), encoding = "UTF-8")

  header <- lapply(
    X = c(conditions = 1, strata = 2, covariates = 3),
    FUN = function(row) strsplit(textlines[row], sep)[[1]]
  )

  # For each covariate prefixed with underscore a further line is expected
  categoricals <- grep("^_", header$covariates, value = TRUE)

  header$categoryLevels <- list()

  row <- 3

  for (categorical in categoricals) {

    row <- row + 1

    fields <- strsplit(textlines[row], sep)[[1]]

    if (fields[1] != categorical) {

      if (! grepl("^unix", getOperatingSystemType())) {
        kwb.utils::hsOpenWindowsExplorer(file)
      }

      stop(sprintf(
        paste("The first field in row %d is not '%s' as expected",
              "but '%s' in '%s'"),
        row, categorical, fields[1], file
      ))
    }

    header$categoryLevels[[categorical]] <- fields[-1]
  }

  resttext <- textlines[- seq_len(row)]

  if (length(resttext) > 0) {

    fields <- strsplit(resttext, sep)

    strata <- sapply(fields, "[", 1)

    unique.strata <- unique(strata)

    # All strata must be mentioned in the header
    stopifnot(all(unique.strata %in% header$strata))

    # All strata must use exactly three rows
    stopifnot(all(table(strata) == 3))

    # Read condition classes, stati and parameter estimates for each stratum
    header$byStratum <- lapply(
      X = structure(unique.strata, names = unique.strata),
      FUN = function(stratum) {
        fields.stratum <- fields[strata == stratum]
        lapply(
          X = c(conditions = 1, stati = 2, estimates = 3),
          FUN = function(i) {
            fields.stratum[[i]][-1]
          })
      })

    # Convert types
    for (stratum in names(header$byStratum)) {
      header$byStratum[[stratum]]$stati <- as.integer(header$byStratum[[stratum]]$stati)
      header$byStratum[[stratum]]$estimates <- as.numeric(header$byStratum[[stratum]]$estimates)
    }

  } else {

    header$byStratum <- list()
  }

  kwb.utils::catIf(dbg, "ok.\n")

  header
}

# writeParameters --------------------------------------------------------------

#' Write Calibration Parameters to File
#'
#' @param parameters list structure containing calibration parameters as
#'   provided by \code{kwbGompitz:::readParameters}
#' @param file full path to file to which parameters are to be written
#' @param sep column separator. Default: ";"
#' @param dbg if \code{TRUE} (default) debug messages are shown
#' @param warn if \code{TRUE} a message is shown that the existing parameter
#'   file was overwritten
#'
writeParameters <- function(
  parameters, file, sep = ";", dbg = TRUE, warn = FALSE
)
{
  if (warn && file.exists(file)) {
    
    message(sprintf("Existing '%s' is overwritten!", basename(file)))
  }

  kwb.utils::catAndRun(dbg = dbg, sprintf("Writing parameters to '%s'", file), {
    writeLines(parameterLines(parameters, sep = sep), con = file)
  })
}

# parameterLines ---------------------------------------------------------------

#' Generate text lines for param.txt
#'
#' Convert the list structure containing calibration parameters as retrieved by
#' \code{kwbGompitz:::readParameters} to a vector of character representing the
#' text lines to be written to file
#'
#' @param parameters list structure containing calibration parameters as
#'   provided by \code{kwbGompitz:::readParameters}
#' @param sep column separator. Default: ";"
#'
#' @return vector of character representing the lines of param.txt, containing
#'   the calibration parameters as generated by /code{gompcal}
#'
parameterLines <- function(parameters, sep = ";")
{
  c(
    parameterLines_header(parameters, sep),
    parameterLines_category(parameters, sep), # may be NULL
    parameterLines_stratum(parameters, sep)
  )
}

# parameterLines_header --------------------------------------------------------
parameterLines_header <- function(parameters, sep)
{
  call_with <- kwb.utils::callWith
  
  # Define common arguments to listElementToCsvLine()
  arguments <- list(x = parameters, sep = sep, first = NULL)

  c(
    call_with(listElementToCsvLine, arguments, element = "conditions"),
    call_with(listElementToCsvLine, arguments, element = "strata"),
    call_with(listElementToCsvLine, arguments, element = "covariates")
  )
}

# parameterLines_category ------------------------------------------------------
parameterLines_category <- function(parameters, sep)
{
  categories <- kwb.utils::selectElements(parameters, "categoryLevels")

  if (length(categories) == 0) {
    
    return(NULL)
  }
  
  vapply(
    X = names(categories), 
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    FUN = listElementToCsvLine, 
    x = categories, 
    sep = sep
  )
}

# parameterLines_stratum -------------------------------------------------------
parameterLines_stratum <- function(parameters, sep)
{
  get <- kwb.utils::selectElements
  call_with <- kwb.utils::callWith
  
  byStratum <- get(parameters, "byStratum")

  result <- vapply(
    X = names(byStratum), 
    FUN.VALUE = character(3), 
    FUN = function(stratum) {
      
      # Provide the information for this stratum
      stratum_info <- byStratum[[stratum]]
      
      # Format the estimates
      stratum_info$estimates <- sprintf("%0.6e", get(stratum_info, "estimates"))
      
      # Define common arguments to listElementToCsvLine()
      arguments <- list(x = stratum_info, sep = sep, first = stratum)
      
      c(
        call_with(listElementToCsvLine, arguments, element = "conditions"),
        call_with(listElementToCsvLine, arguments, element = "stati"),
        call_with(listElementToCsvLine, arguments, element = "estimates")
      )
    })
  
  unlist(result)
}
