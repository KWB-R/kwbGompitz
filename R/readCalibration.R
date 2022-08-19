# readCalibration --------------------------------------------------------------

#' Read Calibration Result
#' 
#' Read calibration result from given file
#' 
#' @param file path to gompcal result file "calibr.txt"
#' @param verbose integer number specifying the level of verbosity
#' @export
readCalibration <- function (file, verbose = 1)
{
  result.lines <- readLines(file, encoding = "UTF-8")
  block.starts <- grep("^\\*\\*\\*", result.lines)
  
  nblocks <- length(block.starts)
  block.starts <- c(block.starts, length(result.lines) + 1)
  
  result.list <- NULL
  
  #i <- 1
  for (i in seq_len(nblocks)) {
    
    block <- .getBlock(result.lines, block.starts, i)
    insp.block <- .getInspectionBlock(block)
    stratum <- .getStratum(result.lines[block.starts[i]])
    
    .catLogMessageIf(verbose > 1, paste("Stratum:", stratum))

    result.list[[stratum]] <- list(
      pipes = list(
        number = .getNumberAtStart(block[1]), 
        weight = .getTotalWeight(block[1])
      ), 
      inspections = list(
        number = .getNumberAtStart(insp.block[1]),
        weight = .getTotalWeight(insp.block[1])
      ),
      conditions = .getInspectionSummary(insp.block),
      convergence = .getConvergenceInfo(x = block)
    )
  }

  # Set path to result files attribute "source"
  structure(result.list, "source" = file)
}

# .getBlock --------------------------------------------------------------------
.getBlock <- function(x, starts, i)
{
  first.line <- starts[i] + 1
  last.line  <- starts[i+1] - 1
  x[first.line:last.line]  
}

# .getInspectionBlock ----------------------------------------------------------
.getInspectionBlock <- function(x)
{
  end.index <- grep("^$", x[-(1:2)])[1] + 1
  x[3:end.index]
}

# .getStratum ------------------------------------------------------------------
.getStratum <- function(x)
{
  gsub("^\\*\\*\\*\\s+Stratum\\s+", "", gsub("\\s+\\*\\*\\*$", "", x))
}

# .getNumberAtStart ------------------------------------------------------------
.getNumberAtStart <- function(x)
{
  as.numeric(strsplit(x, " ")[[1]][1])
}

# .getTotalWeight --------------------------------------------------------------
.getTotalWeight <- function(x)
{
  weight.pattern <- "^.*\\(total weight = (\\d+.\\d+)\\).*$"
  as.numeric(sub(weight.pattern, "\\1", x))
}

# .getInspectionSummary --------------------------------------------------------
.getInspectionSummary <- function(x)
{
  result.list <- NULL
  
  if (length(grep("^Warning", x[2])) == 0) {
    
    numbers <- c()
    weights <- c()
    conditions <- c()
    
    for (one.line in x[-1]) {
      conditions <- c(conditions, .getCondition(one.line))
      numbers <- c(numbers, .getNumberAtStart(one.line))
      weights <- c(weights, .getTotalWeight(one.line))
    }
    
    result.list <- data.frame(
      condition = conditions, 
      number = numbers, 
      weight = weights,
      stringsAsFactors = FALSE
    )
  }
  
  result.list
}

# .getCondition ----------------------------------------------------------------
.getCondition <- function(x)
{
  parts <- strsplit(x, " ")[[1]]
  parts[length(parts)]
}

# .getConvergenceInfo ----------------------------------------------------------
.getConvergenceInfo <- function(x)
{
  result <- NULL
  
  num.iterations <- .getNumIterations(x)
  
  if (length(num.iterations) > 0) {
    result <- list(
      num.iterations = num.iterations,
      log.likelihood = .getLogLikelihood(x),
      covariances = .getFinalCovarianceMatrix(x),
      estimates = .getParameterEstimates(x)
    )
  }
  
  result
}

# .getNumIterations ------------------------------------------------------------
.getNumIterations <- function(x)
{
  pattern <- "^Convergence achieved in (\\d+) iterations.*$"
  one.line <- grep(pattern, x, value = TRUE)
  
  if (length(one.line) > 0) {
    as.numeric(sub(pattern, "\\1", one.line))
  }
}

# .getLogLikelihood ------------------------------------------------------------
.getLogLikelihood <- function(x)
{
  pattern <- "^Log-Likelihood"
  one.line <- grep(pattern, x, value = TRUE)
  
  if (length(one.line) > 0) {
    as.numeric(strsplit(one.line, "\\s*=\\s*")[[1]][2])
  }
}

# .getFinalCovarianceMatrix ----------------------------------------------------
.getFinalCovarianceMatrix <- function(x)
{
  pattern <- "^Final Covariance Matrix"
  
  start.line <- grep(pattern, x) + 1
  
  if (length(start.line) > 0) {
    
    end.line <- grep("^$", x[-(1:start.line)])[1] + start.line - 1
    
    matrixlines <- x[start.line:end.line]
    matrixlines <- sub("^\\{\\s+", "", sub("\\s+\\}$", "", matrixlines))
    
    values.num <- .toNumericMatrix(
      values.list = strsplit(matrixlines, "\\s+,\\s+"), 
      subject = "Final Covariance Matrix"
    )
    
    values.num    
  }  
}

# .toNumericMatrix -------------------------------------------------------------
.toNumericMatrix <- function(values.list, subject)
{
  stopifnot(is.list(values.list))
  stopifnot(kwb.utils::allAreEqual(sapply(values.list, length)))
  
  values <- unlist(values.list)
  values.num <- suppressWarnings(as.numeric(values))
  
  invalid <- is.na(values.num)
  
  if (any(invalid)) {
    
    invalids <- kwb.utils::stringList(unique(values[invalid]))
    
    warning_(
      "The following values in the ", subject, " cannot be ",
      "converted to numeric:\n", invalids
    )
  }
  
  matrix(values.num, nrow = length(values.list), byrow = TRUE)
}

# .getParameterEstimates -------------------------------------------------------
#' Get Parameter Estimates
#'
#' @param x x
#'
#' @return ???
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom  kwb.utils hsTrim
.getParameterEstimates <- function(x)
{
  pattern <- "^Parameter estimates and Wald Chi2 tests"
  start.line <- grep(pattern, x) + 1
  
  if (length(start.line) > 0) {
    
    end.line <- grep("^\\-\\-\\-", x[-(1:start.line)])[2] + start.line
    
    frame.text <- x[start.line:end.line]
    frame.text <- frame.text[4:(length(frame.text)-1)]
    
    labels <- kwb.utils::hsTrim(substr(frame.text, 1, 46))
    
    values.text <- substr(frame.text, 48, nchar(frame.text))
    values.list <- strsplit(values.text, "\\s+")
    values.num <- .toNumericMatrix(
      values.list, subject = "Parameter Estimates Table"
    )
    
    # Get the column captions
    caption.line <- sub("Std\\. Error", "Std.Error", x[start.line + 1])
    captions <- strsplit(caption.line, "\\s+")[[1]][-1]
    
    # There may be less value columns than column captions!
    if (ncol(values.num) < length(captions)) {
      
      emptyColumn <- rep(NA, nrow(values.num))
      
      while (ncol(values.num) < length(captions)) {
        
        values.num <- cbind(values.num, emptyColumn)
      }
    }
    
    values <- as.data.frame(values.num)
    names(values) <- captions
    
    data.frame(Label = labels, values, stringsAsFactors = FALSE)
  }
}
