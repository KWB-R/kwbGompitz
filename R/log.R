# .catLogMessageIf -------------------------------------------------------------
.catLogMessageIf <- function(condition, ...)
{
  if (condition) {
    .catLogMessage(...)
  }
}

# .catLogMessage ---------------------------------------------------------------
.catLogMessage <- function(message, eol = TRUE) 
{
  cat(.toLogMessage(message, eol))
}

# .toLogMessage ----------------------------------------------------------------
.toLogMessage <- function(message, eol = TRUE) 
{
  return(sprintf("=== %s%s", message, ifelse(eol, "\n", "")))
}
