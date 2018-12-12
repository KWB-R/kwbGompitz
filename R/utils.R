# listElementToCsvLine ---------------------------------------------------------
listElementToCsvLine <- function(x, element, sep, first = element)
{
  toCsvLine(kwb.utils::selectElements(x, element), sep, first = first)
}

# stop_ ------------------------------------------------------------------------
stop_ <- function(...)
{
  stop(..., call. = FALSE)
}

# toCsvLine --------------------------------------------------------------------
toCsvLine <- function(x, sep, first = NULL)
{
  paste(c(first, x), collapse = sep)
}

# warning_ ---------------------------------------------------------------------
warning_ <- function(...)
{
  warning(..., call. = FALSE)
}
