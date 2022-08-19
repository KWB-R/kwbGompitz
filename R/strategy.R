# strategyFileContent ----------------------------------------------------------

#' Content for Strategy File
#' 
#' Create the text content for a gompred strategy file
#' 
#' @param strategy strategy identifier. Must be one of 0 (do nothing), 1
#'   (length-driven strategy), 2 (budget-driven strategy), or 3 (optimised
#'   strategy).")
#' @param \dots arguments passed to the corresponding
#'   \code{.fileContentStrategy} functions, see hidden functions 
#'   \code{kwbGompitz:::.fileContentStrategy0}, 
#'   \code{kwbGompitz:::.fileContentStrategy1}, 
#'   \code{kwbGompitz:::.fileContentStrategy2}, 
#'   \code{kwbGompitz:::.fileContentStrategy3}
#'   
#' @return character vector of length one representing the file content of the 
#'   strategy file
#' @importFrom kwb.utils selectElements
strategyFileContent <- function(strategy, ...)
{
  get <- kwb.utils::selectElements
  
  args <- list(...)

  if (strategy %in% c(0, 3)) {
    conditions <- get(args, "input.data")$condition.labels
    range.years <- get(args, "range.years")
  }

  if (strategy %in% 1:3) {
    costs <- get(args, "rehabilitation.costs")
  }

  if (strategy == 0) {
    file.content <- .fileContentStrategy0(range.years, conditions)
  }
  else if (strategy %in% 1:2) {

    file.content <- if (strategy == 1) {
      .fileContentStrategy1(costs, get(args, "annual.total.length"))
    } else {
      .fileContentStrategy2(costs, get(args, "annual.total.budget"))
    }
  }
  else if (strategy == 3) {

    file.content <- .fileContentStrategy3(
      condition.labels = conditions,
      rehabilitation.costs = costs,
      max.tol.prop.of.length = get(args, "max.tol.prop.of.length"),
      range.years = range.years,
      target.year = get(args, "target.year")
    )
    
  } else {
    
    stop_(
      "Invalid strategy identifier. Must be one of 0 (do nothing), ",
      "1 (length-driven strategy), 2 (budget-driven strategy), or ",
      "3 (optimised strategy)."
    )
  }

  file.content
  ### character vector of length one representing the file content of the
  ### strategy file
}

# .fileContentStrategy0 --------------------------------------------------------

#' Content for Strategy File 0
#' 
#' @param range.years vector of two integer numbers: first and last year of
#'   simulation
#' @param condition.labels vector of condition labels
#' @importFrom kwb.utils hsTrim defaultIfNA stringList
.fileContentStrategy0 <- function(range.years, condition.labels)
{
  if (! is.numeric(range.years) || length(range.years) != 2) {
    
    stop_(
      ".fileContentStrategy0(): range.years must be a vector of two numbers"
    )
  }

  if (! is.character(condition.labels)) {

    condition.labels <- as.character(condition.labels)
  }

  labels <- kwb.utils::hsTrim(kwb.utils::defaultIfNA(condition.labels, ""))

  if (any(labels == "")) {
    
    stop_(
      ".fileContentStrategy0(): invalid condition.labels: ",
      kwb.utils::stringList(condition.labels)
    )
  }

  sprintf(
    "%d\n%d\n%s\n",
    range.years[1],
    range.years[2],
    paste(condition.labels, collapse = ";")
  )
}

# .fileContentStrategy1 --------------------------------------------------------

#' Content for Strategy File 1
#' 
#' @param rehabilitation.costs list of rehabilitation costs per condition. The
#'   names of the list elements are the condition labels and the values of the
#'   list elements are the corresponding rehabilitation costs. Example: list(C4
#'   = 200, C3 = 300, C2 = 400, C1 = 500)
#' @param annual.total.length list of annual total lengths to be rehabilitated.
#'   The names of the list elements are the year numbers and the values of the
#'   list elements are the lenghts to be rehabilitated in the corresponding
#'   year. Example: list("2005" = 100, "2006" = 110, "2007" = 120)
#' 
.fileContentStrategy1 <- function(rehabilitation.costs, annual.total.length)
{
  return(.fileContentStrategy1or2(rehabilitation.costs, annual.total.length))
}

# .fileContentStrategy2 --------------------------------------------------------

#' Content for Strategy File 2
#' 
#' @param rehabilitation.costs list of rehabilitation costs per condition. The
#'   names of the list elements are the condition labels and the values of the
#'   list elements are the corresponding rehabilitation costs. Example:
#'   list(C4=200, C3=300, C2=400, C1=500)
#' @param annual.total.budget list of annual total budget of rehabilitation
#'   operations. The names of the list elements are the year numbers and the
#'   values of the list elements are the annual total budgets for the
#'   corresponding year. Example: list("2005"=100000, "2006"=110000,
#'   "2007"=120000)
#' 
.fileContentStrategy2 <- function(rehabilitation.costs, annual.total.budget)
{
  return(.fileContentStrategy1or2(rehabilitation.costs, annual.total.budget))
}

# .fileContentStrategy3 --------------------------------------------------------

#' Content for Strategy File 3
#' 
#' @param condition.labels vector of condition labels Example: c("C4", "C3",
#'   "C2", "C1")
#' @param rehabilitation.costs vector of rehabilitation costs per condition in
#'   the order of the corresponding condition labels in condition.labels 
#'   Example: c(200, 300, 400, 500)
#' @param max.tol.prop.of.length vector of maximum tolerated proportion of
#'   network length in each condition in the order of the corresponding
#'   condition labels in condition.labels. Example: c(1.0, 0.7, 0.4, 0.05)
#' @param range.years two element vector containing the first and last year of
#'   simulation
#' @param target.year year at which the proportions of the network length in
#'   each condition must have been brought just below their maximum tolerated
#'   value
#' 
.fileContentStrategy3 <- function(
  condition.labels, rehabilitation.costs, max.tol.prop.of.length, range.years,
  target.year
)
{
  rows.1.to.3 <- .fileContentStrategy0(range.years, condition.labels)

  row.4 <- paste(rehabilitation.costs, collapse=";")
  row.5 <- paste(max.tol.prop.of.length, collapse=";")

  return(sprintf("%s%s\n%s\n%d\n", rows.1.to.3, row.4, row.5, target.year))
}

# .fileContentStrategy1or2 -----------------------------------------------------

#' File Content for Strategy 1 or 2
#' 
#' @param rehabilitation.costs list of rehabilitation costs per condition. The
#'   names of the list elements are the condition labels and the values of the
#'   list elements are the corresponding rehabilitation costs. Example:
#'   list(C4=200, C3=300, C2=400, C1=500)
#' @param annual.total.length.or.budget list of annual total lengths to be
#'   rehabilitated or annual total budgets.
#' @importFrom kwb.utils defaultIfNA hsChrToNum
#' @importFrom stats approx
.fileContentStrategy1or2 <- function(
  rehabilitation.costs, annual.total.length.or.budget
)
{
  years.text <- names(annual.total.length.or.budget)

  if (is.null(years.text) || any(is.na(years.text))) {
    
    stop_(
      "All elements of the list of annual values must be named with the ",
      "name representing the year"
    )
  }

  condition.labels <- names(rehabilitation.costs)

  if (is.null(condition.labels) ||
      any(kwb.utils::defaultIfNA(condition.labels, "") == "")) {
    
    stop_(
      "All elements of the list of rehabilitation costs must be named with ",
      "the name representing the condition class"
    )
  }

  # hsChrToNum will raise an error if not all values can be converted to numbers
  years <- kwb.utils::hsChrToNum(years.text, country = "en")

  range.years <- range(years)

  # Generate (interpolated) annual total length or budget for each year between
  # min.year and max.year
  all.years <- if (diff(range.years) == 0) {
    range.years[1]
  } else {
    seq(range.years[1], range.years[2])
  }

  all.annual.values <- stats::approx(
    years, annual.total.length.or.budget, all.years
  )

  textblock1 <- .fileContentStrategy0(range.years, condition.labels)

  textblock2 <- paste(as.numeric(rehabilitation.costs), collapse = ";")

  textblock3 <- paste(
    all.annual.values$x,
    all.annual.values$y,
    sep = ";",
    collapse = "\n"
  )

  sprintf("%s%s\n%s\n", textblock1, textblock2, textblock3)
}
