# summary_generic_sim ----------------------------------------------------------

#' Aggregate simulated Condition Classes
#'
#' @param x data frame
#' @param column.group.by name of column in \code{x} by which values to group by
#' @param column.length name of column in \code{x} containing the pipe lengths
#'
#' @export
#' @importFrom kwb.utils countOrSum rbindAll
summary_generic_sim <- function(x, column.group.by, column.length)
{
  # Check for value columns
  columns <- .probabilityColumns(x)

  if (length(columns) == 0) {

    stop_(
      "Columns containing the probabilities (starting with 'prob') ",
      "are missing!"
    )
  }

  # Multiply matrix of probabilities with pipe lengths
  x[, columns] <- x[, columns] *  kwb.utils::selectColumns(x, column.length)

  # For each probability column "prob1", "prob2", ..., sum up the lengths
  # within each group (of pipes with the same age or year)
  result <- kwb.utils::rbindAll(lapply(columns, function(column) {
    kwb.utils::countOrSum(x, by = column.group.by, sum.up = column)
  }))

  row.names(result) <- columns

  result
}

# .probabilityColumns ----------------------------------------------------------
.probabilityColumns <- function(x)
{
  grep("^prob", names(x), value = TRUE)
}
