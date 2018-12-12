# plot_pipe_conditions ---------------------------------------------------------

#' Plot the Pipe Conditions over Time
#'
#' @param x data frame
#' @param column_instyear name of column containing the installation year
#' @param column_ident name of column containing the pipe identifier
#' @param column_inspyear name of column containing the inspection year
#' @param column_condition name of column containing the pipe condition
#' @param colour_values named vector of colour names. The names must refer to
#'   the values in \code{column_condition}.
#' @param y_labels logical. If \code{TRUE} tick marks and labels are plotted
#'   on the y axis, else not.
#' @param facet_by character string representing a formula to be used to create
#'   facets with \code{\link[ggplot2]{facet_wrap}}. Set to \code{NULL} if no
#'   facets are desired.
#' @param by_pipe if \code{TRUE} one plot is created for each pipe and a list
#'   of these plots is returned
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- kwbGompitz:::readObservations(kwbGompitz::exampleFile("obs.txt"))
#' plot_pipe_conditions(x[1:100, ])
#' }
#'
plot_pipe_conditions <- function
(
  x, column_instyear = "instyear", column_ident = "ident",
  column_inspyear = "inspyear", column_condition = "condition",
  colour_values = c(
    C4 = "green", C3 = "yellow", C2 = "orange", C1 = "red", "grey"
  ),
  y_labels = FALSE,
  facet_by = sprintf(
    kwb.utils::underscoreToPercent("~(10*as.integer(_s/10))"), column_instyear
  ),
  by_pipe = FALSE
)
{
  #column_instyear="instyear";column_ident="ident";column_inspyear="inspyear";column_condition="condition";colour_values=c(C4 = "green", C3 = "yellow", C2 = "orange", C1 = "red", "grey");y_labels=FALSE;facet_by=NULL;by_pipe=FALSE

  get <- kwb.utils::selectColumns

  x <- x[order(get(x, column_instyear)), ]

  pipe_ids <- get(x, column_ident)

  if (by_pipe) {

    pipe_list <- lapply(sort(unique(pipe_ids)), function(pipe_id) {
      x[pipe_ids == pipe_id, ]
    })

    # Call this function recursievly for each pipe
    return (lapply(
      pipe_list, plot_pipe_conditions, column_instyear = column_instyear,
      column_ident = column_ident, column_inspyear = column_inspyear,
      column_condition = column_condition, colour_values = colour_values,
      y_labels = y_labels, facet_by = NULL, by_pipe = FALSE
    ))
  }

  x[, column_ident] <- factor(pipe_ids, levels = unique(pipe_ids))

  gg <- ggplot2::ggplot(x, ggplot2::aes_string(column_instyear, column_ident)) +
    ggplot2::geom_point(pch = "|") +
    ggplot2::geom_point(ggplot2::aes_string(
      column_inspyear, column_ident, colour = column_condition
    )) +
    ggplot2::scale_colour_manual(values = colour_values) +
    ggplot2::ylab("Pipes")

  if (! is.null(facet_by)) {
    wrap_formula <- stats::as.formula(facet_by)
    gg <- gg + ggplot2::facet_wrap(wrap_formula, scales = "free", nrow = 2)
  }

  if (! y_labels) {

    blank <- ggplot2::element_blank
    gg <- gg + ggplot2::theme(axis.ticks.y = blank(), axis.text.y = blank())
  }

  gg
}

# plot_prediction_by_pipe ------------------------------------------------------
plot_prediction_by_pipe <- function # Plot Condition Probabilities by Pipe
### List of ggplots with each plot representing the evolution of condition
### probabilities over time for one pipe
(
  prediction,
  ### data frame as returned by \code{\link{runGompitzPrediction}}
  prefix = "prob",
  ### prefix of column names containing the probabilities. Default: "prob"
  pipe_ids = unique(kwb.utils::selectColumns(prediction, "pipe_id")),
  ### vector of pipe IDs for which a plot is to be generated. By default all
  ### available pipes are considered!
  width = 1
  ### passed to \code{\link[ggplot2]{geom_col}}
)
{
  # normalise the format (e.g. column names) of the prediction data frame
  prediction <- format_prediction(prediction)

  start <- nchar(prefix) + 1

  fields <- names(prediction)

  # Names of columns containing the probabilities
  prob_fields <- fields[startsWith(fields, prefix)]

  # Names of other columns
  key_fields <- setdiff(fields, prob_fields)

  # Convert from wide to long format
  x <- kwb.utils::hsMatrixToListForm(
    prediction, key_fields, colNamePar = "Condition"
  )

  # Get the possible condition labels from the probability column names
  condition_labels <- substr(prob_fields, start, nchar(prob_fields))

  # Set condition labels in reverse order to achieve reversed stack order
  x$Condition <- substr(x$Condition, start, nchar(x$Condition))
  x$Condition <- factor(x$Condition, levels = rev(condition_labels))

  # Get the vector of pipe IDs
  pipe_id_vector <- kwb.utils::selectColumns(x, "pipe_id")

  # Provide a named vector of fill colours
  fill_values <- toColours(condition_labels)
  fill_values <- structure(fill_values, names = condition_labels)

  # Create one ggplot per pipe
  plots <- lapply(pipe_ids, function(pipe_id) {

    pipe_data <- x[pipe_id_vector == pipe_id, ]

    ggplot2::ggplot(pipe_data, ggplot2::aes_string("year", "parVal")) +
      ggplot2::geom_col(ggplot2::aes_string(fill = "Condition"), width = width) +
      ggplot2::scale_fill_manual(values = fill_values) +
      ggplot2::ggtitle(sprintf(
        "Pipe: '%s', Stratum: '%s'",
        pipe_id, kwb.utils::selectColumns(pipe_data, "stratum_no")
      )) +
      ggplot2::ylab("Cumulative Probability")
  })

  plots
}

# format_prediction ------------------------------------------------------------
format_prediction <- function(prediction)
{
  #head(prediction)

  replacements <- list(
    "^(Prediction|Inspection)Year$" = "year",
    "^(PipeID|ident)$" = "pipe_id",
    "^stratumNo$" = "stratum_no"
  )

  names(prediction) <- kwb.utils::multiSubstitute(names(prediction), replacements)

  kwb.utils::checkForMissingColumns(prediction, as.character(replacements))

  # Normalise types
  prediction$pipe_id <- as.character(prediction$pipe_id)
  prediction$year <- as.integer(prediction$year)

  # Round numeric columns
  columns <- c(grep("^prob", names(prediction), value = TRUE), "index")
  prediction <- kwb.utils::roundColumns(prediction, columns, digits = 6)

  # Remove column "stratum" (we have stratum_no instead)
  prediction <- kwb.utils::removeColumns(prediction, "stratum")

  # Remove attributes
  prediction <- structure(prediction, calibration = NULL)

  # Remove additional classes
  structure(prediction, class = "data.frame")
}
