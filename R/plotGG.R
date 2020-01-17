# plotPrediction ---------------------------------------------------------------

#' Plot the Result of a Prediction
#'
#' Plot the distribution of condition classes by predicted year
#'
#' @param prediction data frame with columns \code{prob1}, \code{prob2}, ...,
#'   \code{pipeLength}, \code{InspectionYear}
#' @param legend_pos legend position: one of \code{"bottom", "left", "top",
#'   "right", "none"}
#' @param do_print logical indicating whether to actually print the plot to
#'   the current graphical device.
#'
#' @examples
#'
#' prediction <- data.frame(
#'   prob1 = c(0, 1, 0, 0.0, 0),
#'   prob2 = c(1, 0, 0, 0.5, 0),
#'   prob3 = c(0, 0, 0, 0.5, 1),
#'   prob4 = c(0, 0, 1, 0.0, 0),
#'   pipeLength = 1:5,
#'   InspectionYear = seq(2001, 2005)
#' )
#'
#' kwbGompitz:::plotPrediction(prediction, legend_pos = "right")
#'
plotPrediction <- function(
  prediction, legend_pos = c("bottom", "left", "top", "right", "none")[5],
  do_print = TRUE
)
{
  #kwb.utils::assignPackageObjects("kwbGompitz")

  conditionStat <- summary_generic_sim(
    prediction, column.group.by = "InspectionYear", column.length = "pipeLength"
  )

  plotobject <- plot_stacked_bars(conditionStat, legend = legend_pos) +
    ggplot2::xlab("Year of Prediction")

  if (do_print) {

    print(plotobject)
  }

  invisible(plotobject)
}

# plot_stacked_bars ------------------------------------------------------------

#' Generate gg-plot of Stacked Bars
#'
#' @param x matrix containing the bar heights
#' @param x.sd optional. Standard deviations.
#' @param reverse logical indicating whether to reverse the stack order
#' @param \dots additional arguments passed to \code{\link{gg_stacked_bars}}
#'
#' @export
#'
#' @examples
#'
#' values <- c(
#'   0, 2, 0, 0, 0,
#'   1, 0, 0, 2, 0,
#'   0, 0, 0, 2, 5,
#'   0, 0, 3, 0, 0
#' )
#'
#' conditionStat <- matrix(values, nrow = 4, byrow = TRUE, dimnames = list(
#'   condition = paste0("prob", 1:4),
#'   year = 2001:2005
#' ))
#'
#' # Get the base plot
#' baseplot <- kwbGompitz::plot_stacked_bars(conditionStat, legend = "right")
#'
#' # Show the base plot
#' baseplot
#'
#' # Modify the base plot (titles, axis titles, legend title, legend content)
#' baseplot + ggplot2::labs(
#'   x = "Jahr", y = "Anteil Kanaele in %", title = "Zustandsverteilung",
#'   subtitle = "Netzwerk: Berlin"
#' ) +
#' ggplot2::guides(fill = ggplot2::guide_legend("Zustandsklasse")) +
#' ggplot2::scale_fill_manual(
#'   values = c("darkgreen", "yellow", "darkorange3", "red3"),
#'   labels = c("good", "ok", "not so good", "bad")
#' )
#'
plot_stacked_bars <- function(x, x.sd = NULL, reverse = TRUE, ...)
{
  stopifnot(is.matrix(x))

  # If standard deviations are given in x.sd, minimum and maximum positions of
  # the error bars are added in columns "ymin" and "ymax", respectively
  data <- prepare_for_gg_stacked_bars(x, x.sd)

  gg_stacked_bars(data, reverse = reverse, ...)
}

# prepare_for_gg_stacked_bars --------------------------------------------------

#' Prepare data for plot_stacked_bars()
#'
#' @param x numeric vector
#' @param x.sd standard deviation
#'
prepare_for_gg_stacked_bars <- function(x, x.sd = NULL)
{
  # Prepare a list of matrices
  matrices <- list(

    # Matrix of pipe lenghts in m
    length.abs = x,

    # Matrix of percentages of pipe lengths on sum of lengths in each column
    length.rel = kwb.utils::columnwisePercentage(x, digits = NA)
  )

  # If standard deviations are given in x.sd, minimum and maximum positions of
  # the error bars are added in columns "ymin" and "ymax", respectively

  if (! is.null(x.sd)) {

    matrices$sd <- x.sd

    # Centre of error bars: Cumulative percentages per column
    matrices$ymid <- columnwise(matrices$length.rel, function(p) cumsum(p) - p)

    # Minimum and maximum value of the error bars
    matrices$ymin <- matrices$ymid - 2 * x.sd
    matrices$ymax <- matrices$ymid + 2 * x.sd
  }

  # Convert matrices to data frames in "long" format and cbind the data frames
  data <- matrixToLongDataFrame(x = matrices)

  # Name the first two columns "condition" and "age", respectively
  names(data)[1:2] <- c("condition", "age")

  # Convert age to factor if it is not yet a factor
  data$age <- kwb.utils::toFactor(data$age)

  # Check whether the sum of "length.rel" is 100 (or 0!) within each age group
  sums <- stats::aggregate(length.rel ~ age, data = data, FUN = sum)
  N <- nrow(sums)

  x <- sums[, 2]
  y1 <- rep(0, N)
  y2 <- rep(100, N)

  stopifnot(all(kwb.utils::almostEqual(x, y1) | kwb.utils::almostEqual(x, y2)))

  data
}

# columnwise -------------------------------------------------------------------

#' Apply a Function for each Column
#'
#' @param x two dimensional object
#' @param FUN function to be called for each column
#' @param \dots arguments passed to \code{FUN}
#'
columnwise <- function(x, FUN, ...)
{
  stopifnot(length(dim(x)) == 2)

  apply(x, MARGIN = 2, FUN = FUN, ...)
}

# matrixToLongDataFrame --------------------------------------------------------

#' Convert Data in wide View to long View
#'
#' @param x data frame or list of data frames
#' @param columnName column name in result data frame
#'
matrixToLongDataFrame <- function(x, columnName =  deparse(substitute(x)))
{
  if (is.list(x)) {

    # If x is a list, call this function for each list element and cbind all
    # resulting data frames

    stopifnot(! is.null(names.x <- names(x)))

    dataFrames <- lapply(names.x, function(name) {
      matrixToLongDataFrame(x[[name]], columnName = name)
    })

    return (cbindDataFrames(dataFrames))
  }

  # Otherwise it should represent a matrix and will be converted to a data
  # frame in "long" format

  keys <- names(dimnames(x))

  if (is.null(keys) || all(keys == "")) {
    keys <- c("row", "column")
  }

  df <- as.data.frame(x)

  if (is.table(x)) {

    # If x is a "table", as.data.frame() already converted to the "long"
    # format that we require. We just adjust the column names.

    names(df) <- c(keys, columnName)

  } else {

    # Otherwise we use hsMatrixToListForm() to convert from "wide" to "long"
    # format. Therefore we need an extra column representing the "keys",
    # here the row names of the matrix

    df[[keys[1]]] <- rownames(x)

    # Convert "wide" format to "long" format (data frame)
    df <- kwb.utils::hsMatrixToListForm(
      df,
      keyFields = keys[1],
      colNamePar = keys[2],
      colNameVal = columnName
    )
  }

  kwb.utils::resetRowNames(df)
}

# cbindDataFrames --------------------------------------------------------------

#' Bind Columns from a list of Data Frames
#'
#' @param dataFrames list of data frames
#' @param keyindex indices of (key) columns to be excluded
#'
cbindDataFrames <- function(dataFrames, keyindex = 1:2)
{
  data.keys <- dataFrames[[1]][, keyindex]

  data.values <- do.call(cbind, lapply(dataFrames, function(x) {
    columns <- setdiff(seq_len(ncol(dataFrames[[1]])), keyindex)
    x[, columns, drop = FALSE]
  }))

  cbind(data.keys, data.values)
}

# gg_stacked_bars --------------------------------------------------------------

#' Generate gg-plot of stacked Bars
#'
#' @param data data frame with columns \code{length.abs} (absolute pipe length
#'   in m), \code{length.rel} (relative pipe length in percent),
#'   \code{condition}
#' @param relative logical indicating whether to show absolute or relative
#'   lenghts
#' @param labels logical indicating whether to show labels (axes, title) are
#'   shown.
#' @param legend legend position: one of \code{"bottom", "left", "top", "right",
#'   "none"}
#' @param reverse logical indicating whether to stack the bars in reversed order
#' @param lng language acronym, "de" for German or "en" for English, that is
#'   passed to \code{kwbGompitz:::get_label}
#' @param colours named vector that maps the values in \code{data$condition}
#'   to colour names.
#'
#' @export
#'
gg_stacked_bars <- function(
  data, relative = TRUE, labels = FALSE,
  legend = c("bottom", "left", "top", "right", "none")[5], reverse = TRUE,
  lng = "en", colours = NULL
)
{
  #head(data[order(data$age, decreasing = TRUE), ], 3)
  #    condition   age length.abs length.rel        sd     ymid      ymin      ymax
  # 49     prob1 >= 80  43.263375  43.263375 3.5922413  0.00000 -7.184483  7.184483
  # 50     prob2 >= 80  10.880211  10.880211 1.0866295 43.26337 41.090116 45.436634
  # 51     prob3 >= 80  25.746490  25.746490 1.5288176 54.14359 51.085950 57.201221

  #FUN.position <- ifelse(relative, ggplot2::position_fill, ggplot2::position_stack)
  FUN.position <- ggplot2::position_stack

  # Prepare label column if required
  if (isTRUE(labels)) {

    data$label <- sprintf(
      "%0.1f km",
      kwb.utils::selectColumns(data, "length.abs") / 1000
    )
  }

  # Initialise the plot
  g <- ggplot2::ggplot(data, ggplot2::aes_string(
    x = "age",
    y = if (relative) "length.rel" else "length.abs",
    fill = "condition"
  ))

  # Add bars
  g <- g + ggplot2::geom_bar(
    stat = "identity",
    position = FUN.position(reverse = reverse)
    # position = "stack"
  )

  # Get default bar colours if no colours are given
  if (is.null(colours)) {
    
    condition_labels <- unique(kwb.utils::selectColumns(data, "condition"))
    
    colours <- get_default_colours_for_labels(condition_labels)
  }
  
  # Set bar colours
  g <- g + ggplot2::scale_fill_manual(values = colours)

  # Add bar labels
  if (isTRUE(labels)) {
    g <- g + ggplot2::geom_text(
      ggplot2::aes_string(x = "age", y = "length.abs", label = "label"),
      size = 6,
      position = FUN.position(vjust = 0.5, reverse = reverse)
    )
  }

  # Add error bars if columns "ymin" and "ymax" are provided
  if (all(c("ymin", "ymax") %in% names(data))) {

    # Plot only the error bars for the worst condition -> filter for max. cond.
    data.worst <- data[data$condition == max(unique(data$condition)), ]

    g <- g + ggplot2::geom_point(
      mapping = ggplot2::aes_string("age", "ymid"),
      data = data.worst,
      show.legend = FALSE
    )

    g <- g + ggplot2::geom_errorbar(
      mapping = ggplot2::aes_string(ymin = "ymin", ymax = "ymax"),
      data = data.worst,
      width = 0.3
    )
  }

  # Set language dependent plot elements
  g <- g + ggplot2::ylab(get_label("ylab.rel", lng))
  g <- g + ggplot2::labs(fill = get_label("title.legend", lng))

  # General formatting
  g <- g + ggplot2::theme_bw()

  # Show or hide the legend
  g <- g + to_theme_legend(legend)

  g
}

# get_default_colours_for_labels -----------------------------------------------
get_default_colours_for_labels <- function(condition_labels)
{
  n <- length(condition_labels)
  
  if (n == 4) {
    
    structure(toColours(1:4), names = condition_labels)
    
  } else {
    
    default_condition_colours(condition_labels)
  }
}
# toColours --------------------------------------------------------------------

#' Condition Class(es) to Colour String(s)
#'
#' @param x vector of condition class numbers or labels "prob1", "prob2", ...
#'
toColours <- function(x)
{
  colours <- c("darkgreen", "yellow", "darkorange3", "red3")
  
  indices <- as.integer(gsub("prob", "", x))
  
  if (any(is.na(indices))) {
    
    stop_(
      "I cannot guess the condition class number from these condition ", 
      "class labels:\n  ", kwb.utils::stringList(x[is.na(indices)])
    )
  }
  
  n_colours <- length(colours)
  
  if (! all(kwb.utils::inRange(indices, 1, n_colours))) {
  
    index_range <- range(indices)
    
    text <- paste(
      "The range of condition class numbers (%d .. %d) exceeds the range of",
      "colours (1 .. %d)"
    )
    
    stop_(sprintf(text, index_range[1], index_range[2], n_colours))
  }
  
  colours[indices]
}

# default_condition_colours ----------------------------------------------------

#' Get default Vector of Condition Colours
#'
#' @param condition_labels vector of condition class names ordered from the best
#'   to the worst condition
#'
#' @return vector of colour codes with the conditions as element names
#'
#' @export
#'
default_condition_colours <- function(condition_labels)
{
  # Colours from red to green
  colours <- grDevices::rainbow(length(condition_labels), start = 0, end = 0.3)

  # Colours from green to red with names from best to worst condition
  structure(rev(colours), names = condition_labels)
}

# get_label --------------------------------------------------------------------

#' Provide Label for Plot in selected Language
#'
#' @param element one of "title", "ylab", "ylab.rel", "ylab.dev", "title.legend"
#' @param lng language acronym, "de" for German or "en" for English
#'
get_label <- function(element = NULL, lng = "en")
{
  configs <- list(
    title = c(
      de = "Zustandsverteilung",
      en = "Condition Distribution"
    ),
    ylab = c(
      de = "Kanallaenge (km)",
      en = "Pipe length (km)"
    ),
    ylab.rel = c(
      de = "Anteil an Kanallaenge (%)",
      en = "Share of Pipe length (%)"
    ),
    ylab.dev = c(
      de = "Abweichung des L\\xe4ngenanteils (%)",
      en = "Deviation of Pipe length share (%)"
    ),
    title.legend = c(
      de = "Zustandsklasse",
      en = "Condition Class"
    )
  )

  if (is.null(element)) {
    lapply(configs, "[", lng)
  } else {
    kwb.utils::selectElements(configs, element)[lng]
  }
}

# to_theme_legend --------------------------------------------------------------

#' Generate ggplot Theme for Legend
#'
#' @param legend logical indicating whether to put a legend or not or a string
#'   giving the legend position ("left", "right", "top", "bottom")
#'
to_theme_legend <- function(legend)
{
  ggplot2::theme(legend.position = if (is.logical(legend)) {
    ifelse(legend, "right", "none")
  } else {
    legend
  })
}
