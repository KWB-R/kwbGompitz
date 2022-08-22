# plot_survival_curves ---------------------------------------------------------

#' Plot Survival Curves
#'
#' @param calibration calibration object
#' @param stratum name of stratum
#' @param marginal logical. If \code{TRUE} (default) the marginal survival
#'   curves are plotted, otherwise the non-marginal survival curves
#' @param t vector of times at which to calculate the survival curves
#' @param col vector of colour names, named according to the conditions as
#'   stored in \code{attr(calibration, "parameters")$conditions}
#' @param \dots arguments passed to \code{\link[kwb.plot]{plot_curve_areas_gg}},
#'   such as \code{legend}, \code{line_colour}
#' @param args arguments passed to get_survivals
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get the example calibration provided with the Gompitz software
#' calibration <- kwbGompitz::exampleCalibration()
#'
#' # Generate one ggplot2-object for each calibrated stratum
#' (plots_1 <- plot_survival_curves(calibration))
#'
#' # By default the marginal survival curves are shown. You may set marginal to
#' # FALSE to get the non-marginal survival curves
#' (plots_2 <- plot_survival_curves(calibration, marginal = FALSE))
#'
#' # Compare the plots
#' gridExtra::grid.arrange(plots_1[[1]], plots_2[[1]], plots_1[[2]], plots_2[[2]])
#' }
#' @importFrom kwb.utils excludeNULL getAttribute toNamedList
#' @importFrom kwb.plot plot_curve_areas_gg
#' @importFrom ggplot2 element_blank labs theme
plot_survival_curves <- function(
  calibration = exampleCalibration(), stratum = NULL, marginal = TRUE,
  t = 1:100, col = NULL, ..., args = NULL
)
{
  param <- kwb.utils::getAttribute(calibration, "parameters")

  if (is.null(col)) {

    col <- get_default_colours_for_labels(condition_labels = param$conditions)
  }

  if (is.null(stratum)) {

    strata <- names(calibration)

    result <- lapply(
      kwb.utils::toNamedList(strata),
      plot_survival_curves,
      calibration = calibration,
      marginal = marginal,
      t = t,
      col = col,
      ...,
      args = args
    )

    return (kwb.utils::excludeNULL(result))
  }

  stratum_param <- param$byStratum[[stratum]]

  if (! is.null(stratum_param)) {

    stratum_conditions <- stratum_param$conditions

    # Get the survival values
    arguments <- list(
      calibration = calibration, stratum = stratum, marginal = marginal, t = t,
      matrix = FALSE
    )

    y <- do.call(get_survivals, c(arguments, args))

    # Add ones to build the last area (worst observed condition in stratum)
    y[[rev(stratum_conditions)[1]]] <- rep(1, length(t))

    # We have to reverse the vectors of y values because plot_curve_areas_gg
    # puts the first vector to the lowest layer and the last vector to the
    # upmost layer
    kwb.plot::plot_curve_areas_gg(t, rev(y), col[stratum_conditions], ...) +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      y_axis_percent() +
      ggplot2::labs(
        x = "Age in Years",
        y = "Condition Probabilities",
        title = stratum,
        subtitle = paste0(ifelse(marginal, "Marginal ", ""), "Survival")
      )
  }
}

# y_axis_percent ---------------------------------------------------------------

#' y axis percent 
#'
#' @param by by (default: 0.1) 
#' @param ... additional arguments passed to ggplot2::scale_y_continuous
#'
#' @return ???
#' @keywords internal
#' @noRd
#' @noMd
#' @importFrom ggplot2 scale_y_continuous
y_axis_percent <- function(by = 0.1, ...)
{
  # Define y tick positions
  at <- seq(0, 1, by)
  
  ggplot2::scale_y_continuous(breaks = at, labels = paste(100 * at, "%"), ...)
}

# plotCalibration1 -------------------------------------------------------------

#' Plot Calibration Result
#'
#' @param calib calibration result as retrieved by
#'   \code{\link{runGompitzCalibration}}
#' @param to.pdf if \code{TRUE} the plot is directed into a temporary PDF file
#' @importFrom kwb.utils finishAndShowPdfIf preparePdfIf
#' @importFrom graphics par text
plotCalibration1 <- function(calib, to.pdf = FALSE)
{
  pdf.file <- kwb.utils::preparePdfIf(to.pdf)
  opar <- graphics::par(mfrow = c(1, length(calib)), mar = c(3,3,3,0))

  on.exit(graphics::par(opar))
  on.exit(kwb.utils::finishAndShowPdfIf(to.pdf, pdf.file), add = TRUE)

  max.y <- max(as.numeric(sapply(sapply(calib, "[", "pipes"), "[", "number")))

  for (stratum.name in names(calib)) {

    stratum <- calib[[stratum.name]]

    values <- c(
      stratum$pipes$number,
      stratum$pipes$weight,
      stratum$inspections$number,
      stratum$inspections$weight
    )

    height <- matrix(values, nrow = 2, byrow = FALSE)

    xpos <- graphics::barplot(
      height = height,
      beside = TRUE,
      ylim = c(0, 1.1 * max.y),
      names.arg = c("Pipes", "Inspections"),
      main = stratum.name,
      args.legend = list(x = 5, y = 1.1 * max.y),
      legend.text = c("Number", "Weight")
    )

    graphics::text(xpos, height + 0.01 * max.y, as.character(height), cex = 0.9)
  }
}

# plotCalibration2 -------------------------------------------------------------

#' Plot Calibration Result (2)
#'
#' @param calib calibration result as retrieved by
#'   \code{\link{runGompitzCalibration}}
#' @param to.pdf if \code{TRUE} the plot is directed into a temporary PDF file
#' @importFrom kwb.utils finishAndShowPdfIf preparePdfIf
#' @importFrom graphics barplot text
plotCalibration2 <- function(calib, to.pdf = FALSE)
{
  obs.by.condition <- .getObservationByCondition(calib)

  height <- as.matrix(obs.by.condition[, -1])
  max.y <- max(height, na.rm = TRUE)

  pdf.file <- kwb.utils::preparePdfIf(to.pdf)
  on.exit(kwb.utils::finishAndShowPdfIf(to.pdf, pdf.file))

  xpos <- graphics::barplot(
    height,
    beside = TRUE,
    ylim = c(0, 1.1 * max.y),
    main = "Number of observations in condition class",
    legend.text = obs.by.condition$condition
  )

  graphics::text(xpos, height + 0.03 * max.y, as.character(height), cex = 0.9)
}

# plotPredictionByYear ---------------------------------------------------------

#' Plot Prediction by Year to temp. PDF File
#'
#' Plot prediction by year to temporary PDF file
#'
#' @param prediction prediction result as provided by e.g.
#'   \code{\link{runGompitzPrediction}}
#' @param to.pdf if \code{TRUE} (default) the plot goes into a PDF file
#' @param FUN.name name of function to be used to order the matrix of
#'   probabilities. Available functions: "orderByWeightedProbabilities",
#'   "orderByMostProbClassAndDecreasingProb"; default:
#'   "orderByWeightedProbabilities"
#' @param column.year name of column containing the year
#'
#' @seealso \code{\link{runGompitzPrediction}}
#' @importFrom kwb.utils finishAndShowPdfIf preparePdfIf selectColumns
plotPredictionByYear <- function(
  prediction, to.pdf = TRUE, FUN.name = "orderByWeightedProbabilities",
  column.year = "year"
)
{
  years <- sort(unique(kwb.utils::selectColumns(prediction, column.year)))

  file.pdf <- kwb.utils::preparePdfIf(to.pdf)

  for (year in years) {

    plotPredictionForYear(
      prediction, year, FUN.name = FUN.name, column.year = column.year
    )
  }

  kwb.utils::finishAndShowPdfIf(to.pdf, file.pdf)
}

# plotPredictionForYear --------------------------------------------------------

#' Plot Prediction for the given Year
#'
#' @param res prediction result as provided by e.g.
#'   \code{\link{runGompitzPrediction}}
#' @param year year number for which results are to be filtered an plot is to be
#'   generated
#' @param FUN.name name of function to be used to order the matrix of
#'   probabilities. Available functions: "orderByWeightedProbabilities",
#'   "orderByMostProbClassAndDecreasingProb"; default:
#'   "orderByWeightedProbabilities"
#' @param main main title of plot
#' @param column.year name of column containing the year
#' @seealso \code{\link{runGompitzPrediction}}
#' @importFrom kwb.utils selectColumns
#' @importFrom graphics barplot
plotPredictionForYear <- function(
  res, year, FUN.name = "orderByWeightedProbabilities", main = NULL,
  column.year = "year"
)
{
  # set default main title
  if (is.null(main)) {
    main <- paste(year, sprintf("(order function: %s)", FUN.name))
  }

  prob.matrix <- .extractMatrixOfProbabilities(res)

  res <- res[do.call(paste0(".", FUN.name), list(prob.matrix)), ]

  # Filter for the given year
  res.year <- res[kwb.utils::selectColumns(res, column.year) == year, ]

  prob.matrix <- .extractMatrixOfProbabilities(res.year)

  names.arg <- rep("", nrow(prob.matrix))
  graphics::barplot(t(prob.matrix), border = NA, main = main, names.arg = names.arg,
          legend.text = dimnames(prob.matrix)[[2]])
}

# .extractMatrixOfProbabilities ------------------------------------------------

.extractMatrixOfProbabilities <- function(res)
{
  as.matrix(res[, .probabilityColumns(res)])
}

# .orderByWeightedProbabilities ------------------------------------------------

#' probability order 2
#'
#' order by weighted probabilities
#'
#' @param probabilities matrix of probabilities
#' @param weight vector of weights with as many elements as there are columns in
#'   probabilities
#'
.orderByWeightedProbabilities <- function(
  probabilities, weight = seq_len(ncol(probabilities))
)
{
  FUN <- function(probability.vector) {
    sum(probability.vector * weight, na.rm = TRUE)
  }

  indicator <- apply(probabilities, 1, FUN)
  order(indicator)
}

# .orderByMostProbClassAndDecreasingProb ---------------------------------------

#' Probability Order
#'
#' Order by most probable condition classes and decreasing probability
#'
#' @param probabilities maxtrix of numeric representing probabilities
#' @noRd
#' 
.orderByMostProbClassAndDecreasingProb <- function(probabilities)
{
  max.prob <- apply(probabilities, 1, max, na.rm = TRUE)
  whichmax <- apply(probabilities, 1, which.max)
  order(whichmax, -max.prob)
}
