# demo_plots_survival_after_repair ---------------------------------------------
demo_plots_survival_after_repair <- function(variant = 1, t = 0:80)
{
  # Get an example calibration
  calibration <- exampleCalibration()

  # Create two different argument lists for kwbGompitz::plot_survival_curves
  arguments <- list(calibration = calibration, stratum = "Cast Iron", t = t)
  arguments_1 <- kwb.utils::arglist(arguments, marginal = FALSE)
  arguments_2 <- kwb.utils::arglist(arguments, marginal = TRUE)

  # Create three different argument lists passed to get_survivals
  args_0 <- list(interpol_info = NULL)

  # Define interpolation info
  args_1 <- list(interpol_info = list(start = 30, length = 20, target = 1))
  args_2 <- list(interpol_info = list(start = 30, length = 20, target = 2))
  args_3 <- list(interpol_info = list(start = 30, length = 30, target = 1))
  args_4 <- list(interpol_info = list(start = 30, length = 30, target = 2))

  # Call the plot function with different argument lists
  p10 <- kwb.utils::callWith(plot_survival_curves, arguments_1, args = args_0)
  p11 <- kwb.utils::callWith(plot_survival_curves, arguments_1, args = args_1)
  p12 <- kwb.utils::callWith(plot_survival_curves, arguments_1, args = args_2)
  p13 <- kwb.utils::callWith(plot_survival_curves, arguments_1, args = args_3)
  p14 <- kwb.utils::callWith(plot_survival_curves, arguments_1, args = args_4)

  p20 <- kwb.utils::callWith(plot_survival_curves, arguments_2, args = args_0)
  p21 <- kwb.utils::callWith(plot_survival_curves, arguments_2, args = args_1)
  p22 <- kwb.utils::callWith(plot_survival_curves, arguments_2, args = args_2)
  p23 <- kwb.utils::callWith(plot_survival_curves, arguments_2, args = args_3)
  p24 <- kwb.utils::callWith(plot_survival_curves, arguments_2, args = args_4)

  if (variant == 1) {

    plots_raw <- list(p10, p20, p11, p12, p21, p22)

    # Modify the subtitles
    kwb.plot::set_subtitles(
      plots_raw,
      subtitle = sprintf(
        " (%d year lasting repair to %sbest condition in year %d)",
        args_1$interpol_info$length, c("", "second "),
        args_1$interpol_info$start
      ),
      indices = 3:6,
      action = "append"
    )
  } else if (variant == 2) {

    plots_raw <- list(p20, p20, p21, p22, p23, p24)

    # Modify the subtitles
    fmt <- "%d year lasting repair to %sbest condition"

    kwb.plot::set_labels(
      plots_raw,
      title = c(
        rep("No Reparation", 2),
        rep(sprintf("Reparation in year %d", args_1$interpol_info$start), 4)
      ),
      subtitle = c(
        rep("", 2),
        sprintf(fmt, args_1$interpol_info$length, ""),
        sprintf(fmt, args_1$interpol_info$length, "second "),
        sprintf(fmt, args_2$interpol_info$length, ""),
        sprintf(fmt, args_2$interpol_info$length, "second ")
      )
    )

  } else {

    stop("Only two variants defined")
  }
}

# get_survivals ----------------------------------------------------------------

#' Get Survival Curves from a Calibration Object
#'
#' @param calibration calibration object as returned by
#'   \code{\link{runGompitzCalibration}}
#' @param stratum name of stratum for which to return the survival curves
#' @param t vector of times for which to calculate the survival probabilities
#' @param matrix if \code{TRUE} (default) the result is returned as a matrix,
#'   otherwise as a list.
#' @param \dots further arguments passed to \code{\link{survivals}}
#'
#' @seealso \code{\link{survivals}}
#'
#' @export
#'
get_survivals <- function(calibration, stratum, t = 1:100, matrix = TRUE, ...)
{
  args_1 <- get_survival_parameters(calibration, stratum)
  args_2 <- list(t = t, matrix = matrix, ...)

  do.call(survivals, c(args_1, args_2))
}

# get_survival_parameters ------------------------------------------------------
get_survival_parameters <- function(
  calibration = exampleCalibration(), stratum = NULL, param = NULL,
  format = if (is.null(param)) 1 else 3
)
{
  # If no stratum is specified call this function for each stratum and return
  # a list of results
  if (is.null(stratum)) {

    strata <- names(which(
      .calibrationAvailable(calibration, param = param)
    ))

    return (lapply(
      kwb.utils::toNamedList(strata),
      get_survival_parameters,
      calibration = calibration,
      format = format,
      param = param
    ))
  }

  converged <- .calibrationAvailable(calibration, stratum, param)

  if (! converged) {

    message(sprintf("For stratum \"%s\" the model did not converge.\n", stratum))

    return (NULL)
  }

  if (format %in% 1:2) {

    .get_pars_from_calibration(calibration, stratum, format)

  } else if (format == 3) {

    if (is.null(param)) {
      param <- kwb.utils::getAttribute(calibration, "parameters")
    }

    .get_pars_from_param(param, stratum)

  } else {

    stop("format ", format, " not supported!")
  }
}

# .get_pars_from_calibration ---------------------------------------------------
.get_pars_from_calibration <- function(calibration, stratum, format = 1)
{
  stopifnot(format %in% 1:2)

  # Extend the table of estimations with information from splitting the labels
  stratum_calib <- kwb.utils::selectElements(calibration, stratum)
  estimates <- stratum_calib$convergence$estimate
  estimates <- cbind(split_labels(x = estimates[, 1]), estimates[, -1])

  if (format == 2) {
    return (estimates)
  }

  # Split the data frame into groups in which the parameter name is the same
  # and filter estimates_list for elements regarding alpha, beta and iff values
  estimates_list <- split(estimates, estimates$name)
  estimates_list <- estimates_list[c("Alpha", "T", "Sigma")]

  # Get the alpha parameters. There is one alpha value less than there are
  # conditions that were actually found in the current stratum. Name the
  # parameters according to the condition label that it refers to (column "ind")
  alpha_estimates <- kwb.utils::selectElements(estimates_list, "Alpha")
  alpha <- structure(alpha_estimates$Estimate, names = alpha_estimates$ind)

  list(
    alpha = alpha,
    bz1 = kwb.utils::selectElements(estimates_list, "T")$Estimate,
    bz0 = 0,
    s  = kwb.utils::selectElements(estimates_list, "Sigma")$Estimate
  )
}

# split_labels -----------------------------------------------------------------
split_labels <- function(x)
{
  # Define a pattern describing the obligatory and optional parts of the label
  pattern <- kwb.utils::resolve(
    "^<name><ref_paren>?<ref_brack>? <versus>$",
    name = "([^[(]+)",
    ref_paren = "(<paren_open><name><paren_close>)",
    ref_brack = "(<brack_open><name><brack_close>)",
    versus = "<paren_open>vs (<name><ref_paren>?)<paren_close>",
    paren_open = "\\(",
    paren_close = "\\)",
    brack_open = "\\[",
    brack_close = "\\]"
  )

  # Split the labels into the substrings defined by the pattern
  index <- c(name = 1, ind = 3, level = 5, vs_name = 7, vs_ind = 9)
  result <- kwb.utils::extractSubstring(pattern, x, index)
  result$label <- x

  # Check if recreating the labels from the parts results in the original labels
  stopifnot(all(create_label(result) == result$label))

  # Check if each non-empty "vs_ind" is found as "ind" one row above
  rows <- which(result$vs_ind != "")
  stopifnot(all(result$ind[rows - 1] == result$vs_ind[rows]))

  kwb.utils::moveColumnsToFront(result, "label")
}

# create_label -----------------------------------------------------------------
create_label <- function(x)
{
  embrace <- function(x, chars) {
    chars <- strsplit(chars, "")[[1]]
    available <- (x != "")
    x[available] <- paste0(chars[1], x[available], chars[2])
    x
  }

  sprintf(
    "%s%s%s (vs %s%s)", x$name, embrace(x$ind, "()"), embrace(x$level, "[]"),
    x$vs, embrace(x$vs_ind, "()")
  )
}

# split_theta ------------------------------------------------------------------
split_theta <- function(theta, n_cond, n_bz0, n_bz1)
{
  stopifnot(length(theta) == n_cond + n_bz0 + n_bz1 + 1)

  x <- c(alpha = n_cond - 1, bz0 = n_bz0, bz1.gen = 1, bz1 = n_bz1, s = 1)

  offset <- 0

  lapply(x, function(n) {
    elements <- theta[seq_len(n) + offset]
    offset <<- offset + n
    elements
  })
}

# .get_pars_from_param ---------------------------------------------------------
.get_pars_from_param <- function(param, stratum)
{
  get <- kwb.utils::selectElements

  param <- get(get(param, "byStratum"), stratum)
  conditions <- get(param, "conditions")

  # If param is in the "raw" version (as read from param.txt) the single
  # parameters need to be split from the "estimates" field, otherwise the
  # single parameters "alpha", "bz1", "s" should already be available
  pars <- if ("estimates" %in% names(param)) {

    stati <- get(param, "stati")

    split_theta(
      theta = get(param, "estimates"),
      n_cond = length(conditions),
      n_bz0 = sum(base::bitwAnd(stati, 1) > 0),
      n_bz1 = sum(base::bitwAnd(stati, 2) > 0)
    )
  } else {

    # Rename "bz1" to "bz1.gen" -> pars similar to what split_theta() returns
    get(param, c("alpha", bz1.gen = "bz1", "s"))
  }

  list(
    alpha = structure(
      get(pars, "alpha"),
      names = conditions[- length(conditions)]
    ),
    bz1 = get(pars, "bz1.gen"),
    bz0 = 0,
    s = get(pars, "s")
  )
}

# survivals --------------------------------------------------------------------

#' Get Values of (Standard or Marginal) Survival Curves
#'
#' @param t numeric vector of times (ages)
#' @param alpha numeric vector of alpha-parameter(s)
#' @param bz1 bz1 parameter
#' @param bz0 bz0 parameter
#' @param s passed to \code{\link{marginal_survival}}
#' @param marginal if \code{TRUE} the marginal survival curve with \code{s} as
#'   standard deviation is calculated instead of the standard survival curve.
#'   By default \code{marginal} is \code{TRUE} if \code{s} is not \code{NULL}.
#' @param matrix if \code{TRUE} and the length of \code{alpha} is greater than
#'   one the result is a matrix with each row representing one \code{alpha}
#'   value and each column representing a time. Otherwise the result is a list
#'   with each list element representing one \code{alpha} value
#' @param set_attributes if \code{TRUE} (the default is \code{FALSE}) an
#'   attribute \code{args} containing the arguments given to this function is
#'   set in the result
#' @param interpol_info if not \code{NULL} it is expected to be a list
#'   containing the elements \code{start} (start year of interpolation) and
#'   \code{length} (duration of interpolation in years)
#' @param \dots further arguments passed to \code{\link{marginal_survival}} or
#'   \code{\link{standard_survival}}
#'
#' @export
#'
survivals <- function(
  t = 0:99, alpha, bz1, bz0, s = NULL, marginal = ! is.null(s), matrix = TRUE,
  set_attributes = FALSE, interpol_info = NULL, ...
) {

  if (is.null(interpol_info)) {

    survivals_original(
      t = t, alpha = alpha, bz1 = bz1, bz0 = bz0, s = s, marginal = marginal,
      matrix = matrix, set_attributes = set_attributes
      , ...
    )

  } else {

    if (any(diff(t) != 1) || any(as.integer(t) != t)) {

      stop_(
        "Interpolation requires t to be a sequence of integer values ",
        "increasing by one"
      )
    }

    args_survival <- list(
      alpha = alpha, bz1 = bz1, bz0 = bz0, s = s, marginal = marginal
      , ...
    )

    out <- survivals_interpolated(
      t_range = range(t), interpol_info, args_survival = args_survival
    )

    if (matrix) out else kwb.utils::asRowList(out)
  }
}

# survivals_original -----------------------------------------------------------

#' Get Values of (Standard or Marginal) Survival Curves
#'
#' @param t numeric vector of times (ages)
#' @param alpha numeric vector of alpha-parameter(s)
#' @param bz1 bz1 parameter
#' @param bz0 bz0 parameter
#' @param s passed to \code{\link{marginal_survival}}
#' @param marginal if \code{TRUE} the marginal survival curve with \code{s} as
#'   standard deviation is calculated instead of the standard survival curve.
#'   By default \code{marginal} is \code{TRUE} if \code{s} is not \code{NULL}.
#' @param matrix if \code{TRUE} and the length of \code{alpha} is greater than
#'   one the result is a matrix with each row representing one \code{alpha}
#'   value and each column representing a time. Otherwise the result is a list
#'   with each list element representing one \code{alpha} value
#' @param set_attributes if \code{TRUE} (the default is \code{FALSE}) an
#'   attribute \code{args} containing the arguments given to this function is
#'   set in the result
#' @param \dots further arguments passed to \code{\link{marginal_survival}} or
#'   \code{\link{standard_survival}}
#'
#' @export
#'
survivals_original <- function(
  t = 0:99, alpha, bz1, bz0, s = NULL, marginal = ! is.null(s), matrix = TRUE,
  set_attributes = FALSE, ...
)
{
  survival_matrix <- options()[["kwbGompitz.survival_matrix"]]
  
  if (! is.null(survival_matrix)) {
    
    return(survival_matrix)
  }
  
  args_survival <- list(
    alpha = alpha, bz1 = bz1, bz0 = bz0, s = s, marginal = marginal, ...
  )

  result <- lapply(alpha, function(x) {

    if (marginal) {

      marginal_survival(t = t, alpha = x, bz1 = bz1, bz0 = bz0, s = s, ...)

    } else {

      standard_survival(t = t, alpha = x, bz1 = bz1, bz0 = bz0, ...)
    }
  })

  result <- if (matrix && is.list(result)) {

    do.call(rbind, result)

  } else if (! matrix && is.matrix(result)) {

    kwb.utils::asRowList(result)

  } else {

    result
  }

  # Set attribute "args" to arg_list if set_attributes is TRUE
  if (set_attributes) {

    attr(result, "args") <- c(args_survival, list(t = t, matrix = matrix))
  }

  result
}

# survivals_interpolated -------------------------------------------------------
survivals_interpolated <- function(t_range, interpol_info, args_survival)
{
  getele <- kwb.utils::selectElements

  (t_start <- getele(interpol_info, "start"))
  (t_end <- t_start + getele(interpol_info, "length"))

  (t_min <- min(t_range[1], t_start))
  (t_max <- max(t_range[2], t_end))
  (times <- seq(t_min, t_max))

  # Get the survival matrix over the whole required time range
  args_survival <- c(args_survival, list(t = times), matrix = TRUE)
  y <- do.call(survivals_original, args_survival)
  colnames(y) <- times

  in_range <- kwb.utils::inRange(times, t_range[1], t_range[2])

  # If interpolation starts after the maximum of t-values, we are done
  if (t_start > t_range[2]) {

    return (y[, in_range, drop = FALSE])
  }

  # We need the values y_1 at start and y_2 at end of interpolation
  y_linear <- kwbGompitz::interpolate_between(
    y1 = rep(1, nrow(y)),
    y2 = y[, times == t_end],
    n = t_end - t_start + 1
  )

  y[, as.character(t_start:t_end)] <- y_linear

  # Set survival after interpolation start to zero if the target is not one
  if ((target <- getele(interpol_info, "target")) > 1) {

    y[seq_len(nrow(y)) < target, as.character(t_start:t_max)] <- 0
  }

  y[, in_range, drop = FALSE]
}
