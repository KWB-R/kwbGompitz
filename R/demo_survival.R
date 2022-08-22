# demo_survival ----------------------------------------------------------------

#' Interactive Plot of Survival Curves
#'
#' @param t vector of times at which to calculate the survival curves
#' @param alpha.0 initial value of the \code{alpha} parameter
#' @param bz0.0 initial value of the \code{bz0} parameter
#' @param bz1.0 initial value of the \code{bz1} parameter
#' @param theme ggplot2-theme applied to the plots
#' @importFrom ggplot2 theme_bw
#' @importFrom manipulate checkbox manipulate slider 
#' @export
#' 
demo_survival <- function(
  t = 1:100, alpha.0 = -1.15, bz0.0 = -0.88, bz1.0 = -2.77, 
  theme = ggplot2::theme_bw()
)
{
  # Just to let R CMD Check not complain
  alpha <- bz0 <- bz1 <- span.alpha <- span.bz0 <- span.bz1 <- NULL

  #t=1:100;alpha.0=-1.15;bz0.0=-0.88;bz1.0=-2.77

  manipulate::manipulate(
    multiplot_survival(
      t, alpha, bz0, bz1, span = c(span.alpha, span.bz0, span.bz1), theme
    ),
    alpha = manipulate::slider(-10, 10, alpha.0, step = 0.1),
    bz0 = manipulate::slider(-5, 5, bz0.0, step = 0.1),
    bz1 = manipulate::slider(-5, 0, bz1.0, step = 0.1),
    span.alpha = manipulate::checkbox(TRUE, "span alpha"),
    span.bz0 = manipulate::checkbox(FALSE, "span bz0"),
    span.bz1 = manipulate::checkbox(FALSE, "span bz1")
  )
}

# multiplot_survival -----------------------------------------------------------

#' Plot several Survival Curves
#' 
#' @param t vector of times for which to calculate the survival curves
#' @param alpha parameter alpha of the survival function
#' @param bz0 parameter bz0 of the survival function
#' @param bz1 parameter bz1 of the survival function
#' @param span vector of logical of length three indicating for each of the 
#'   parameters \code{alpha}, \code{bz0}, \code{bz1} if they are to be varied or
#'   to be kept fix
#' @param theme ggplot2-theme applied to the plots
#' @importFrom ggplot2 aes_string geom_line geom_point ggplot theme_bw labs  
#' @importFrom kwb.utils selectColumns
multiplot_survival <- function(
  t, alpha = 0, bz0 = 0, bz1 = 0, span = rep(TRUE, 3), 
  theme = ggplot2::theme_bw()
)
{
  #t=1:100;alpha=0;bz0=0;bz1=0;span=rep(TRUE, 3)

  data <- multicall(
    standard_survival,
    alpha = spanIf(span[1], alpha),
    t = t,
    bz1 = spanIf(span[3], bz1),
    bz0 = spanIf(span[2], bz0)
  )

  # Helper function to convert a safely selected column to a factor
  to_factor <- function(x, col) factor(kwb.utils::selectColumns(data, col))

  data$bz1 <- to_factor(data, "bz1")
  data$bz0 <- to_factor(data, "bz0")
  data$alpha <- to_factor(data, "alpha")

  ggplot2::ggplot(
    data,
    ggplot2::aes_string("t", "y", group = "group", color = "bz1")
  ) +
    ggplot2::geom_line(ggplot2::aes_string(linetype = "bz0")) +
    ggplot2::geom_point(ggplot2::aes_string(shape = "alpha")) +
    y_axis_percent(limits = c(0, 1)) +
    ggplot2::labs(y = "Condition Probabilities in %", x = "Age in Years") +
    theme
}

# spanIf -----------------------------------------------------------------------

#' Span a Vector around x if Condition is met
#'
#' @param condition logical. If \code{TRUE} a vector around \code{x} is spanned
#' @param x value around which to span a vector (or not if \code{condition} is
#' \code{FALSE})
#' @param digits number of digits to which the values are rounded
#' 
spanIf <- function(condition, x, digits = 4)
{
  values <- if (condition) {
    x * (1 + 0.5 * seq(-0.2, 0.2, 0.1))
  } else {
    x
  }
  
  round(values, digits)
}

# multicall --------------------------------------------------------------------

#' Call a Function with all Combinations of Argument Ranges
#' 
#' Call a function with all possible combinations of argument ranges
#' 
#' @param FUN function to be called
#' @param ... scalars or vectors named as the arguments that are accepted by 
#'   \code{FUN}. From all objects with a name that is not in \code{fix.} 
#'   combinations of their values are created and \code{FUN} is called with each
#'   of these combinations.
#' @param fix. names of arguments to be kept constant
#' @param max.combinations maximum number of argument combinations to be created
#'   at maximum
#'   
#' @examples
#' bmi <- function(mass, height) round(mass / (height * height), 1)
#' 
#' kwbGompitz:::multicall(
#'   bmi, mass = 60:70, height = seq(1.7, 1.8, 0.1), fix. = NULL
#' )
#' 
multicall <- function(
  FUN, ..., fix. = names(formals(FUN))[1], max.combinations = 1000
)
{
  args <- list(...)

  # We expect all arguments to be named
  if (any(kwb.utils::is.unnamed(args))) {

    stop_("All arguments must be named!")
  }

  is_declared <- names(args) %in% names(formals(FUN))

  if (! all(is_declared)) {

    stop_(sprintf(
      "The following arguments are not declared by %s: %s",
      deparse(substitute(FUN)),
      kwb.utils::stringList(names(args)[! is_declared])
    ))
  }

  # Create all combinations of the values given in the arguments, except those
  # to be kept "fix"
  args_expand <- args[setdiff(names(args), fix.)]

  # Create all combinations of the values given in the arguments
  if (prod(sapply(args_expand, length)) > max.combinations) {

    stop_(
      "The number of combinations of values in ",
      kwb.utils::stringList(names(args_expand)), " exceeds ",
      max.combinations, ". Decrease the number of values in these vectors ",
      "or increase max.combinations if required."
    )
  }

  combinations <- do.call(expand.grid, args_expand)

  # Call the function with each parameter combination
  kwb.utils::rbindAll(lapply(seq_len(nrow(combinations)), function(i) {

    args_mixed <- c(args[fix.], combinations[i, ])
    args <- c(args_mixed, list(group = i, y = do.call(FUN, args_mixed)))
    do.call(data.frame, args)
  }))
}

# parameterplot ----------------------------------------------------------------

#' Plot multiple Survival Curves
#'
#' @param t vector of times for which to calculate the survival curves
#' @param alpha vector of parameters alpha to be passed to the survival function
#' @param bz0 vector of parameters bz0 to be passed to the survival function
#' @param bz1 vector of parameters bz1 to be passed to the survival function
#' @param xlim limits of the x axis
#' 
parameterplot <- function(
  t, alpha = 0, bz0 = 0, bz1 = 0, xlim = c(0, 1.2 * max(t))
)
{
  #t = 0:100; alpha = -4:0; bz1 = -5:5; bz0=-5; xlim = c(0, 1.2 * max(t))

  # Set left margin
  margins <- graphics::par("mar")
  margins[2] <- 6
  par.now <- graphics::par(mar = margins)
  on.exit(graphics::par(par.now))

  # Start with an empty plot
  graphics::plot(
    t, rep(NA, length(t)), xlim = xlim, ylim = c(0, 1),
    ylab = expression(exp(-exp(alpha + bz[0] + t * e^bz[1])))
  )

  # Count the values in each parameter vector
  parameters <- list(alpha = alpha, bz0 = bz0, bz1 = bz1)
  lengths <- sapply(parameters, length)

  if (! any(lengths == 1)) {
    
    stop_("At least one parameter must be kept constant. ")
  }

  # Order the parameter vectors by their lengths
  parameters <- parameters[order(lengths)]

  plotpars <- c("", "col", "pch")

  for (p2 in seq_along(parameters[[2]])) {

    for (p3 in seq_along(parameters[[3]])) {
      #p2=1;p3=1
      args <- parameters
      args[[2]] <- args[[2]][p2]
      args[[3]] <- args[[3]][p3]

      y <- kwb.utils::callWith(standard_survival, args, t = t)

      args <- structure(list(p3, p2), names = c(plotpars[c(3, 2)]))
      args <- kwb.utils::arglist(args, x = t, y = y, cex = 0.8)

      do.call(graphics::lines, args)
      do.call(graphics::points, args)
    }
  }

  parNames <- names(parameters)

  main <- addAssignment("", parNames[1], parameters[[1]])

  # Add two legends at maxiimum
  positions <- c("", "bottomright", "topright")

  for (i in c(3, 2)) {

    if (length(parameters[[i]]) > 1) {

      args <- list(
        x = positions[i],
        legend = parameters[[i]],
        title = parNames[i],
        seq_along(parameters[[i]]) # value for pch, col, etc.
      )

      names(args)[length(args)] <- plotpars[i]
      if (! any(names(args) == "lty")) {
        args <- c(args, lty = 1)
      }
      do.call(graphics::legend, args)
    }
    else {
      main <- addAssignment(main, parNames[i], parameters[[i]])
    }
  }

  graphics::title(main, cex.main = 1)
}

# addAssignment ----------------------------------------------------------------

#' Append assignment "a=b"
#'
#' @param main current assignment string
#' @param a key of the assignment
#' @param b value of the assignment
#' @return \code{main} with \code{a = b} appended with a comma as separator if
#' \code{main} is not empty
#' 
addAssignment <- function(main = "", a, b)
{
  paste0(main, ifelse(main == "", "", ", "), a, " = ", b)
}
