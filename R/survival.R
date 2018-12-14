#' @useDynLib kwbGompitz, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

# test_marginal_survival -------------------------------------------------------
test_marginal_survival <- function(t = 0:99)
{
  calibration <- exampleCalibration()
  param <- get_survival_parameters(calibration, "Cast Iron")
  
  # Generate random parameter combinations "around" param
  args <- sapply(param, stats::rnorm, n = 10000)
  
  indices <- seq_len(nrow(args))
  
  r <- list()
  
  for (v in 1:3) {
    
    duration <- system.time(r[[v]] <- lapply(indices, function(i) {
      kwb.utils::callWith(
        marginal_survival, as.list(args[i, ]), t = t, version = v
      )
    }))
    
    print(duration)
  }
  
  kwb.utils::printIf(TRUE, .max_diff_range(r[[1]], r[[2]]))
  kwb.utils::printIf(TRUE, .max_diff_range(r[[1]], r[[3]]))
}

# .max_diff_range --------------------------------------------------------------
.max_diff_range <- function(a, b) 
{
  range(sapply(seq_along(a), function(i) range(abs(a[[i]] - b[[i]]))))    
}

#
# These functions have been adapted from the C programming code in the source
# code of Gompitz
#

# marginal_survival ------------------------------------------------------------

#' Condition state marginal survival function surv(j,t,theta,Z)
#'
#' alpha = theta[cond]
#' Integr P{Y(t) <= j | theta, Z, IFF} . phi(IFF) . dIFF
#' Gauss-Legendre Integration on [a,b]
#'
#' @param alpha parameter "alpha"
#' @param t time
#' @param bz0 parameter "bz0"
#' @param bz1 parameter "bz1"
#' @param s standard deviation?
#' @param version 1, 2, or 3
#' 
marginal_survival <- function(t, alpha, bz0, bz1, s, version = 1)
{
  if (version == 1) {

    marginal_survival_v1(t, alpha, bz0, bz1, s)

  } else if (version == 2) {

    marginal_survival_v2(t, alpha, bz0, bz1, s)
    
  } else if (version == 3) {
    
    c_marginal_survival(alpha, t, bz1, bz0, s)
    
  } else {
    
    stop_("Not implemented: version = ", version)
  }
}

# marginal_survival_v1 ---------------------------------------------------------
marginal_survival_v1 <- function(t, alpha, bz0, bz1, s)
{
  x <- c(.1488743389, .4333953941, .6794095682, .8650633666, .9739065285)
  w <- c(.2955242247, .2692667193, .2190863625, .1494513491, .0666713443)

  xr <- 2.5 * s
  
  total <- double(length(t))
  
  for (i in seq_along(x)) {
    
    dx <- xr * x[i]
  
    # Density of Normal distribution (= dnorm(x, mean = 0, sd = sd))
    phi <- exp(- dx * dx / (2 * s * s)) / (SQRT_2_PI * s)

    total <- total + w[i] * phi * (
      standard_survival(alpha, t, bz1 + dx, bz0) +
      standard_survival(alpha, t, bz1 - dx, bz0)
    )
  }
  
  kwb.utils::limitToRange(xr * total)
}

#' Mathematical Constant \code{sqrt(2 * pi)}
SQRT_2_PI <- sqrt(2 * pi)

# marginal_survival_v2 ---------------------------------------------------------
marginal_survival_v2 <- function(t, alpha, bz0, bz1, s)
{
  xr <- 2.5 * s
  
  x <- c(.1488743389, .4333953941, .6794095682, .8650633666, .9739065285) * xr
  w <- c(.2955242247, .2692667193, .2190863625, .1494513491, .0666713443)
  
  total <- double(length(t))
  
  for (i in seq_along(x)) {
    
    dx <- .subset(x, i)
    
    # Density of Normal distribution (= dnorm(x, mean = 0, sd = sd))
    phi <- exp(- dx * dx / (2 * s * s)) / (SQRT_2_PI * s)
    
    total <- total + .subset(w, i) * phi * (
      standard_survival(alpha, t, bz1 + dx, bz0) +
        standard_survival(alpha, t, bz1 - dx, bz0)
    )
  }
  
  kwb.utils::limitToRange(xr * total)
}

# standard_survival ------------------------------------------------------------

#' Weibull condition state survival function
#'
#' @param alpha parameter "alpha"
#' @param t time
#' @param bz1 parameter "bz1"
#' @param bz0 parameter "bz0"
#' @param offset in order not to recalculate the following expression each time
#'   again, its result can be given here: \code{= bz0 + t * exp(bz1)}. If given,
#'   the arguments \code{t, bz0, and bz1} can be omitted, otherwise they are
#'   required and the offset is calculated according to the above expression.
#' @param limits numeric vector of two elements giving the minimum and maximum
#'   value to which the result shall be restricted. If not given or \code{NULL}
#'   the result will not be restricted to a value range.
#'
standard_survival <- function(
  alpha, t, bz1, bz0 = 0, offset = t * exp(bz1) + bz0, limits = NULL
)
{
  result <- exp(- exp(alpha + offset))

  if (! is.null(limits)) {

    result <- kwb.utils::limitToRange(result, limits[1], limits[2])
  }

  result
}

# standard_survival_c ----------------------------------------------------------
standard_survival_c <- function(
  alpha, t, bz1, bz0 = 0, offset = t * exp(bz1) + bz0, limits = NULL
)
{
  if (is.null(limits)) {
    
    c_survival(alpha, offset)
    
  } else {
    
    c_survival_limit(alpha, offset, limits)
  }
}
