# interpolate ------------------------------------------------------------------
interpolate <- function(y_list, interpol_info)
{
  getele <- kwb.utils::selectElements

  stopifnot(kwb.utils::allAreEqual(sapply(y_list, length)))
  
  # Save attributes before modifying heights with lapply
  attribs <- attributes(y_list)

  # Get the times to which the values in y_list are related
  t <- getele(kwb.utils::getAttribute(y_list, "args"), "t")

  # Determine start and end position
  start_index <- which(t == getele(interpol_info, "start"))

  stopifnot(length(start_index) == 1)

  stop_index <- start_index + getele(interpol_info, "length") - 1

  if (stop_index > length(y_list[[1]])) {
    
    stop_(
      "Interpolation end index (", stop_index, ") must not be greater than ",
      "the maximum possible index (", length(y_list[[1]]), ")"
    )
  }
  
  target <- getele(interpol_info, "target")

  # Interpolate values between one at the start position and the value at the
  # end position
  y_list <- lapply(seq_along(y_list), function(i) {
    y <- y_list[[i]]
    if (i >= target) {
      y_new <- seq(1, y[stop_index], length.out = stop_index - start_index + 1)
      y[start_index:stop_index] <- y_new
    } else {
      y[start_index:length(y)] <- 0
    }
    y
  })

  kwb.utils::hsRestoreAttributes(y_list, attribs)
}

# interpolate_between ----------------------------------------------------------

#' Interpolate equidistantly between End Points
#'
#' @param y1 numeric vector of y-values at the beginning
#' @param y2 numeric vector of y-values at the end. Must be as long as \code{y1}
#' @param n number of interpolation points including first and last value
#' @param version version of implementation
#'
#' @return matrix \code{M} with \code{n} columns and as many rows as there are
#'   values in \code{y1} (and also in \code{y1}). The first column contains the
#'   values from \code{y1}, the last column contains the values from \code{y2}
#'   and the \code{n - 2} columns in between contain the interpolated values.
#'
#' @export
#'
#' @examples
#' y1 <- c(1, 1, 1)
#' y2 <- c(0.1, 0.5, 0.8)
#' n <- 10
#'
#' y <- interpolate_between(y1, y2, n)
#' barplot(y, beside = TRUE)
#'
#' # Compare the performance of slightly different implementations
#' microbenchmark::microbenchmark(
#'   v1 = interpolate_between(y1, y2, n, version = 1),
#'   v2 = interpolate_between(y1, y2, n, version = 2),
#'   v3 = interpolate_between(y1, y2, n, version = 3),
#'   times = 1000,
#'   check = function(x) kwb.utils::allAreIdentical(x[2:3]) &&
#'     all(kwb.utils::almostEqual(x[[1]], x[[2]]))
#' )
#'
interpolate_between <- function(y1, y2, n = 2L, version = 3)
{
  stopifnot(is.numeric(y1), is.numeric(y2), length(y1) == length(y2), n >= 2)

  if (version == 1) {

    steps <- seq(0, 1, length.out = n)
    y1 + (y2 - y1) * matrix(ncol = n, byrow = TRUE, rep(steps, length(y1)))

  } else if (version == 2) {

    steps <- (seq_len(n) - 1) / (n - 1)
    y1 + (y2 - y1) * matrix(ncol = n, byrow = TRUE, rep(steps, length(y1)))

  } else if (version == 3) {

    steps <- (seq_len(n) - 1) / (n - 1)
    y1 + (y2 - y1) * matrix(steps, nrow = length(y1), ncol = n, byrow = TRUE)
  }
}
