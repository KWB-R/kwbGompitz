% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gompred.R
\name{runGompitzPrediction}
\alias{runGompitzPrediction}
\title{Run Gompitz Predicion}
\usage{
runGompitzPrediction(
  input.data,
  subset = NULL,
  calibration,
  strategy = 0,
  ...,
  verbose = 1,
  do.stop = TRUE,
  clear.observations = TRUE,
  VERSION = getOperatingSystemType(),
  use.data.table = getOption("kwbGompitz.use.data.table", FALSE)
)
}
\arguments{
\item{input.data}{prepared input data as retrieved by
\code{\link{composeGompitzInputData}}}

\item{subset}{indexes of rows in \code{input.data} to be used for prediction.
if \code{NULL}, all rows in \code{input.data} will be used.}

\item{calibration}{result of Gompitz calibration as retrieved by 
\code{\link{runGompitzCalibration}}}

\item{strategy}{strategy identifier. Must be one of 0 (do nothing), 1
(length-driven strategy), 2 (budget-driven strategy), or 3 (optimised
strategy).")}

\item{\dots}{arguments passed to the corresponding
\code{.fileContentStrategy} functions, such as \code{range.years}
(two-element vector with first and last year of prediction),
rehabilitation.costs (needed for strategy = 1 or 2 or 3, see
\code{kwbGompitz:::.fileContentStrategy1} or 
\code{kwb.gomiptz:::.fileContentStrategy2} or 
\code{kwb.gomiptz:::.fileContentStrategy3}), annual.total.length (needed 
for strategy = 1, see \code{kwbGompitz:::.fileContentStrategy1}), 
annual.total.budget (needed for strategy = 2, see 
\code{kwb.gomiptz:::.fileContentStrategy2}), max.tol.prop.of.length (needed
for strategy = 3, see \code{kwb.gomiptz:::.fileContentStrategy3}), 
target.year (needed for strategy = 3, see 
\code{kwb.gomiptz:::.fileContentStrategy3})}

\item{verbose}{verbosity level, default: 1}

\item{do.stop}{if \code{TRUE} the program stops in case of inconsistencies}

\item{clear.observations}{if \code{TRUE} (default) the columns containing the
inspection year and the observed condition class, respectively, are cleared
in the input file given to \code{gompred}. Otherwise the observed condition
classes are kept in the input file and thus considered by \code{gompred}.}

\item{VERSION}{name of subdirectory in package containing the binary files to
be executed. Possible values: "unix", "win32", "win32_kwb"}

\item{use.data.table}{if \code{TRUE}, \code{\link[data.table]{fread}} is used
to read the result file}
}
\description{
Run Gompitz Predicion
}
\examples{
# For an example, see the Tutorial vignette "How to Use the Package"
}
