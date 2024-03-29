% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compose.R
\name{composeGompitzInputData}
\alias{composeGompitzInputData}
\title{Compose Input Data for Gompitz Functions}
\usage{
composeGompitzInputData(
  masterdata,
  covariates,
  weight,
  covariates.status,
  condition.labels = NULL,
  warn = FALSE
)
}
\arguments{
\item{masterdata}{data.frame containing master data as retrieved by 
\code{\link{composeMasterData}}}

\item{covariates}{data.frame containing covariates in columns. Must have as
many rows as masterdata}

\item{weight}{weight to be given for each inspection (each row) in
\emph{masterdata}. Should be a vector of same length as there are rows in
\emph{masterdata}. Default: 1}

\item{covariates.status}{matrix of covariate status as retrieved by 
\code{\link{createStatusMatrix}}}

\item{condition.labels}{All possible condition labels, e.g. c("1", "2", "3",
"4"). Default: unique values in column "condition" of \emph{masterdata}}

\item{warn}{if TRUE (default), a warning is given if \emph{weight} does not
have the expected length}
}
\value{
list with elements \emph{masterdata} (data.frame), \emph{covariates} 
  (data.frame), \emph{weight} (numeric vector), \emph{covariates.status} 
  (numeric matrix) and \emph{condition.labels} (character vector)
}
\description{
Compose input data for Gompitz functions \code{\link{runGompitzCalibration}},
\code{\link{runGompitzPrediction}}
}
