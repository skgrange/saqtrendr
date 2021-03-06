% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decompose_with_stlplus.R
\name{decompose_with_stlplus}
\alias{decompose_with_stlplus}
\title{Function to classically decompose a monthly time series with \code{stlplus}.}
\usage{
decompose_with_stlplus(df, window = 35, na_preserve = TRUE)
}
\arguments{
\item{df}{Data frame containing time series observations. The resolution 
must be monthly and this will be tested for.}

\item{window}{Span (in lags) of the loess window for seasonal extraction. 
This should be an odd number.}

\item{na_preserve}{Should observations which were missing with the values
also be propagated into the the deseasonalised components?}
}
\value{
Tibble.
}
\description{
Function to classically decompose a monthly time series with \code{stlplus}.
}
\author{
Stuart K. Grange
}
