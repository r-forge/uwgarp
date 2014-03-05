\name{EWMA}
\alias{EWMA}
\title{Exponential Weighted Moving Average (EWMA)}
\usage{
  EWMA(R, lambda = 0.94, initialWindow = 10, cor = FALSE)
}
\arguments{
  \item{R}{asset returns}

  \item{lambda}{smoothing parameter, must be greater than 0
  or less than 1}

  \item{initialWindow}{initial window of observations used
  in estimating the initial covariance or correlation}

  \item{TRUE/FALSE}{to return a correlation matrix. Default
  cor = FALSE.}
}
\description{
  Use an exponentially weighted moving average to estimate
  the covariance or correlation of asset returns.
}

