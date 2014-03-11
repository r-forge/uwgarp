\name{EWMA}
\alias{EWMA}
\title{EWMA Model}
\usage{
  EWMA(R, lambda = 0.94, initialWindow = 10, n = 10,
    type = c("volatility", "covariance", "correlation"))
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{lambda}{smoothing parameter, must be greater than 0
  or less than 1. If NULL, lambda will be estimated by
  minimizing the sum of squared difference between the
  estimated value and the realized value.}

  \item{initialWindow}{initial window of observations used
  in estimating the initial conditions}

  \item{n}{number of periods used to calculate realized
  volatility, covariance, or correlation.}

  \item{type}{estimate volatility, covariance, or
  correlation.}
}
\description{
  EWMA model to estimate volatility, covariance, and
  correlation
}
\details{
  If lambda=NULL, the lambda value can be estimated for
  univariate estimates of volatility, covariance, and
  correlation by minimizing the sum of squared differences
  between the estimated value and realized value.
}

