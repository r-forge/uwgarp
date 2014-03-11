\name{uvEWMAcor}
\alias{uvEWMAcor}
\title{EWMA Correlation Estimate}
\usage{
  uvEWMAcor(R, lambda = 0.94, initialWindow = 10)
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{lambda}{smoothing parameter, must be greater than 0
  or less than 1}

  \item{initialWindow}{initial window of observations used
  in estimating the initial conditions}
}
\description{
  EWMA model to estimate correlation
}

