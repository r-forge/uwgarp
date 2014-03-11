\name{estimateLambdaVol}
\alias{estimateLambdaVol}
\title{Estimated Lambda}
\usage{
  estimateLambdaVol(R, initialWindow = 10, n = 10)
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{initialWindow}{initial window of observations used
  in estimating the initial}

  \item{n}{number of periods used to calculate realized
  volatility}
}
\description{
  Estimate lambda for EWMA volatility estimate
}

