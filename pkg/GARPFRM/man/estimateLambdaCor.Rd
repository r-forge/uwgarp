\name{estimateLambdaCor}
\alias{estimateLambdaCor}
\title{Estimated Lambda}
\usage{
  estimateLambdaCor(R, initialWindow = 10, n = 10)
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{initialWindow}{initial window of observations used
  in estimating the initial}

  \item{n}{number of periods used to calculate realized
  correlation}
}
\description{
  Estimate lambda for EWMA correlation estimate
}

