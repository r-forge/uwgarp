\name{bootSimpleVolatility}
\alias{bootSimpleVolatility}
\title{Bootstrap Simple Volatility}
\usage{
  bootSimpleVolatility(R, ..., replications = 1000,
    parallel = FALSE)
}
\arguments{
  \item{R}{xts object or matrix of asset returns}

  \item{\dots}{passthrough parameters to
  \code{\link{SimpleVolatility}}}

  \item{replications}{number of bootstrap replications.}

  \item{parallel}{TRUE/FALSE (default FALSE) to compute the
  bootstrap in parallel.}
}
\description{
  Bootstrap the simple volatility of an xts object or
  matrix of asset returns
}
\author{
  Ross Bennett
}

