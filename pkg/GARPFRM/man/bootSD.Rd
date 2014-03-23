\name{bootSD}
\alias{bootSD}
\title{Bootstrap Standard Deviation}
\usage{
  bootSD(R, ..., replications = 1000, parallel = FALSE)
}
\arguments{
  \item{R}{xts object or matrix of asset returns}

  \item{\dots}{passthrough parameters to
  \code{\link[stats]{sd}}}

  \item{replications}{number of bootstrap replications.}

  \item{parallel}{TRUE/FALSE (default FALSE) to compute the
  bootstrap in parallel.}
}
\description{
  Bootstrap the standard deviation of an xts object of
  asset returns
}
\author{
  Ross Bennett
}

