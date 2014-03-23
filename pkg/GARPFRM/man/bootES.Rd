\name{bootES}
\alias{bootES}
\title{Bootstrap Expected Shortfall}
\usage{
  bootES(R, ..., replications = 1000, parallel = FALSE)
}
\arguments{
  \item{R}{xts object or matrix of asset returns}

  \item{\dots}{passthrough parameters to
  \code{\link[PerformanceAnalytics]{ES}}}

  \item{replications}{number of bootstrap replications.}

  \item{parallel}{TRUE/FALSE (default FALSE) to compute the
  bootstrap in parallel.}
}
\description{
  Bootstrap the Expected Shortfall (ES) of an xts object of
  asset returns
}
\author{
  Ross Bennett
}

