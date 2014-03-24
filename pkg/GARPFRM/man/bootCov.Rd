\name{bootCov}
\alias{bootCov}
\title{Bootstrap Covariance}
\usage{
  bootCov(R, ..., replications = 1000, parallel = FALSE)
}
\arguments{
  \item{R}{xts object or matrix of asset returns}

  \item{\dots}{passthrough parameters to
  \code{\link[stats]{cov}}}

  \item{replications}{number of bootstrap replications.}

  \item{parallel}{TRUE/FALSE (default FALSE) to compute the
  bootstrap in parallel.}
}
\description{
  Bootstrap the covariance of an xts object or matrix of
  asset returns
}
\examples{
data(crsp_weekly)
R <- largecap_weekly[,1:4]
bootCov(R[,1:2])
bootCov(R)
}
\author{
  Ross Bennett
}

