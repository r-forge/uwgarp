\name{bootMean}
\alias{bootMean}
\title{Bootstrap Mean}
\usage{
  bootMean(R, ..., replications = 1000)
}
\arguments{
  \item{R}{xts object or matrix of asset returns}

  \item{\dots}{passthrough parameters to
  \code{\link[base]{mean}}}

  \item{replications}{number of bootstrap replications.}
}
\description{
  Bootstrap the mean of an xts object or matrix of asset
  returns
}
\examples{
data(crsp_weekly)
R <- largecap_weekly[,1:4]
bootMean(R[,1])
bootMean(R)
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link[base]{mean}}
}

