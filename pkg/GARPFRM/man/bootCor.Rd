\name{bootCor}
\alias{bootCor}
\title{Bootstrap Correlation}
\usage{
bootCor(R, ..., replications = 1000, parallel = FALSE)
}
\arguments{
  \item{R}{xts object or matrix of asset returns}

  \item{\dots}{passthrough parameters to
  \code{\link[stats]{cor}}}

  \item{replications}{number of bootstrap replications.}

  \item{parallel}{TRUE/FALSE (default FALSE) to compute the
  bootstrap in parallel.}
}
\description{
Bootstrap the correlation of an xts object or matrix of
asset returns
}
\examples{
data(crsp_weekly)
R <- largecap_weekly[,1:4]
bootCor(R[,1:2])
bootCor(R[,1:2], method="kendall")
bootCor(R)
}
\author{
Ross Bennett
}
\seealso{
\code{\link[stats]{cor}}
}

