\name{bootVaR}
\alias{bootVaR}
\title{Bootstrap Value at Risk}
\usage{
  bootVaR(R, ..., replications = 1000)
}
\arguments{
  \item{R}{xts object or matrix of asset returns}

  \item{\dots}{passthrough parameters to
  \code{\link[PerformanceAnalytics]{VaR}}}

  \item{replications}{number of bootstrap replications.}
}
\description{
  Bootstrap the Value at Risk (VaR) of an xts object or
  matrix of asset returns
}
\examples{
data(crsp_weekly)
R <- largecap_weekly[,1:4]
bootVaR(R[,1], p=0.9, method="historical")
bootVaR(R[,1], p=0.9, method="gaussian")
bootVaR(R, p=0.9, method="historical", invert=FALSE)
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link[PerformanceAnalytics]{VaR}}
}

