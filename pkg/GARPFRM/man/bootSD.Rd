\name{bootSD}
\alias{bootSD}
\title{Bootstrap Standard Deviation}
\usage{
bootSD(R, ..., replications = 1000)
}
\arguments{
  \item{R}{xts object or matrix of asset returns}

  \item{\dots}{passthrough parameters to
  \code{\link[stats]{sd}}}

  \item{replications}{number of bootstrap replications.}
}
\description{
Bootstrap the standard deviation of an xts object or matrix
of asset returns
}
\examples{
data(crsp_weekly)
R <- largecap_weekly[,1:4]
bootSD(R[,1])
bootSD(R)
}
\author{
Ross Bennett
}
\seealso{
\code{\link[stats]{sd}}
}

