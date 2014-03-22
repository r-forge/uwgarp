\name{rollCov}
\alias{rollCov}
\title{Rolling Covariance Estimate}
\usage{
  rollCov(R, width)
}
\arguments{
  \item{R}{xts or zoo object of asset returns}

  \item{width}{width of rolling window}
}
\description{
  This function calculates the covariance estimate of the
  returns of two assets over a rolling window
}
\examples{
data(crsp_weekly)
R <- largecap_weekly[,1:2]
tail(rollCov(R, 10))
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{cov}}
}

