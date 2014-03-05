\name{getCov}
\alias{getCov}
\title{EWMA Covariance}
\usage{
  getCov(EWMA, assets)
}
\arguments{
  \item{EWMA}{an EWMA object created by \code{EWMA}}

  \item{assets}{character vector or numeric vector. If
  \code{assets} is of length 1, then the variance will be
  returned. The assets can be specified by name or index.}
}
\description{
  Extract the covariance between two assets from an EWMA
  object
}
\examples{
data(crsp.short)
# Use the first 5 assets in largecap.ts for the returns data
R <- largecap.ts[, 1:5]
# Estimate the covariance matrix via EWMA
covEst <- EWMA(R, 0.94, 15)
# get the covariance between AMAT and CAT
covAMATCAT <- getCov(covEst, assets=c("AMAT", "CAT"))
cov13 <- getCov(covEst, assets=c(1, 3))
all.equal(covAMATCAT, cov13)
}

