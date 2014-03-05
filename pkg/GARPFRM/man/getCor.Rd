\name{getCor}
\alias{getCor}
\title{EWMA Correlation}
\usage{
  getCor(EWMA, assets)
}
\arguments{
  \item{object}{an EWMA object created by \code{EWMA}}

  \item{assets}{character vector or numeric vector. The
  assets can be specified by name or index.}
}
\description{
  Extract the correlation of two assets from an \code{EWMA}
  object
}
\examples{
data(crsp.short)
# Use the first 5 assets in largecap.ts for the returns data
R <- largecap.ts[, 1:5]
# Estimate the correlation matrix via EWMA
corEst <- EWMA(R, 0.94, 15, TRUE)
# get the correlation between AMAT and CAT
corAMATCAT <- getCov(corEst, assets=c("AMAT", "CAT"))
cor13 <- getCov(corEst, assets=c(1, 3))
all.equal(corAMATCAT, cor13)
}

