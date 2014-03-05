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

