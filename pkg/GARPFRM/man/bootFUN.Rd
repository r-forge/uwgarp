\name{bootFUN}
\alias{bootFUN}
\title{Bootstrap}
\usage{
  bootFUN(R, FUN = "mean", ..., replications = 1000,
    parallel = FALSE)
}
\arguments{
  \item{R}{xts object or matrix of data passed to
  \code{FUN}.}

  \item{FUN}{the function to be applied.}

  \item{\dots}{optional arguments to \code{FUN}.}

  \item{replications}{number of bootstrap replications.}

  \item{parallel}{(default FALSE) to compute the bootstrap
  in parallel.}
}
\description{
  Bootstrap a function
}
\details{
  \code{R} is the data passed to \code{FUN}. \code{FUN}
  must have \code{x} or \code{R} as arguments for the data.
  For example, see the functions linked to in the 'See
  Also' section. Care must be taken when using
  \code{bootFUN} on multivariate data. This function is
  designed to only accept univariate (i.e. ncol(R) = 1)
  data, however is made to work with bivariate data for
  \code{bootCor} and \code{bootCov}. For multivariate data,
  a wrapper function should be written to apply the
  bootstrap function to each column of data.

  To run the bootstrap in parallael, this function uses the
  \code{foreach} pacakge. According to the
  \code{\link[foreach]{foreach}} documentation, the
  parallel computation depends upon a parallel backend that
  must be registered before performing the computation. The
  parallel backends available will be system-specific, but
  include \code{doParallel}, which uses R's built-in
  parallel package, \code{doMC}, which uses the multicore
  package, and \code{doSNOW}. Each parallel backend has a
  specific registration function, such as
  \code{registerDoParallel} or \code{registerDoSNOW}.
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{bootMean}}, \code{\link{bootSD}},
  \code{\link{bootStdDev}},
  \code{\link{bootSimpleVolatility}},
  \code{\link{bootCor}}, \code{\link{bootCov}},
  \code{\link{bootVaR}}, \code{\link{bootES}},
  \code{\link[foreach]{foreach}}
}

