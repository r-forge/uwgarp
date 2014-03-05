\name{plotEndingPrices}
\alias{plotEndingPrices}
\title{Plot Ending Prices}
\usage{
  plotEndingPrices(mc, ..., main = "Ending Prices",
    xlab = "Price", ylab = "Density")
}
\arguments{
  \item{mc}{monte carlo object created with
  \code{monteCarlo}}

  \item{\dots}{additional arguments passed to \code{hist}}
}
\description{
  Plot the kernel density estimate and histogram of the
  ending prices from a Monte Carlo simulation.
}
\examples{
library(GARPFRM)

mc <- monteCarlo(0.05, 0.25, 500, 1, 52, 10)
plotEndingPrices(mc)
}

