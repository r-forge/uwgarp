\name{plot.EWMA}
\alias{plot.EWMA}
\title{Plot EWMA Model Estimates}
\usage{
  \method{plot}{EWMA} (x, y = NULL, ..., assets = c(1, 2),
    legendLoc = NULL, main = "EWMA Estimate",
    cexLegend = 0.8)
}
\arguments{
  \item{x}{an EWMA object}

  \item{y}{NULL}

  \item{\dots}{additional arguments passed to
  \code{plot.xts}}

  \item{assets}{for multivariate EWMA objects, character
  vector or numeric vector of assets to extract from the
  covariance or correlation matrix. The assets can be
  specified by name or index.}

  \item{legendLoc}{location of legend. If NULL, the legend
  will be omitted from the plot}

  \item{main}{main title for the plot}
}
\description{
  Plot method for EWMA objects
}

