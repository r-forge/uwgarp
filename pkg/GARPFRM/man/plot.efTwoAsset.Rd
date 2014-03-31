\name{plot.efTwoAsset}
\alias{plot.efTwoAsset}
\title{Efficient Frontier Plot}
\usage{
  \method{plot}{efTwoAsset} (x, y, ..., chartAssets = TRUE,
    main = "Efficient Frontier", xlim = NULL, ylim = NULL,
    type = "l", pchAssets = 19, cexAssets = 0.8)
}
\arguments{
  \item{x}{object of class \code{efTwoAsset} created via
  \code{\link{efficientFrontierTwoAsset}}}

  \item{y}{not used}

  \item{\dots}{passthrough parameters}

  \item{chartAssets}{TRUE/FALSE to include the assets in
  the plot}

  \item{main}{a main title for the plot}

  \item{xlim}{set the x-axis limit, same as in \code{plot}}

  \item{ylim}{set the x-axis limit, same as in \code{plot}}

  \item{type}{what type of plot should be drawn, same as in
  \code{plot}}

  \item{pchAssets}{plotting character of the assets}

  \item{cexAssets}{numerical value giving the amount by
  which the asset points and labels should be magnified
  relative to the default.}
}
\description{
  Plot the efficient frontier in return - standard
  deviation space
}

