\name{PCA}
\alias{PCA}
\title{Estimate PCA loadings and creat PCA object}
\usage{
PCA(data, nfactors, rotate = "none")
}
\arguments{
  \item{data}{time series data}

  \item{nfactors}{number of components to extract}

  \item{rotate}{"none", "varimax", "quatimax", "promax",
  "oblimin", "simplimax", and "cluster" are possible
  rotations/transformations of the solution.}
}
\value{
pca object loadings
}
\description{
This function estimates the delta for hedging a particular
bond given bond data
}

