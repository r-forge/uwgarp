\name{minVarPortfolio}
\alias{minVarPortfolio}
\title{Minimum Variance Portfolio}
\usage{
  minVarPortfolio(ef)
}
\arguments{
  \item{ef}{efficient frontier object created via
  \code{efficientFrontierTwoAsset}}
}
\description{
  Extract the minimum variance portfolio from an efficient
  frontier
}
\details{
  Note that the values for the minimum variance portfolio
  are an approximation and depend on the number of
  portfolios used to create the efficient frontier.
}

