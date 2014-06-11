\name{optionValue}
\alias{optionValue}
\title{Option Value}
\usage{
optionValue(option, method = c("Binomial", "Black-Scholes"), N = 20, ...)
}
\arguments{
  \item{option}{an \code{option} object created with
  \code{\link{optionSpec}}}

  \item{method}{the method used to value the option}

  \item{N}{number of steps in binomial tree}

  \item{\dots}{any other passthrough parameters}
}
\value{
the estimated value of the option
}
\description{
Estimate the value of an option
}
\author{
Ross Bennett
}

