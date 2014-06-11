\name{bondSpec}
\alias{bondSpec}
\title{Constructor for bond specification}
\usage{
bondSpec(time = seq(from = 0.5, to = 2, by = 0.5), face = 100, m = 2,
  couponRate = 0.01)
}
\arguments{
  \item{time}{vector of sequence of coupon payments in
  years}

  \item{face}{face value of bond}

  \item{m}{compounding frequency}

  \item{couponRate}{rate the coupon pays}
}
\value{
a \code{bond} object with the bond data used for pricing
}
\description{
Created a bond object \code{bond.spec} with data for bond
specification.
}
\author{
TF
}

