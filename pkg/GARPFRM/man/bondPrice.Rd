\name{bondPrice}
\alias{bondPrice}
\title{Estimate price of bond}
\usage{
bondPrice(bond, discountCurve)
}
\arguments{
  \item{bond}{a \code{discountFactorArbitrage} object}

  \item{discountCurve}{vector of discount rates}
}
\value{
price of the bond
}
\description{
This function calculates the price of a fixed rate coupon
bond given the discount curve and bond data. First it
converts the discountCurve into CF
}
\author{
TF
}

