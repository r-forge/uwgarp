\name{bondConvexity}
\alias{bondConvexity}
\title{Calculate the convexity of a fixed rate coupon bond}
\usage{
bondConvexity(bond, discountCurve)
}
\arguments{
  \item{bond}{a \code{bond} object in
  discountFactorArbitrage}

  \item{discountCurve}{vector of discount rates}
}
\value{
convexity of the bond
}
\description{
This function estimates the convexity of a fixed rate
coupon bond given the discount curve and bond data.
}

