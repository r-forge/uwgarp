\name{bondDuration}
\alias{bondDuration}
\title{Calculate the modified duration of a bond}
\usage{
bondDuration(bond, discountCurve, percentChangeYield = 0)
}
\arguments{
  \item{bond}{a \code{bond} object in
  discountFactorArbitrage}

  \item{discountCurve}{vector of discount rates}

  \item{percentChangeYield}{optional elasticity measure}
}
\value{
duration of the bond
}
\description{
The function estimates modified duration of a fixed rate
coupon bond given the discount curve and bond data. The
modified duration is calculated using the continuously
compounded yield
}

