\name{bondFullPrice}
\alias{bondFullPrice}
\title{Estimate price of bond w/ acrrued interest}
\usage{
bondFullPrice(bond, yield, cashFlowPd, t0, t1, currentDate)
}
\arguments{
  \item{bond}{is a bondSpec object}

  \item{yield}{is the yield on the bond}

  \item{cashFlowPd}{cash flow period}

  \item{t0}{previous coupon date}

  \item{t1}{next coupon period}

  \item{currentDate}{current date}
}
\value{
price of the bond: clean, dirty and accrued interest
}
\description{
This function calculates the price of a fixed rate coupon
bond given coupon rate, yield, compoundPd, cashFlowPd, face
value, previous coupon date, next coupon date.
}
\author{
TF
}

