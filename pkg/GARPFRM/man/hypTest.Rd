\name{hypTest}
\alias{hypTest}
\title{CAPM Hypothesis Test}
\usage{
  hypTest(object, CI)
}
\arguments{
  \item{object}{a capm object created by
  \code{\link{CAPM}}}

  \item{CI}{confidence level}
}
\value{
  TRUE/FALSE if the null hypothesis is rejected
}
\description{
  Test the CAPM coefficients for significance.
}
\details{
  TODO: We need to clearly define the null hypothesis here.
  This test if the coefficients are significantly different
  from 0. The null hypothesis is that the coefficients are
  equal to 0. If the p-value is less than the specified
  confidence level, the null hypothesis is rejected meaning
  that the coefficients are significantly different from 0.
  If the p-value is greater than the specified confidence
  level, the null hypothesis cannot be rejected.
}

