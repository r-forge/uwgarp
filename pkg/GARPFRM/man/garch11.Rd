\name{garch11}
\alias{garch11}
\title{GARCH Models}
\usage{
  garch11(R, model = "sGARCH", distribution.model = "norm")
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{model}{“sGARCH”, “fGARCH”, “eGARCH”, “gjrGARCH”,
  “apARCH” and “iGARCH” and “csGARCH”}

  \item{distribution.model.}{Valid choices are “norm” for
  the normal distibution, “snorm” for the skew-normal
  distribution, “std” for the student-t, “sstd” for the
  skew-student, “ged” for the generalized error
  distribution, “sged” for the skew-generalized error
  distribution, “nig” for the normal inverse gaussian
  distribution, “ghyp” for the Generalized Hyperbolic, and
  “jsu” for Johnson's SU distribution.}
}
\description{
  This function is a basic wrapper of functions in the
  rugarch and rmgarch packages to specify and fit GARCH
  models. The rugarch and rmgarch packages provide
  functions to specify and fit a rich set of GARCH models.
  The purpose of this function is to specify and fit a
  GARCH model while abstracting away some complexities.
}
\details{
  The rugarch package implements univariate garch models
  and the rmgarch package implements multivariate garch
  models. Univariate or multivariate data is automatically
  detected and the appropriate GARCH model will be
  specified and fit.

  For complete functionality of GARCH models, it is
  recommended to directly use functions in the rugarch and
  rmgarch packages.
}

