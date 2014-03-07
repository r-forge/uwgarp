\name{uvGARCH}
\alias{uvGARCH}
\title{Univariate GARCH Model}
\usage{
  uvGARCH(R, model = "sGARCH", garchOrder = c(1, 1),
    armaOrder = c(1, 1), distribution = "norm",
    fixedParams = NULL, solver = "hybrid", outSample = 0,
    fitControl = NULL, solverControl = NULL)
}
\arguments{
  \item{R}{xts object of asset returns.}

  \item{model}{GARCH Model to specify and fit. Valid GARCH
  models are “sGARCH”, “fGARCH”, “eGARCH”, “gjrGARCH”,
  “apARCH”, “iGARCH” and “csGARCH”.}

  \item{garchOrder}{the ARCH(q) and GARCH(p) orders.}

  \item{armaOrder}{the autoregressive and moving average
  orders.}

  \item{distribution}{conditional density to use for the
  innovations. Valid distributions are “norm” for the
  normal distibution, “snorm” for the skew-normal
  distribution, “std” for the student-t, “sstd” for the
  skew-student, “ged” for the generalized error
  distribution, “sged” for the skew-generalized error
  distribution, “nig” for the normal inverse gaussian
  distribution, “ghyp” for the Generalized Hyperbolic, and
  “jsu” for Johnson's SU distribution.}

  \item{fixedParams}{named list of parameters to keep
  fixed.}

  \item{solver}{the solver to use to fit the GARCH model.
  Valid solvers are “nlminb”, “solnp”, “lbfgs”, “gosolnp”,
  “nloptr” or “hybrid”}

  \item{outSample}{number of periods of data used to fit
  the model. \code{nrow(R) - outSample} number of periods
  to keep as out of sample data points.}

  \item{fitControl}{named list of arguments for the fitting
  routine}

  \item{solverControl}{named list of arguments for the
  solver}
}
\value{
  a list of length two containing GARCH specification and
  GARCH fit objects
}
\description{
  Specify and fit a univariate GARCH model
}

