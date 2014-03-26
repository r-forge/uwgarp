\name{forecast.uvEWMAvol}
\alias{forecast.uvEWMAvol}
\title{Forecast Univariate EWMA Volatility Model}
\usage{
  \method{forecast}{uvEWMAvol} (model, nAhead = 1, ...)
}
\arguments{
  \item{model}{EWMA model fit via \code{\link{EWMA}}}

  \item{nAhead}{number of steps ahead to forecast. (nAhead
  = 1 only supported)}

  \item{\dots}{additional passthrough parameters}

  \item{nRoll}{number of rolling forecasts}

  \item{externalForecasts}{named list of external
  regressors in the mean and/or variance equations}
}
\value{
  one period ahead EWMA volatility forecast
}
\description{
  Forecasting for EWMA Volatility models fit via
  \code{\link{EWMA}}
}

