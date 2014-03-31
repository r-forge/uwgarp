\name{forecast}
\alias{forecast}
\title{Model Forecasting}
\usage{
  forecast(model, nAhead, ...)
}
\arguments{
  \item{model}{a model object}

  \item{nAhead}{number of periods ahead to forecast}

  \item{\dots}{any other passthrough parameters}
}
\value{
  forecasted value from model
}
\description{
  Generic method for forecasting from EWMA and GARCH models
}
\author{
  Ross Bennett
}

