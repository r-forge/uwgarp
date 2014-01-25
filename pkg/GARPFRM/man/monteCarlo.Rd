\name{monteCarlo}
\alias{monteCarlo}
\title{Monte Carlo Price Path Simulation}
\usage{
  monteCarlo(mu, sigma, N = 100, Time = 1, steps = 52,
    starting_value = 100, log = TRUE)
}
\arguments{
  \item{mu}{annualized expected return}

  \item{sigma}{annualized standard deviation}

  \item{N}{number of simulations}

  \item{Time}{length of simulation (in years)}

  \item{steps}{number of time steps}

  \item{starting_value}{price to start at}

  \item{log}{TRUE/FALSE (default = TRUE) simulate ln(P)
  rather than S; where S is the price of the asset.}
}
\value{
  matrix of Monte Carlo simulated price paths
}
\description{
  Description for Monte Carlo. Geometric brownian motion.
  mu and sigma are assumed constant
}

