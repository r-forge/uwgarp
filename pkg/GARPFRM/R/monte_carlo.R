# Monte Carlo Function
generateMC <- function(mu, sigma, Time=1, steps=52, starting_value=100){
  dt <- Time / steps
  S <- vector("numeric", steps)
  S[1] <- starting_value
  for(i in 2:length(S)){
    dS <- mu * dt + sigma * rnorm(1) * sqrt(dt)
    S[i] <- dS + S[i-1]
  }
  return(S)
}

# Monte Carlo using ln(S) rather than S
# more accurate
generateLogMC <- function(mu, sigma, Time=1, steps=52, starting_value=100){
  dt <- Time / steps
  S <- vector("numeric", steps)
  S[1] <- starting_value
  for(i in 2:length(S)){
    S[i] <- S[i-1] * exp((mu - (sigma^2 / 2)) * dt + sigma * rnorm(1) * sqrt(dt))
  }
  return(S)
}

#' Monte Carlo Price Path Simulation
#' 
#' Description for Monte Carlo. Geometric brownian motion. mu and sigma are assumed constant
#' 
#' @param mu annualized expected return
#' @param sigma annualized standard deviation
#' @param N number of simulations
#' @param Time length of simulation (in years)
#' @param steps number of time steps
#' @param starting_value price to start at
#' @param log TRUE/FALSE (default = TRUE) simulate ln(P) rather than S; where S 
#' is the price of the asset.
#' @return matrix of Monte Carlo simulated price paths
monteCarlo <- function(mu, sigma, N=100, Time=1, steps=52, starting_value=100, log=TRUE){
  mc_mat <- matrix(0, N, steps)
  if(log){
    for(i in 1:N){
      mc_mat[i,] <- generateLogMC(mu, sigma, Time, steps, starting_value)
    }
  } else {
    for(i in 1:N){
      mc_mat[i,] <- generateMC(mu, sigma, Time, steps, starting_value)
    }
  }
  class(mc_mat) <- "monte_carlo"
  return(mc_mat)
}

plot.monte_carlo <-function(x, y, ..., main="Monte Carlo Simulation", xlab="Time Index", ylab="Price"){
  plot(x[1,], type="n", ylim=range(x), main=main, xlab=xlab, ylab=ylab)
  for(i in 1:nrow(x)){
    lines(x[i,])
  }
}

#' Ending Prices of Monte Carlo Simulation
#' 
endingPrices <- function(mc){
  mc[, ncol(mc)]
}

plotEndingPrices <- function(mc){
  ending_prices <- endingPrices(mc)
  dens_ep <- density(ending_prices)
  hist(ending_prices, freq=FALSE)
  lines(dens_ep)
  invisible(list(ending_prices=ending_prices, density=dens_ep))
}
