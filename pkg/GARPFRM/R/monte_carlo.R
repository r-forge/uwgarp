# Monte Carlo Function
generateMC <- function(mu, sigma, Time=1, steps=52, starting_value=100){
  dt <- Time / steps
  S <- vector("numeric", steps+1)
  eps <- rnorm(steps)
  S[1] <- starting_value
  for(i in 2:length(S)){
    dS <- mu * dt + sigma * eps(i-1) * sqrt(dt)
    S[i] <- dS + S[i-1]
  }
  return(S)
}

# Monte Carlo using ln(S) rather than S
# more accurate
generateLogMC <- function(mu, sigma, Time=1, steps=52, starting_value=100){
  dt <- Time / steps
  S <- vector("numeric", steps+1)
  S[1] <- starting_value
  for(i in 2:length(S)){
    S[i] <- S[i-1] * exp((mu - 0.5 * sigma^2) * dt + sigma * eps(i-1) * sqrt(dt))
  }
  return(S)
}

#' Monte Carlo Price Path Simulation
#' 
#' Run \code{N} monte carlo simulations to generate asset price paths following
#' a geometric brownian motion process.
#' 
#' TODO: add equations for GBM
#' 
#' @note This function returns a m x N matrix of simulated price paths where
#' m is the number of steps + 1 and N is the number of simulations. This can be 
#' very memory and compute intensive with a large number of steps and/or a 
#' large number of  simulations. 
#' More efficient methods in terms of speed and memory should be used, for 
#' example, to price options.
#' 
#' @param mu annualized expected return
#' @param sigma annualized standard deviation
#' @param N number of simulations
#' @param Time length of simulation (in years)
#' @param steps number of time steps
#' @param starting_value asset price starting value
#' @param log TRUE/FALSE (default = TRUE) simulate ln(S) rather than S; where S 
#' is the price of the asset.
#' @return matrix of simulated price paths where each column represents a price path
#' @export
monteCarlo <- function(mu, sigma, N=100, Time=1, steps=52, starting_value=100, log=TRUE){
  mc_mat <- matrix(0, steps, N)
  if(log){
    for(i in 1:N){
      mc_mat[,i] <- generateLogMC(mu, sigma, Time, steps, starting_value)
    }
  } else {
    for(i in 1:N){
      mc_mat[,i] <- generateMC(mu, sigma, Time, steps, starting_value)
    }
  }
  class(mc_mat) <- "monte_carlo"
  return(mc_mat)
}

plot.monte_carlo <-function(x, y, ..., main="Monte Carlo Simulation", xlab="Time Index", ylab="Price"){
  plot(x[,1], type="n", ylim=range(x), main=main, xlab=xlab, ylab=ylab, ...)
  for(i in 1:ncol(x)){
    lines(x[,i])
  }
}

#' Ending Prices of Monte Carlo Simulation
#' 
#' Get the ending prices, i.e. terminal values, of a monte carlo simulation
#' @param mc monte carlo object created with \link{\code{monteCarlo}}
#' @return vector ending prices
#' @export
endingPrices <- function(mc){
  if(!inherits(mc, "monte_carlo")) stop("mc must be of class 'monte_carlo'")
  return(mc[nrow(mc),])
}

#' Plot Ending Prices 
#' 
#' Plot the ending prices from a Monte Carlo simulation
#' @param mc monte carlo object created with \link{\code{monteCarlo}}
#' @export
plotEndingPrices <- function(mc){
  if(!inherits(mc, "monte_carlo")) stop("mc must be of class 'monte_carlo'")
  ending_prices <- endingPrices(mc)
  dens_ep <- density(ending_prices)
  hist(ending_prices, freq=FALSE)
  lines(dens_ep)
  invisible(list(ending_prices=ending_prices, density=dens_ep))
}
