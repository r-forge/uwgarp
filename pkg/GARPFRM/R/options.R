
##### Option Specification #####
#' Option Specification
#' 
#' Specify parameters of an option
#' 
#' @param style style of the option, e.g. european, american, etc.
#' @param type type of the option. Only calls and puts are supported currently.
#' @param S0 underlying asset price
#' @param K strike price
#' @param maturity the life of the option, measured in years.
#' @param r risk free rate
#' @param volatility volatility of the underlying asset price
#' @param q continuous dividend yield rate for options on stocks or stock 
#' indices paying a dividend. Also the foreign risk free rate for options on 
#' currencies.
#' @return an object of class "option" with the parameters that specify the option
#' @author Ross Bennett
#' @export
optionSpec <- function(style=c("european", "american"), 
                       type=c("call", "put"), 
                       S0=100, 
                       K=100,
                       maturity=1,
                       r=0.05, 
                       volatility=0.2, 
                       q=0){
  style <- match.arg(style)
  type <- match.arg(type)
  
  # Put into a list and return
  out <- list()
  out$style <- tolower(style)
  out$type <- tolower(type)
  out$S0 <- S0
  out$K <- K
  out$maturity <- maturity
  out$r <- r
  out$volatility <- volatility
  out$q <- q
  class(out) <- "option"
  return(out)
}

is.option <- function(x){
  inherits(x, "option")
}

##### Value #####

#' Option Value
#' 
#' Estimate the value of an option
#' 
#' @param option an \code{option} object created with \code{\link{optionSpec}}
#' @param method the method used to value the option
#' @param N number of steps in binomial tree
#' @param \dots any other passthrough parameters
#' @return the estimated value of the option
#' @author Ross Bennett
#' @export
optionValue <- function(option, method=c("Binomial", "Black-Scholes"), N=20, ...){
  if(!is.option(option)) stop("option must be of class 'option'")
  
  style <- option$style
  method <- tolower(method[1])
  
  if(style == "american"){
    if(method == "binomial" || method == "lattice"){
      out <- americanBinomial(option, N)
    } else {
      print(paste(method, " is not supported for an american option"))
      out <- NULL
    }
  } # american
  
  if(style == "european"){
    bs_methods <- c("black-scholes", "black-scholes-merton", "bs", "bsm")
    if(method == "binomial" || method == "lattice"){
      out <- europeanBinomial(option, N)
    } else if(method %in% bs_methods){
      out <- europeanBS(option)
    } else {
      print(paste(method, " is not supported for an american option"))
      out <- NULL
    }
  } # european
  return(out)
}

##### Binomial Tree #####

# Binomial tree to price a european option
europeanBinomial <- function(option, N){
  if(!is.option(option)) stop("option must be of class 'option'")
  if(option$style != "european") stop("must be a european option")
  
  # N: number of time steps
  # type: call or put
  # S0: initial asset value
  # K: strike price
  # r: continuously compounded yearly risk-free rate
  # vol: annualized standard deviation of log return
  # q: continuous dividend yield
  # ttm: time to maturity (in years), i.e. the life of the option
  
  # Extract the parameters of the option
  type <- option$type
  S0 <- option$S0
  K <- option$K
  r <- option$r
  vol <- option$volatility
  q <- option$q
  ttm <- option$maturity
  
  # 1 for call, -1 for put
  if(type == "call"){
    mult <- 1
  } else if(type == "put") {
    mult <- -1
  } else {
    mult <- 0
  }
  
  # Time step (delta t)
  dt <- ttm / N
  
  # Size of up move
  u <- exp(vol * sqrt(dt))
  
  # Size of down move
  d <- exp(-vol * sqrt(dt))
  
  # Risk neutral probability of an uptick
  p <- (exp((r - q) * dt) - d)/(u - d)
  
  # Vectorized version of binomial tree for european option
  A <- choose(N, 0:N) * p^(0:N) * ((1 - p)^(N - (0:N))) * pmax(mult * ((u^(0:N)) * (d^(N - (0:N))) * S0 - K), 0)
  A <- exp(-r * ttm) * sum(A)
  return(A)
}

# Binomial tree to price an american option
americanBinomial <- function(option, N){
  if(!is.option(option)) stop("option must be of class 'option'")
  if(option$style != "american") stop("must be an american option")
  
  # N: number of time steps
  # type: call or put
  # S0: initial asset value
  # K: strike price
  # r: continuously compounded yearly risk-free rate
  # vol: annualized standard deviation of log return
  # q: continuous dividend yield
  # ttm: time to maturity (in years), i.e. the life of the option
  
  # Extract the parameters of the option
  type <- option$type
  S0 <- option$S0
  K <- option$K
  r <- option$r
  vol <- option$volatility
  q <- option$q
  ttm <- option$maturity
  
  # 1 for call, -1 for put
  if(type == "call"){
    mult <- 1
  } else if(type == "put") {
    mult <- -1
  } else {
    mult <- 0
  }
  
  # List to store option values
  # These are used at the end to compute greeks
  # option_value <- vector("list", 4)
  
  # Time step (delta t)
  dt <- ttm / N
  
  # Size of up move
  u <- exp(vol * sqrt(dt))
  
  # Size of down move
  d <- exp(-vol * sqrt(dt))
  
  # Risk neutral probability of an uptick
  p <- (exp((r - q) * dt) - d)/(u - d)
  
  # Discount factor
  df <- exp(-r * dt)
  
  # At the terminal node, there are N+1 asset values
  V <- pmax(0, mult * (S0 * (u^(0:N)) * (d^(N - (0:N))) - K))
  # if(N == 4) option_value[[4]] <- V
  
  # Iterate backward, such that there are j+1 asset values, where j is the
  # Number of time steps
  j.index <-seq(from=N-1, to=0, by=-1)
  for (j in j.index) {
    # S is the vector of prices at each time step and node
    S <- S0 * (u^(0:j)) * (d^(j - (0:j)))
    
    # V.new is the vector of option values at each time step and node
    V.new <- pmax(df * (p * V[2:(j+2)] + (1 - p) * V[1:(j+1)]), mult * (S[1:(j+1)] - K))
    #if((j <= 4) & (j != 0)){
    #  option_value[[j]] <- V.new
    #}
    V[1:(j+1)] <- V.new[1:(j+1)]
    print(V)
  }
  # calculate the greeks
  # delta <- (f_02 - f_00) / (u^2 * S0 - d^2 * S0)
  #delta <- (option_value[[2]][3] - option_value[[2]][1]) / (u^2 * S0 - d^2 * S0)
  #delta_u <- (option_value[[2]][3] - option_value[[2]][2]) / (u^2 * S0 - S0)
  #delta_d <- (option_value[[2]][2] - option_value[[2]][1]) / (S0 - d^2 * S0)
  #gamma <- (delta_u - delta_d) / (0.5 * (u^2 * S0 - d^2 * S0))
  # theta <- (f_22 - f_01) / (2 * dt)
  #theta <- (option_value[[4]][3] - option_value[[2]][2]) / (2 * dt)
  # The final value is the option price
  f <- V[1]
  #list(option_price=f, delta=delta, gamma=gamma, theta=theta, tree_values=option_value)
  return(f)
}

##### Black-Scholes #####

europeanBS <- function(option){
  if(!is.option(option)) stop("option must be of class 'option'")
  if(option$style != "european") stop("must be a european option")
  
  type <- option$type
  
  S0 <- option$S0
  K <- option$K
  r <- option$r
  q <- option$q
  vol <- option$volatility
  ttm <- option$maturity
  
  if(type == "call"){
    out <- callEuropeanBS(S0=S0, K=K, r=r, q=q, vol=vol, ttm=ttm)
  } else if(type == "put"){
    out <- putEuropeanBS(S0=S0, K=K, r=r, q=q, vol=vol, ttm=ttm)
  } else {
    out <- NULL
  }
  return(out)
}

callEuropeanBS <- function(S0, K, r, q, vol, ttm){
  # S0: inital price of underlying
  # K: strike price
  # r: risk-free rate
  # q: dividend yield
  # vol: annualized volatility
  # ttm: time to maturity (in years)
  d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  call <- S0 * pnorm(d1) * exp(-q * ttm) - K * pnorm(d2) * exp(-r * ttm)
  return(call)
}

putEuropeanBS <- function(S0, K, r, q, vol, ttm){
  # S0: initial price of underlying
  # K: strike price
  # vol: annualized volatility
  # r: risk-free rate of 
  # rf: risk-free rate of foreign currency
  # ttm: time to maturity in years
  
  d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  put <- K * exp(-r * ttm) * pnorm(-d2) - S0 * exp(-q * ttm) * pnorm(-d1)
  return(put)
}

##### Greeks #####
# delta
# theta
# gamma
# vega
# rho

deltaBS <- function(S0, K, r, q, vol, ttm, type){
  d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  if(type == "call"){
    out <- exp(-q * ttm) * pnorm(d1)
  } else if(type == "put"){
    out <- exp(-q * ttm) * (pnorm(d1) - 1)
  } else {
    # Not a valid type
    out <- NULL
  }
  return(out)
}

# delta.call <- function(S0, K, r, q, vol, ttm){
#   d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   delta <- exp(-q * ttm) * pnorm(d1)
#   return(delta)
# }
# 
# delta.put <- function(S0, K, r, q, vol, ttm){
#   d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   delta <- exp(-q * ttm) * (pnorm(d1) - 1)
#   return(delta)
# }

thetaBS <- function(S0, K, r, q, vol, ttm, type){
  d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  if(type == "call"){
    out <- - (S0 * dnorm(d1) * vol) / (2 * sqrt(ttm)) - r * K * exp(-r * ttm) * pnorm(d2)
  } else if(type == "put"){
    out <- - (S0 * dnorm(d1) * vol) / (2 * sqrt(ttm)) + r * K * exp(-r * ttm) * pnorm(-d2)
  } else {
    # Not a valid type
    out <- NULL
  }
  return(out)
}

# theta.call <- function(S0, K, r, q, vol, ttm){
#   d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   theta <- - (S0 * dnorm(d1) * vol) / (2 * sqrt(ttm)) - r * K * exp(-r * ttm) * pnorm(d2)
#   return(theta)
# }
# 
# theta.put <- function(S0, K, r, q, vol, ttm){
#   d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   theta <- - (S0 * normCDF(d1) * vol) / (2 * sqrt(ttm)) + r * K * exp(-r * ttm) * pnorm(-d2)
#   return(theta)
# }

gammaBS <- function(S0, K, r, q, vol, ttm){
  d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  out <- dnorm(d1) / (S0 * vol * sqrt(ttm))
  return(out)
}

# gamma.call <- function(S0, K, r, q, vol, ttm){
#   d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   gamma <- dnorm(d1) / (S0 * vol * sqrt(ttm))
#   return(gamma)
# }
# 
# gamma.put <- gamma.call

vegaBS <- function(S0, K, r, q, vol, ttm){
  d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  out <- S0 * sqrt(ttm) * dnorm(d1)
  return(out)
}

# vega.call <- function(S0, K, r, q, vol, ttm){
#   d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   vega <- S0 * sqrt(ttm) * dnorm(d1)
#   return(vega)
# }
# 
# vega.put <- vega.call

rhoBS <- function(S0, K, r, q, vol, ttm, type){
  d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  if(type == "call"){
    out <- K * ttm * exp(-r * ttm) * pnorm(d2)
  } else if(type == "put"){
    out <- -K * ttm * exp(-r * ttm) * pnorm(-d2)
  } else {
    out <- NULL
  }
  return(out)
}

# rho.call <- function(S0, K, r, q, vol, ttm){
#   d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   rho <- K * ttm * exp(-r * ttm) * pnorm(d2)
#   return(rho)
# }
# 
# rho.put <- function(S0, K, r, q, vol, ttm){
#   d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   rho <- -K * ttm * exp(-r * ttm) * pnorm(-d2)
#   return(rho)
# }

