# Convexity and Duration

#' Calculate the modified duration of a bond
#' 
#' This function calculates the modified duration of a fixed rate coupon bond 
#' given the discount curve and bond data. The modified duration is calculated
#' using the continuously compounded yield
#' 
#' @param bond a \code{bond} object
#' @param discountCurve vector of discount rates
#' @return duration of the bond
#' @export
bondDuration <- function(bond, discountCurve){
  
  # Get data from the bond and discount curve
  nDC <- length(discountCurve)
  m <- bond$m
  couponRate <- bond$couponRate
  face <- bond$face
  time <- bond$time
  
  # Calculate the ytm
  ytm <- bondYTM(bond=bond, discountCurve=discountCurve)
  
  # Convert to continuously compounded rate
  y_c <- m * log(1 + ytm / m)
  
  # Get the cashflows of coupon amounts and face value
  couponAmount <- face * couponRate / m
  cashflows <- rep(couponAmount, nDC)
  cashflows[nDC] <- couponAmount + face
  
  # Calculate the price based on the continuously compounded rate
  price <- sum(cashflows * exp(-y_c * time))
  
  # Calculate the duration
  duration <- sum(-time * cashflows * exp(-y_c * time)) / -price
  return(duration)
}

#' Calculate the convexity of a fixed rate coupon bond
#' 
#' This function calculates the convexity of a fixed rate coupon bond 
#' given the discount curve and bond data.
#' 
#' @param bond a \code{bond} object
#' @param discountCurve vector of discount rates
#' @return convexity of the bond
#' @export
bondConvexity <- function(bond, discountCurve){
  # Get data from the bond and discount curve
  nDC <- length(discountCurve)
  m <- bond$m
  couponRate <- bond$couponRate
  face <- bond$face
  time <- bond$time
  
  # Get the cashflows of coupon amounts and face value
  couponAmount <- face * couponRate / m
  cashflows <- rep(couponAmount, nDC)
  cashflows[nDC] <- couponAmount + face
  
  # The price is the sum of the discounted cashflows
  price <- sum(discountCurve * cashflows)
  
  weights <- (discountCurve * cashflows) / price
  
  # weights <- ((discountCurve * cashflows) / price) * time^2
  convexity <- sum(weights * time^2)
  return(convexity)
}

#### linear hedge####

