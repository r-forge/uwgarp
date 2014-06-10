# Convexity and Duration

#' Calculate the modified duration of a bond
#' 
#' The function estimates modified duration of a fixed rate coupon bond 
#' given the discount curve and bond data. The modified duration is calculated
#' using the continuously compounded yield
#' 
#' @param bond a \code{bond} object in discountFactorArbitrage
#' @param discountCurve vector of discount rates
#' @param percentChangeYield optional elasticity measure 
#' @return duration of the bond
#' @export
bondDuration <- function(bond, discountCurve, percentChangeYield = 0){
  # Get data from the bond and discount curve
  nDC = length(discountCurve)
  m = bond$m
  couponRate = bond$couponRate
  face = bond$face
  time = bond$time
  # Calculate the ytm
  ytm = bondYTM(bond=bond, discountCurve=discountCurve) + percentChangeYield
  # Convert to continuously compounded rate
  y_c = m * log(1 + ytm / m)
  # Get the cashflows of coupon amounts and face value
  couponAmount = face * couponRate / m
  cashflows = rep(couponAmount, nDC)
  cashflows[nDC] = couponAmount + face
  # Calculate the price based on the continuously compounded rate
  price = sum(cashflows * exp(-y_c * time))
  # Calculate the duration
  duration = sum(-time * cashflows * exp(-y_c * time)) / -price
  return(duration)
}

#' Calculate the convexity of a fixed rate coupon bond
#' 
#' This function estimates the convexity of a fixed rate coupon bond 
#' given the discount curve and bond data.
#' 
#' @param bond a \code{bond} object in discountFactorArbitrage
#' @param discountCurve vector of discount rates
#' @return convexity of the bond
#' @export
bondConvexity <- function(bond, discountCurve){
  # Get data from the bond and discount curve
  nDC = length(discountCurve)
  m = bond$m
  couponRate = bond$couponRate
  face = bond$face
  time = bond$time
  # Get the cashflows of coupon amounts and face value
  couponAmount = face * couponRate / m
  cashflows = rep(couponAmount, nDC)
  cashflows[nDC] = couponAmount + face
  # The price is the sum of the discounted cashflows
  price = sum(discountCurve * cashflows)
  weights = (discountCurve * cashflows) / price
  convexity = sum(weights * time^2)
  return(convexity)
}

#' Calculate the yield to maturity of a bond
#' 
#' This function calculates the yield to maturity of a fixed rate coupon bond 
#' given the discount curve and bond data.
#' 
#' @param bond a \code{bond} object
#' @param discountCurve vector of discount rates
#' @return yield to maturity of the bond
#' @export
bondYTM <- function(bond, discountCurve){
  # First step is to calculate the price based on the discount curve
  price <- bondPrice(bond=bond, discountCurve=discountCurve)
  
  # Get the data from the bond object
  m <- bond$m
  couponRate <- bond$couponRate
  face <- bond$face
  time <- bond$time
  
  # Use optimize to solve for the yield to maturity
  tmp <- optimize(ytmSolve, interval=c(-1,1), couponRate=couponRate, m=m, nPayments=length(time), face=face, targetPrice=price, tol=.Machine$double.eps)
  ytm <- tmp$minimum
  return(ytm)
}

#' Solve for the yield to maturity of a bond
#' 
#' This function solves for the yield to maturity of a fixed rate coupon bond 
#' given the discount curve and bond data.
#' 
#' @param ytm yield to maturity
#' @param couponRate coupon rate
#' @param m compounding frequency
#' @param nPayments is the number of payments
#' @param face is the face value
#' @param targetPrice is the price of the bond
#' @return Absolute value of difference between the price and the present value
#' @export
ytmSolve <- function(ytm, couponRate, m, nPayments, face, targetPrice){
  C <- face * couponRate / m
  tmpPrice <- 0
  for(i in 1:nPayments){
    tmpPrice <- tmpPrice + C / ((1 + (ytm / m))^i)
  }
  tmpPrice <- tmpPrice + face / (1 + ytm / m)^nPayments
  return(abs(tmpPrice - targetPrice))
}

#' Calculate the convexity of a fixed rate coupon bond
#' 
#' This function estimates the delta for hedging a particular bond 
#' given bond data
#' 
#' @param bond a \code{bond} object in discountFactorArbitrage
#' @return delta of the hedge
#' @export
linearHedge <- function(bond){
  ### Write body####
  
  
  
  return(delta)
}

#' Estimate PCA loadings and creat PCA object
#' 
#' This function estimates the delta for hedging a particular bond 
#' given bond data
#' 
#' @param data time series data
#' @return pca object loadings
#' @export
PCA <- function(data){
  ### Write body####
  
  
  
  return(delta)
}

#' PCA loadings
#' 
#' Extract the computed loadings.
#' 
#' @param object a capm object created by loadings PCA
#' @author TF
#' @export
getthreeLoadings <- function(object){
  UseMethod("getthreeLoadings")
}

#' @method getthreeLoadings
#' @S3method getthreeLoadings
getthreeLoadings <- function(object){
  ### Write body####
  
  
  return(threeLoadings)
}

#' Plotting method for PCA
#' 
#' Plot a fitted PCA object
#' 
#' @param x a PCA object created.
#' @param y not used
#' @param number specify the nunber of loadings
#' @param \dots passthrough parameters to \code{\link{plot}}.
#' @param main a main title for the plot
#' @author Thomas Fillebeen
#' @method plot pca loadings
#' @S3method plot capm_uv
plot.capm_uv <- function(x, y, number, ..., main="CAPM"){
  ### Write body####
  
  
  
  
  # Plot the first three factors
  plot(pca$loading[,1], type="l", main="Beta from PCA regression", 
       xlab="maturity", ylab="beta")
  lines(pca$loading[,2], col="blue",lty=2)
  lines(pca$loading[,3], col="red",lty=2)
  legend("topleft",legend=c("PCA1","PCA2","PCA3"),bty="n",lty=c(1,2,2),col=c("black","blue","red"), cex=0.8)
  
}

