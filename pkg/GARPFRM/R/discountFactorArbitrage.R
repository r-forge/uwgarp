# Ch 6 Prices, Discount Factors, and Arbitrage (Law of one Price)
# The Cash Flows from Fixed-Rate Government Coupon Bonds, Discount Faors, Law of One Price
# Arbitrage opportunity: trade that generates profits without any chance of losing money.
# If there is a deviation from the law of one price, there exists an arbitrage opportunity.
# In order to estimate the discount factor for a particular term gives the value today, 
# or the present alue of one unit of currency to be received at the end of that term.(Pg.129)

#' Constructor for bond specification
#' 
#' Created a bond object \code{bond.spec} with data for bond specification.
#' 
#' @param time vector of sequence of coupon payments in years
#' @param face face value of bond
#' @param m compounding frequency
#' @param couponRate rate the coupon pays
#' @return a \code{bond} object with the bond data used for pricing
#' @author TF
#' @export
bondSpec = function(time=seq(from=0.5,to=2,by=0.5), face=100, m=2, couponRate=0.01){
  if(!all(diff(time) == (1/m))) stop("misspecification of sequence of time and compounding frequency")
  bond = list()
  bond$m = m
  bond$couponRate = couponRate
  bond$face = face
  bond$time = time
  class(bond) = c("bond.spec", "bond")
  return(bond)
}

#' To determine if user is specifying bond parameters correctly
#' 
#' @param object a capm object created by \code{\link{bond.spec}}
#' @author TF
#' @export
is.bond = function(object){
  inherits(object, "bond.spec")
}

#' Estimate price of bond
#' 
#' This function calculates the price of a fixed rate coupon bond given the 
#' discount curve and bond data. First it converts the discountCurve into CF
#' @param bond a \code{discountFactorArbitrage} object
#' @param discountCurve vector of discount rates
#' @return price of the bond
#' @author TF
#' @export
bondPrice = function(bond, discountCurve){
  if(!is.bond(bond)) stop("bond must be an object of class 'bond'")
  # Number of periods in discount curve
  nDC <- length(discountCurve)
  m <- bond$m
  couponRate <- bond$couponRate
  face <- bond$face
  time <- bond$time
  
  couponAmount <- face * couponRate / m
  cashflows <- rep(couponAmount, nDC)
  cashflows[nDC] <- couponAmount + face
  price <- sum(cashflows * discountCurve)
  return(price)
}

#' Estimate discountFactor
#' 
#' This function calculates the discountFactor (DF) given price 
#' and cashFlows.
#' @param bond a \code{discountFactorArbitrage} object
#' @param price  of a bond
#' @return cashFlow of a bond
#' @author TF
#' @export
discountFactor = function(price, cashFlow){
  DF = solve(cashFlow) %*% price
  return(DF)
}

#' Estimate price of bond w/ acrrued interest
#' 
#' This function calculates the price of a fixed rate coupon bond given coupon rate, yield, 
#' compoundPd, cashFlowPd, face value, previous coupon date, next coupon date.
#' @param bond is a bondSpec object
#' @param yield is the yield on the bond
#' @param cashFlowPd cash flow period
#' @param t0 previous coupon date
#' @param t1 next coupon period
#' @param currentDate current date
#' @return price of the bond: clean, dirty and accrued interest
#' @author TF
#' @export
bondFullPrice = function(bond, yield, cashFlowPd, t0, t1, currentDate){
  compoundPd = bond$m
  face = bond$face
  couponRate = bond$couponRate
  # Apply a general dayCount (weekend included)
  d1 = as.numeric(t1-currentDate)
  d2 = as.numeric(t1-t0)
  # Initialize
  tmp = 0 
  for(k in 1:(cashFlowPd-1)){
    tmp = tmp + ((couponRate / compoundPd * face) / ((1 + yield/compoundPd)^k))
  }
  # Calculate dirty price based on partial periods formula
  dirtyP = (1 / ((1 + yield / compoundPd)^(d1/d2))) * (couponRate / compoundPd * face + tmp + face / ((1 + yield/compoundPd)^(cashFlowPd-1)))
  # Calculate accruedInterest
  aiDays = as.numeric(currentDate-t0)
  couponDays = as.numeric(t1-t0)
  ai = couponRate / compoundPd * face * aiDays / couponDays
  cleanP = dirtyP - ai
  return(list(dirty=dirtyP, clean=cleanP, accruedInterest=ai))
}

#' Estimate continuously conpounding rate to be used in term structure
#' 
#' This function calculates the continuously compounding rate given an initial dataset 
#' with specific format, date of reference coumpounding frequency, and face value
#' @param dat is a dataset with cusip, issueDate, MaturityDate, Name, Coupon, Bid/Ask
#' @param intialDate is the date when the estimation should be conducted: date of reference
#' @param m compounding frequency
#' @param face face value
#' @return continuously compounding rates
#' @author TF
#' @export
compoundingRate = function(dat, initialDate=as.Date("1995-05-15"), m, face=100){
  # Convert the dates to a date class
  dat[, "IssueDate"] = as.Date(dat[, "IssueDate"], format="%m/%d/%Y")
  dat[, "MaturityDate"] = as.Date(dat[, "MaturityDate"], format="%m/%d/%Y")
  # Convert the coupon column to a numeric
  dat[, "Coupon"] = as.numeric(gsub("%", "", dat[, "Coupon"])) / 100
  # Vector of prices
  price = (dat[, "Bid"] + dat[, "Ask"]) / 2
  
  # Generate cash flow dates for each bond
  bondData = list()
  for(i in 1:nrow(dat)){
    maturityDate = dat[i, "MaturityDate"]
    # Intialize a new list for every price, coupon, coupon date.
    bondData[[i]] = list()
    # Store price and the number of the coupon
    bondData[[i]]$price = price[i]
    bondData[[i]]$coupon = dat[i, "Coupon"]
    # Remove initialDate
    tmpSeq <- seq(from=initialDate, to=maturityDate, by="3 months")
    bondData[[i]]$couponDates = tmpSeq[-1]
    tmpDates = bondData[[i]]$couponDates
    tmpCoupons = vector("numeric", length(tmpDates))
    for(j in 1:length(tmpDates)){
      tmpCoupons[j] = bondData[[i]]$coupon / m * face
      if(j == length(tmpDates)){
        tmpCoupons[j] = face + bondData[[i]]$coupon / m * face
      }
    }
    bondData[[i]]$cashFlow = tmpCoupons
  }
  # Create a matrix of cash flows
  CF = matrix(0, length(price), length(price))
  # Populate the CF matrix
  for(i in 1:nrow(CF)){
    tmp = bondData[[i]]$cashFlow
    index = 1:length(tmp)
    CF[i, index] = tmp
  }
  # Utilize the discountFactor function
  DF = discountFactor(price,CF)

  dates = bondData[[nrow(dat)]]$couponDates
  years = vector("numeric", length(dates))
  for(i in 1:length(years)){
    years[i] = (as.numeric(strftime(dates[i], "%Y")) + as.numeric(strftime(dates[i], "%m"))/12) - (as.numeric(strftime(initialDate, "%Y")) + as.numeric(strftime(initialDate, "%m"))/12)
  }
  # Calculate continuously compounded rates from discount factors
  ccRate = vector("numeric", length(years))
  for(i in 1:length(ccRate)){
    ccRate[i] = - log(DF[i]) / years[i]
  }
  rate = list()
  rate$years = years
  rate$ccRate = ccRate 
  return(rate)
}