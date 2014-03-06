
##### functions #####

# function to compute EWMA volatility estimates of univariate returns
volEWMA <- function(R, lambda=0.94, initialWindow=10){
  # Check for lambda between 0 and 1 & initialWindow must be greater than ncol(R)
  if ((lambda > 1) || (lambda < 0)) stop("lambda must be greater than 0 or less than 1")
  if (initialWindow > nrow(R)) stop("Initial window must be less than the number of observations")
  
  # R must be an xts object and have only 1 column of data for univariate EWMA estimate
  if(ncol(R) > 1){
    warning("Only using first column of data")
    R <- R[,1]
  }
  
  # Separate data into a initializing window and a testing window
  initialR = R[1:initialWindow,]
  testR = R[(initialWindow+1):nrow(R),]
  
  # Compute initial variance estimate
  varOld <- var(initialR)
  
  # Initialize output vector
  tmp <- vector("numeric", nrow(testR))
  
  # Loop through and recursively update variance estimate
  for(i in 1:nrow(testR)){
    tmp[i] <- lambda * varOld + (1 - lambda) * testR[i]^2
    varOld <- tmp[i]
  }
  
  # Pad with leading NA and compute sqrt of variance vector
  out <- xts(c(rep(NA, initialWindow), sqrt(tmp)), index(R))
  colnames(out) <- colnames(R)
  return(out)
}

# function to compute EWMA covariance estimates
covEWMA <- function(R, lambda=0.94, initialWindow=10){
  # Check for lambda between 0 and 1 & initialWindow must be greater than ncol(R)
  if ((lambda > 1) || (lambda < 0)) stop("lambda must be greater than 0 or less than 1")
  if (initialWindow > nrow(R)) stop("Initial window must be less than the number of observations")
  
  # R must be an xts object and have only 2 columnw of data for bivariate EWMA estimate
  if(ncol(R) > 2){
    warning("Only using first two columns of data")
    R <- R[,1:2]
  }
  
  # Separate data into a initializing window and a testing window
  initialR = R[1:initialWindow,]
  testR = coredata(R[(initialWindow+1):nrow(R),])
  
  # Compute initial estimate
  covOld <- cov(initialR[,1], initialR[,2])
  
  # Initialize output vector
  tmp <- vector("numeric", nrow(testR))
  
  # Loop through and recursively update covariance estimate
  for(i in 1:nrow(testR)){ 
    tmp[i] <- covOld + (1 - lambda) * (testR[i, 1] * testR[i, 2]  - covOld)
    covOld <- tmp[i]
  }
  
  # Pad with leading NA
  out <- xts(c(rep(NA, initialWindow), tmp), index(R))
  colnames(out) <- paste(colnames(R), collapse=".")
  return(out)
}

# function to compute EWMA correlation estimates of a bivariate dataset
corEWMA <- function(R, lambda=0.94, initialWindow=10){
  # Check for lambda between 0 and 1 & initialWindow must be greater than ncol(R)
  if ((lambda > 1) || (lambda < 0)) stop("lambda must be greater than 0 or less than 1")
  if (initialWindow > nrow(R)) stop("Initial window must be less than the number of observations")
  
  # R must be an xts object and have only 2 columns of data for bivariate EWMA estimate
  if(ncol(R) > 2){
    warning("Only using first two columns of data")
    R <- R[,1:2]
  }
  
  # Separate data into a initializing window and a testing window
  initialR = R[1:initialWindow,]
  testR = coredata(R[(initialWindow+1):nrow(R),])
  
  # Compute initial estimates
  covOld <- cov(initialR[,1], initialR[,2])
  varOldX <- var(initialR[,1])
  varOldY <- var(initialR[,2])
  
  # Initialize output vector
  tmp <- vector("numeric", nrow(testR))
  
  # Loop through and recursively update estimates
  for(i in 1:nrow(testR)){
    # Compute the covariance EWMA estimate
    tmpCov <- covOld + (1 - lambda) * (testR[i, 1] * testR[i, 2]  - covOld)
    # Compute the variance EWMA estimate for each asset
    tmpVarX <- lambda * varOldX + (1 - lambda) * testR[i,1]^2
    tmpVarY <- lambda * varOldY + (1 - lambda) * testR[i,2]^2
    
    # Compute correlation with the covariance and volatility of each asset
    tmp[i] <- tmpCov / (sqrt(tmpVarX) * sqrt(tmpVarY))
    
    # Now update the old values
    covOld <- tmpCov
    varOldX <- tmpVarX
    varOldY <- tmpVarY
  }
  
  # Pad with leading NA
  out <- xts(c(rep(NA, initialWindow), tmp), index(R))
  colnames(out) <- paste(colnames(R), collapse=".")
  return(out)
}

##### testing #####
library(GARPFRM)

# data and parameters for EWMA estimate
data(crsp.short)
R <- largecap.ts[, 1:2]
lambda <- 0.94
initialWindow <- 15

# volatility estimate of univariate data
vol1 <- volEWMA(R[,1], lambda, initialWindow)

# covariance estimate of bivariate data
cov1 <- covEWMA(R, lambda, initialWindow)

# correlation estimate of bivariate data
cor1 <- corEWMA(R, lambda, initialWindow)

plot(vol1)
plot(cov1)
plot(cor1)
