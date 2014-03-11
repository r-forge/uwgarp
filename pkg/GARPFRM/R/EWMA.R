
#' EWMA Model
#' 
#' EWMA model to estimate volatility, covariance, and correlation
#' 
#' If lambda=NULL, the lambda value can be estimated for univariate estimates 
#' of volatility,  covariance, and correlation by minimizing the sum of 
#' squared differences between the estimated value and realized value. 
#' 
#' @param R xts object of asset returns
#' @param lambda smoothing parameter, must be greater than 0 or less than 1. If
#' NULL, lambda will be estimated by minimizing the sum of squared difference
#' between the estimated value and the realized value.
#' @param initialWindow initial window of observations used in estimating the 
#' initial conditions
#' @param n number of periods used to calculate realized volatility, covariance, or correlation.
#' @param type estimate volatility, covariance, or correlation.
#' 
#' @export
EWMA <- function(R, lambda=0.94, initialWindow=10, n=10, type=c("volatility", "covariance", "correlation")){
  type <- match.arg(type)
  
  # Check for lambda between 0 and 1 & initialWindow must be greater than ncol(R)
  if(!is.null(lambda)){
    if ((lambda > 1) || (lambda < 0)) stop("lambda must be greater than 0 or less than 1")
  }
  if (initialWindow > nrow(R)) stop("Initial window must be less than the number of observations")
  
  
  # check if R is univariate, bivariate, or multivariate
  if(ncol(R) == 1){
    # univariate
    data <- "uv"
  } else if(ncol(R) == 2){
    # bivariate
    data <- "bv"
  } else {
    # multivariate
    data <- "mv"
  }
  
  realized <- NULL
  
  # univariate volatility estimate
  if((data == "uv") & (type == "volatility")){
    if(is.null(lambda)){
      lambda <- estimateLambdaVol(R=R, initialWindow=initialWindow, n=n)
    }
    est <- uvEWMAvol(R=R, lambda=lambda, initialWindow=initialWindow)
    realized <- realizedVol(R=R, n=n)
    class <- "uvEWMAvol"
  }
  
  # bivariate covariance estimate
  if((data == "bv") & (type == "covariance")){
    if(is.null(lambda)){
      lambda <- estimateLambdaCov(R=R, initialWindow=initialWindow, n=n)
    }
    est <- uvEWMAcov(R=R, lambda=lambda, initialWindow=initialWindow)
    #realized <- realizedCov(R=R, n=n)
    class <- "uvEWMAcov"
  }
  
  # bivariate correlation estimate
  if((data == "bv") & (type == "correlation")){
    if(is.null(lambda)){
      lambda <- estimateLambdaCor(R=R, initialWindow=initialWindow, n=n)
    }
    est <- uvEWMAcor(R=R, lambda=lambda, initialWindow=initialWindow)
    #realized <- realizedCor(R=R, n=n)
    class <- "uvEWMAcor"
  }
  
  # multivariate covariance estimate
  if((data == "mv") & (type == "covariance")){
    if(is.null(lambda)){
      lambda <- 0.94
      warning("lambda must be specified for multivariate data")
    }
    est <- mvEWMAcov(R=R, lambda=lambda, initialWindow=initialWindow)
    class <- "mvEWMAcov"
  }
  
  # multivariate correlation estimate
  if((data == "mv") & (type == "correlation")){
    if(is.null(lambda)){
      lambda <- 0.94
      warning("lambda must be specified for multivariate data")
    }
    est <- mvEWMAcor(R=R, lambda=lambda, initialWindow=initialWindow)
    class <- "mvEWMAcor"
  }
  
  # put the parameters in a list
  parameters <- list(lambda=lambda, 
                     initialWindow=initialWindow,
                     type=type)
  
  # put the raw data and realized values in a list
  data <- list(R=R, realized_values=realized)
  
  # structure and return
  out <- structure(list(estimate=est,
                        model=parameters,
                        data=data), 
                   class=c("EWMA", class))
  return(out)
}


#' EWMA Volatility Estimate
#' 
#' EWMA model to estimate volatility
#' 
#' @param R xts object of asset returns
#' @param lambda smoothing parameter, must be greater than 0 or less than 1
#' @param initialWindow initial window of observations used in estimating the 
#' initial conditions
#' 
#' @export
uvEWMAvol <- function(R, lambda=0.94, initialWindow=10){
  # function to compute EWMA volatility estimates of univariate returns
  
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
  oldVar <- var(initialR)
  oldR <- mean(initialR)
  
  # Initialize output vector
  tmp <- vector("numeric", nrow(testR))
  
  # Loop through and recursively update variance estimate
  for(i in 1:nrow(testR)){
    tmp[i] <- lambda * oldVar + (1 - lambda) * oldR^2
    oldVar <- tmp[i]
    oldR <- testR[i]
  }
  
  # Pad with leading NA and compute sqrt of variance vector
  out <- xts(c(rep(NA, initialWindow), sqrt(tmp)), index(R))
  colnames(out) <- colnames(R)
  return(out)
}

#' EWMA Covariance Estimate
#' 
#' EWMA model to estimate covariance
#' 
#' @param R xts object asset returns
#' @param lambda smoothing parameter, must be greater than 0 or less than 1
#' @param initialWindow initial window of observations used in estimating the 
#' initial conditions
#' 
#' @export
uvEWMAcov <- function(R, lambda=0.94, initialWindow=10){
  # function to compute EWMA covariance estimates
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
  oldR1 <- mean(initialR[,1])
  oldR2 <- mean(initialR[,2])
  
  # Initialize output vector
  tmp <- vector("numeric", nrow(testR))
  
  # Loop through and recursively update covariance estimate
  for(i in 1:nrow(testR)){ 
    tmp[i] <- covOld + (1 - lambda) * (oldR1 * oldR2  - covOld)
    covOld <- tmp[i]
    oldR1 <- testR[i, 1]
    oldR2 <- testR[i, 2]
  }
  
  # Pad with leading NA
  out <- xts(c(rep(NA, initialWindow), tmp), index(R))
  colnames(out) <- paste(colnames(R), collapse=".")
  return(out)
}

#' EWMA Correlation Estimate
#' 
#' EWMA model to estimate correlation
#' 
#' @param R xts object of asset returns
#' @param lambda smoothing parameter, must be greater than 0 or less than 1
#' @param initialWindow initial window of observations used in estimating the 
#' initial conditions
#' 
#' @export
uvEWMAcor <- function(R, lambda=0.94, initialWindow=10){
  # function to compute EWMA correlation estimates of a bivariate dataset
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
  varOld1 <- var(initialR[,1])
  varOld2 <- var(initialR[,2])
  oldR1 <- mean(initialR[,1])
  oldR2 <- mean(initialR[,2])
  
  # Initialize output vector
  tmp <- vector("numeric", nrow(testR))
  
  # Loop through and recursively update estimates
  for(i in 1:nrow(testR)){
    # Compute the covariance EWMA estimate
    tmpCov <- covOld + (1 - lambda) * (oldR1 * oldR2 - covOld)
    # Compute the variance EWMA estimate for each asset
    tmpVar1 <- lambda * varOld1 + (1 - lambda) * oldR1^2
    tmpVar2 <- lambda * varOld2 + (1 - lambda) * oldR2^2
    
    # Compute correlation with the covariance and volatility of each asset
    tmp[i] <- tmpCov / (sqrt(tmpVar1) * sqrt(tmpVar2))
    
    # Now update the old values
    covOld <- tmpCov
    varOld1 <- tmpVar1
    varOld2 <- tmpVar2
    oldR1 <- testR[i, 1]
    oldR2 <- testR[i, 2]
  }
  
  # Pad with leading NA
  out <- xts(c(rep(NA, initialWindow), tmp), index(R))
  colnames(out) <- paste(colnames(R), collapse=".")
  return(out)
}

mvEWMAcov <- function(R, lambda, initialWindow){
  # Separate data into a initializing window and a testing window
  initialR = R[1:initialWindow,]
  testR = R[(initialWindow+1):nrow(R),]
  
  # Initialize starting values
  lagCov = cov(initialR)
  oldR = as.numeric(colMeans(initialR))
  
  est = vector("list", nrow(testR))
  for(i in 1:nrow(testR)){
    est[[i]] = lambda * (oldR %*% t(oldR)) + (1 - lambda) * lagCov
    # Update values from the current period
    lagCov = est[[i]]
    oldR = as.numeric(testR[i,])
  }
  # Properly assign list key to date
  names(est) <- index(testR)
  return(est)
}

mvEWMAcor <- function(R, lambda, initialWindow){
  cov_est <- mvEWMAcov(R=R, lambda=lambda, initialWindow=initialWindow)
  est <- lapply(cov_est, cov2cor)
  # Properly assign list key to date
  # names(est) <- index(testR)
  return(est)
}

#' Realized Volatility
#' 
#' Calculate realized volatility
#' 
#' @param R xts object of asset returns
#' @param n number of periods used to calculate realized volatility
#' 
#' @export
realizedVol <- function(R, n){
  lag(rollapply(R[,1], width=n, FUN=sd))
}

cor.fun <- function(x){
  cor(x)[1,2]
}

cov.fun <- function(x){
  cov(x)[1,2]
}

#' Realized Covariance
#' 
#' Calculate realized covariance
#' 
#' @param R xts object of asset returns
#' @param n number of periods used to calculate realized volatility
#' 
#' @export
realizedCov <- function(R, n){
  lag(rollapply(R, width=n, FUN=cov.fun))
}

#' Realized Correlation
#' 
#' Calculate realized correlation
#' 
#' @param R xts object of asset returns
#' @param n number of periods used to calculate realized volatility
#' 
#' @export
realizedCor <- function(R, n){
  lag(rollapply(R, width=n, FUN=cor.fun))
}

# objective function for calculating lambda
# objective is the sum of squared differences between estimated volatility and 
# realized volatility
objLambdaVol <- function(lambda, R, initialWindow, n){
  realized <- realizedVol(R, n)
  est <- uvEWMAvol(R, lambda, initialWindow)
  tmpDat <- na.omit(cbind(est, realized))
  obj <- sum((tmpDat[,1] - tmpDat[,2])^2)
  return(obj)
}

# objective function for calculating lambda
# objective is the sum of squared differences between estimated covariance and 
# realized covariance
objLambdaCov <- function(lambda, R, initialWindow, n){
  realized <- realizedCov(R, n)
  est <- uvEWMAcov(R, lambda, initialWindow)
  tmpDat <- na.omit(cbind(est, realized))
  obj <- sum((tmpDat[,1] - tmpDat[,2])^2)
  return(obj)
}

# objective function for calculating lambda
# objective is the sum of squared differences between estimated correlation and 
# realized correlation
objLambdaCor <- function(lambda, R, initialWindow, n){
  realized <- realizedCor(R, n)
  est <- uvEWMAcor(R, lambda, initialWindow)
  tmpDat <- na.omit(cbind(est, realized))
  obj <- sum((tmpDat[,1] - tmpDat[,2])^2)
  return(obj)
}

#' Estimated Lambda
#' 
#' Estimate lambda for EWMA volatility estimate
#' 
#' @param R xts object of asset returns
#' @param initialWindow initial window of observations used in estimating the 
#' initial
#' @param n number of periods used to calculate realized volatility
#' 
#' @export
estimateLambdaVol <- function(R, initialWindow=10, n=10){
  opt <- optimize(objLambdaVol, interval=c(0,1), R=R, 
                  initialWindow=initialWindow, n=n, 
                  tol=.Machine$double.eps)
  lambda <- opt$minimum
  lambda
}

#' Estimated Lambda
#' 
#' Estimate lambda for EWMA covariance estimate
#' 
#' @param R xts object of asset returns
#' @param initialWindow initial window of observations used in estimating the 
#' initial
#' @param n number of periods used to calculate realized covariance
#' 
#' @export
estimateLambdaCov <- function(R, initialWindow=10, n=10){
  opt <- optimize(objLambdaCov, interval=c(0,1), R=R, 
                  initialWindow=initialWindow, n=n, 
                  tol=.Machine$double.eps)
  lambda <- opt$minimum
  lambda
}

#' Estimated Lambda
#' 
#' Estimate lambda for EWMA correlation estimate
#' 
#' @param R xts object of asset returns
#' @param initialWindow initial window of observations used in estimating the 
#' initial
#' @param n number of periods used to calculate realized correlation
#' 
#' @export
estimateLambdaCor <- function(R, initialWindow=10, n=10){
  opt <- optimize(objLambdaCor, interval=c(0,1), R=R, 
                  initialWindow=initialWindow, n=n, 
                  tol=.Machine$double.eps)
  lambda <- opt$minimum
  lambda
}

#' @method print EWMA
#' @S3method print EWMA
print.EWMA <- function(x, ...){
  cat("EWMA Estimate\n\n")
  
  cat("Parameters\n")
  cat("lambda: ", x$model$lambda, "\n", sep="") 
  cat("initialWindow: ", x$model$initialWindow, "\n", sep="")
  cat("type: ", x$model$type, "\n\n", sep="")
  
  cat("Final Period EWMA Estimate: \n")
  print(last(x$estimate))
}

# extract the covariance between two assets from an mvEWMAcov object
#' EWMA Covariance
#' 
#' Extract the covariance of two assets from an \code{EWMA} object
#' 
#' @param object an EWMA object created by \code{EWMA}
#' @param assets character vector or numeric vector. The assets can be 
#' specified by name or index.
#' @export
getCov <- function(EWMA, assets){
  UseMethod("getCov")
}

#' @method getCov mvEWMAcov
#' @S3method getCov mvEWMAcov
getCov.mvEWMAcov <- function(EWMA, assets=c(1,2)){
  if(!inherits(EWMA, "mvEWMAcov")) stop("object must be of class mvEWMAcov")
  
  if(length(assets) == 1) assets[2] <- assets[1]
  
  cnames <- colnames(EWMA$estimate[[1]])
  
  # Check if asset is a character
  if(is.character(assets[1]) & is.character(assets[2])){
    idx1 = grep(assets[1], cnames)
    if(length(idx1) == 0) stop("name for asset1 not in object")
    idx2 = grep(assets[2], cnames)
    if(length(idx2) == 0) stop("name for asset2 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = assets[1]
    idx2 = assets[2]
  }
  
  out = xts(unlist(lapply(EWMA$estimate, function(X) X[idx1, idx2])), as.Date(names(EWMA$estimate)))
  colnames(out) = paste(cnames[idx1], cnames[idx2], sep=".")
  
  return(out)
}


# extract the variance from an mvEWMAcov object
#' EWMA Variance
#' 
#' Extract the Variance of an asset from an \code{EWMA} object
#' 
#' @param object an EWMA object created by \code{EWMA}
#' @param assets character vector or numeric vector. The assets can be 
#' specified by name or index.
#' @export
getVar <- function(EWMA, assets){
  UseMethod("getVar")
}

#' @method getCov mvEWMAcov
#' @S3method getCov mvEWMAcov
getVar.mvEWMAcov <- function(EWMA, assets=1){
  if(!inherits(EWMA, "mvEWMAcov")) stop("EWMA must be of class mvEWMAcov")
  
  cnames <- colnames(EWMA$estimate[[1]])
  
  # Check if asset is a character
  if(is.character(assets[1])){
    idx1 = grep(assets[1], cnames)
    if(length(idx1) == 0) stop("name for asset not in EWMA object")
  } else {
    idx1 = assets[1]
  }
  out = xts(unlist(lapply(EWMA$estimate, function(x) x[idx1])), as.Date(names(EWMA$estimate)))
  colnames(out) = cnames[idx1]
  return(out)
}

# extract the correlation between two assets from an mvEWMAcor object
#' EWMA Correlation
#' 
#' Extract the correlation of two assets from an \code{EWMA} object
#' 
#' @param object an EWMA object created by \code{EWMA}
#' @param assets character vector or numeric vector. The assets can be 
#' specified by name or index.
#' @export
getCor <- function(EWMA, assets){
  UseMethod("getCor")
}

#' @method getCor mvEWMAcor
#' @S3method getCor mvEWMAcor
getCor.mvEWMAcor <- function(EWMA, assets=c(1,2)){
  if(!inherits(EWMA, "mvEWMAcor")) stop("object must be of class mvEWMAcor")
  
  if(length(assets) == 1) assets[2] <- assets[1]
  
  cnames <- colnames(EWMA$estimate[[1]])
  
  # Check if asset is a character
  if(is.character(assets[1]) & is.character(assets[2])){
    idx1 = grep(assets[1], cnames)
    if(length(idx1) == 0) stop("name for asset1 not in object")
    idx2 = grep(assets[2], cnames)
    if(length(idx2) == 0) stop("name for asset2 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = assets[1]
    idx2 = assets[2]
  }
  
  out = xts(unlist(lapply(EWMA$estimate, function(X) X[idx1, idx2])), as.Date(names(EWMA$estimate)))
  colnames(out) = paste(cnames[idx1], cnames[idx2], sep=".")
  
  return(out)
}

# plot methods for
# uvEWMAvol
# uvEWMAcov
# uvEWMAcor
# mvEWMAcov
# mvEWMAcor

#' Plot EWMA Model Estimates
#' 
#' Plot method for EWMA objects
#' 
#' @param x an EWMA object
#' @param y NULL
#' @param \dots additional arguments passed to \code{plot.xts}
#' @param assets for multivariate EWMA objects, character vector or numeric 
#' vector of assets to extract from the covariance or correlation matrix. 
#' The assets can be specified by name or index.
#' @param legendLoc location of legend. If NULL, the legend will be omitted 
#' from the plot
#' @param main main title for the plot
#' @method plot EWMA
#' @S3method plot EWMA
plot.EWMA <- function(x, y=NULL, ..., assets=c(1,2), legendLoc=NULL, main="EWMA Estimate", cexLegend=0.8){
  
  if(inherits(x, "uvEWMAvol") | inherits(x, "uvEWMAcov") | inherits(x, "uvEWMAcor")){
    # uvEWMA has same format
    estValues <- x$estimate
  } else if(inherits(x, "mvEWMAcov") | inherits(x, "mvEWMAcor")){
    # mvEWMA stuff
    if(inherits(x, "mvEWMAcov")){
      estValues <- getCov(x, assets)
    }
    
    if(inherits(x, "mvEWMAcor")){
      estValues <- getCor(x, assets)
    }
  }
  plot.xts(x=estValues, ...=..., type="l", main=main)
  if(!is.null(legendLoc)){
    legend(legendLoc, legend=c("EWMA Estimate"), 
           lty=1, col="black", bty="n", cex=cexLegend)
  }
}
