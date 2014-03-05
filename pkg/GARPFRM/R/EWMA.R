
#' Exponential Weighted Moving Average (EWMA)
#' 
#' Use an exponentially weighted moving average to estimate the covariance or 
#' correlation of asset returns.
#' 
#' @param R asset returns
#' @param lambda smoothing parameter, must be greater than 0 or less than 1
#' @param initialWindow initial window of observations used in estimating the 
#' initial covariance or correlation
#' @param TRUE/FALSE to return a correlation matrix. Default cor = FALSE.
#' @export
EWMA <- function(R, lambda=0.94, initialWindow=10, cor=FALSE){
  # Check for lambda between 0 and 1 & initialWindow must be greater than ncol(R)
  if ((lambda > 1) || (lambda < 0)) stop("lambda must be greater than 0 or less than 1")
  if (initialWindow > nrow(R)) stop("Initial window must be less than the number of observations")
  
  # Separate data into a initializing window and a testing window
  initialR = R[1:initialWindow,]
  testR = R[(initialWindow+1):nrow(R),]
  
  # Initialization of covariance matrix
  lagCov = cov(initialR)
  covTmp = vector("list", nrow(testR))
  for(i in 1:nrow(testR)){
    # Extract R for the ith time step
    tmpR = testR[i,]
    covTmp[[i]] = lambda * (t(tmpR)%*%tmpR) + (1 - lambda) * lagCov
    # Update lagCov to be covTmp from the current period
    lagCov <- covTmp[[i]]
  }
  # Final estimated EWMA of covariance
  estEWMA <- covTmp
  # Properly assign list key to date
  names(estEWMA) <- index(testR)
  
  # Add R as a separate element to object to return
  out <- list(EWMA=estEWMA, R=R)
  
  # Check correlation option
  if(cor & ncol(R) > 1) {
    out$EWMA <- lapply(out$EWMA, cov2cor)
    class(out) <- c("EWMACor")
  } else if(cor & (ncol(R) == 1)) {
    stop("EWMA correlation is only to be estimated for two or more assets")
  }
  
  # Check for Covar or Var
  if((cor == FALSE) & (ncol(R) > 1)) { 
    class(out) <- c("covEWMA")
  } else if ((cor == FALSE) & (ncol(R) == 1)){
    class(out) <- c("varEWMA")
  }
  return(out)
}

#' EWMA Covariance
#' 
#' Extract the covariance between two assets from an EWMA object
#' 
#' @param EWMA an EWMA object created by \code{\link{EWMA}}
#' @param assets character vector or numeric vector. If 
#' \code{assets} is of length 1, then the variance will be returned. 
#' The assets can be specified by name or index.
#' @export
getCov <- function(EWMA, assets){
  UseMethod("getCov")
}

#' @method getCov covEWMA
#' @S3method getCov covEWMA
getCov.covEWMA <- function(EWMA, assets=c(1,2)){
  if(!inherits(EWMA, "covEWMA")) stop("object must be of class covEWMA")
  
  # Check if asset is a character
  if(is.character(assets[1]) & is.character(assets[2])){
    idx1 = grep(assets[1], colnames(EWMA$EWMA[[1]]))
    if(length(idx1) == 0) stop("name for asset1 not in object")
    idx2 = grep(assets[2], colnames(EWMA$EWMA[[1]]))
    if(length(idx2) == 0) stop("name for asset2 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = assets[1]
    idx2 = assets[1]
  }
  
  out = xts(unlist(lapply(EWMA$EWMA, function(X) X[idx1, idx2])), as.Date(names(EWMA$EWMA)))
  colnames(out) = paste(assets[1], assets[2], sep=".")
  
  return(out)
}

#' @method getCov varEWMA
#' @S3method getCov varEWMA
getCov.varEWMA <- function(EWMA, assets=1){
  if(!inherits(EWMA, "varEWMA")) stop("EWMA must be of class varEWMA")
  
  # Check if asset is a character
  if(is.character(assets[1])){
    idx1 = grep(assets[1], colnames(EWMA$EWMA[[1]]))
    if(length(idx1) == 0) stop("name for asset not in EWMA object")
  } else {
    idx1 = assets[1]
  }
  out = xts(unlist(lapply(EWMA$EWMA, function(x) x[idx1])), as.Date(names(EWMA$EWMA)))
  colnames(out) = assets[1]
  return(out)
}

#' EWMA Correlation
#' 
#' Extract the correlation of two assets from an EWMA object
#' 
#' @param object an EWMA object created by \code{\link{EWMA}}
#' @export
getCor <- function(EWMA, assets){
  UseMethod("getCor")
}

#' @method getCor corEWMA
#' @S3method getCor corEWMA
getCor.corEWMA <- function(EWMA, assets=c(1,2)){
  if(!inherits(EWMA, "corEWMA")) stop("EWMA must be of class corEWMA")
  
  # Check if asset is a character 
  if(is.character(assets[1]) & is.character(assets[2])){
    idx1 = grep(assets[1], colnames(EWMA$EWMA[[1]]))
    if(length(idx1) == 0) stop("name for asset1 not in EWMA object")
    idx2 = grep(assets[2], colnames(EWMA$EWMA[[1]]))
    if(length(idx2) == 0) stop("name for asset2 not in EWMA object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = assets[1]
    idx2 = assets[2]
  }
  
  out = xts(unlist(lapply(EWMA$EWMA, function(x) x[idx1, idx2])), as.Date(names(EWMA$EWMA)))
  colnames(out) = paste(assets[1], assets[2], sep=".")
  
  return(out)
}

# The generic method for plot is
# plot(x, y, ...)
# The first arguments for your plot.* methods must match the generic plot method

# plot.EWMACovar(x, y, ..., asset1, asset2)

# EWMA plotting for covar
#' @export
plot.covEWMA <- function(object, ..., assets=c(1, 2)){
  # Check if asset is a character 
  if(is.character(assets[1]) & is.character(assets[2])){
    idx1 = grep(assets[1], colnames(object[[1]]))
    if(length(idx1) == 0) stop("name for assets[1] not in object")
    idx2 = grep(assets[2], colnames(object[[1]]))
    if(length(idx2) == 0) stop("name for assets[2] not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = assets[1]
    idx2 = assets[2]
  }
  tmp = getCov(object, ..., assets)
  plot(x=time(as.zoo(tmp)), y=tmp, type="l", xlab="Time", ylab="Covariance", 
       lwd=2, col="blue", main="EWMA Covariance")
  grid()
  abline(h=var(object$R)[idx1,idx2], lwd=2, col="red")
}


# EWMA plotting for var
#' @export
plot.varEWMA <- function(object, ..., assets=c(1,2)){
  tmp = getCov(object, assets[1])
  plot(x=time(as.zoo(tmp)),y=tmp, type="l", xlab="Time", ylab="Variance", 
       lwd=2, col="blue", main="EWMA Variance");
  grid()
  abline(h=var(object$R), lwd=2, col="red")
}


# EWMA plotting for correlation
#' @export
plot.corEWMA <- function(object, ..., assets=c(1,2)){
  # Check if asset is a character 
  if(is.character(assets[1]) & is.character(assets[2])){
    idx1 = grep(assets[1], colnames(object[[1]]))
    if(length(idx1) == 0) stop("name for asset1 not in object")
    idx2 = grep(assets[2], colnames(object[[1]]))
    if(length(idx2) == 0) stop("name for asset2 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = assets[1]
    idx2 = assets[2]
  }
  tmp = getCor(object, assets)
  plot(x=time(as.zoo(tmp)), y=tmp, type="l", xlab="Time", ylab="Correlation", 
       lwd=2, col="blue", main="EWMA Correlation")
  grid()
  abline(h=cor(object$R)[idx1,idx2], lwd=2, col="red")
}
