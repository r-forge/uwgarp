# EWMA <- function(object, lambda=0.96, cor=FALSE) {    
#     if ((lambda<1 || lambda > 0)){
#       object.names  = colnames(object)
#       t.object      = nrow(object)
#       k.object      = ncol(object)
#       object        = as.matrix(object)
#       t.names       = rownames(object)
#      
#       covEWMA = array(,c(t.object,k.object,k.object))
#       # Note it is unconditional cov
#       cov.f = var(object)
#       FF = (object[1,]- mean(object)) %*% t(object[1,]- mean(object))
#       covEWMA[1,,] = (1-lambda)*FF  + lambda*cov.f
#       for (i in 2:t.object) {
#         FF = (object[i,]- mean(object)) %*% t(object[i,]- mean(object))
#         print(FF)
#         covEWMA[i,,] = (1-lambda)*FF  + lambda*covEWMA[(i-1),,]
#       }
#       
#     } else {
#       stop("exp-decay lambda must be ]0:1[") 
#     }
# 
#     dimnames(covEWMA) = list(t.names, object.names, object.names)
#     
#     if(cor) {
#         corEWMA = covEWMA
#       for (i in 1:dim(corEWMA)[1]) {
#         corEWMA[i, , ] = cov2cor(covEWMA[i, ,])
#       }
#       return(corEWMA)
#     } else{
#       return(covEWMA)  
#     }
#   }
#' Exponential Weighted Moving Average (EWMA)
#' 
#' Description of EWMA. The function handles UV and MLM objects and returns either cov/cor.
#' 
#' @param R
#' @param lambda
#' @param initialWindow is the initializing window
#' @param correlation option (cor by default = FALSE) 
#' @export
EWMA <- function(R, lambda=0.94, initialWindow=10, cor=FALSE){
  # Check for lambda between 0 and 1 & initialWindow must be greater than ncol(R)
  if (((lambda>1 || lambda < 0))) {stop("For exponential decay lambda must belong to ]0:1[")}
  if (initialWindow> nrow(R)){stop("Initialization window is too large")}
  
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
  } else if(cor & ncol(R)==1) {
    stop("EWMA correlation is only to be estimated for two or more assets")
  }
  
  # Check for Covar or Var
  if((cor == FALSE) & (ncol(R) > 1)) { 
    class(out) <- c("EWMACovar")
  } else if ((cor == FALSE) & (ncol(R) == 1)){
    class(out) <- c("EWMAVar")
  }
  # Bind initial data to EWMA object in order to plot a comparison
  # out$y_data <- R
  # out$y_data <- R adds R to the list element
  # The EWMA estimate and R should be separate elements in the list returned
  return(out)
}

#' EWMA Volatility/Cross-Volatility
#' 
#' Description of EWMA Vola
#' 
#' @param object a EWMA object created by \code{\link{EWMA}}
#' @export
getCov <- function(object, asset1, asset2){
  UseMethod("getCov")
}

#' @method getCov EWMACovar
#' @S3method getCov EWMACovar
getCov.EWMACovar <- function(object, asset1, asset2){
  if(!inherits(object, "EWMACovar")) stop("object must be of class EWMACovar")
  # Manipulate object for feasible use   
  # object[[length(object)]] = NULL
  
  # Check if asset is a character
  if(is.character(asset1) & is.character(asset2)){
    idx1 = grep(asset1, colnames(object$EWMA[[1]]))
    if(length(idx1) == 0) stop("name for asset1 not in object")
    idx2 = grep(asset2, colnames(object$EWMA[[1]]))
    if(length(idx2) == 0) stop("name for asset2 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = asset1
    idx2 = asset2
  }
  
  out = xts(unlist(lapply(object$EWMA, function(X) X[idx1, idx2])), as.Date(names(object$EWMA)))
  colnames(out) = paste(asset1, asset2, sep=".")
  
  return(out)
}

#' @method getCov EWMAVar
#' @S3method getCov EWMAVar
getCov.EWMAVar <- function(object, asset1, asset2){
  if(!inherits(object, "EWMAVar")) stop("object must be of class EWMAVar")
  if (is.null(asset2) == FALSE) {warning("Running univariate EWMA leave asset2 unspecified")}
  # Manipulate object for feasible use  
  # object[[length(object)]] = NULL
  
  # Check if asset is a character
  if(is.character(asset1)){
    idx1 = grep(asset1, colnames(object$EWMA[[1]]))
    if(length(idx1) == 0) stop("name for asset1 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = asset1
  }
  out = xts(unlist(lapply(object$EWMA, function(x) x[idx1])), as.Date(names(object$EWMA)))
  colnames(out) = asset1
  
  return(out)
}

#' EWMA Correlation
#' 
#' Description of EWMA Correlation, requires two assets
#' 
#' @param object a EWMA object created by \code{\link{EWMA}}
#' @export
getCor <- function(object, asset1, asset2){
  UseMethod("getCor")
}

#' @method getCor EWMACor
#' @S3method getCor EWMACor
getCor.EWMACor <- function(object, asset1, asset2){
  if(!inherits(object, "EWMACor")) stop("object must be of class EWMACor")
  # Manipulate object for feasible use  
  # object[[length(object)]] = NULL
  
  # Check if asset is a character 
  if(is.character(asset1) & is.character(asset2)){
    idx1 = grep(asset1, colnames(object$EWMA[[1]]))
    if(length(idx1) == 0) stop("name for asset1 not in object")
    idx2 = grep(asset2, colnames(object$EWMA[[1]]))
    if(length(idx2) == 0) stop("name for asset2 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = asset1
    idx2 = asset2
  }
  
  out = xts(unlist(lapply(object$EWMA, function(x) x[idx1, idx2])), as.Date(names(object$EWMA)))
  colnames(out) = paste(asset1, asset2, sep=".")
  
  return(out)
}

# The generic method for plot is
# plot(x, y, ...)
# The first arguments for your plot.* methods must match the generic plot method

# plot.EWMACovar(x, y, ..., asset1, asset2)

# EWMA plotting for covar
#' @export
plot.EWMACovar <- function(object, asset1, asset2){
  # Check if asset is a character 
  if(is.character(asset1) & is.character(asset2)){
    idx1 = grep(asset1, colnames(object[[1]]))
    if(length(idx1) == 0) stop("name for asset1 not in object")
    idx2 = grep(asset2, colnames(object[[1]]))
    if(length(idx2) == 0) stop("name for asset2 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = asset1
    idx2 = asset2
  }
  tmp = getCov(object,..., asset1, asset2)
  plot(x=time(as.zoo(tmp)), y=tmp, type="l", xlab="Time", ylab="Covariance", lwd=2, col="blue",
       main="EWMA Covariance");
  grid()
  abline(h=var(object$R)[idx1,idx2], lwd=2, col="red")
}


# EWMA plotting for var
#' @export
plot.EWMAVar <- function(object,...,asset1){
  tmp = getCov(object,asset1)
  plot(x=time(as.zoo(tmp)),y=tmp, type="l", xlab="Time", ylab="Variance", lwd=2, col="blue",
       main="EWMA Variance");
  grid()
  abline(h=var(object$R), lwd=2, col="red")
}


# EWMA plotting for correlation
#' @export
plot.EWMACor <- function(object, ...,asset1, asset2){
  # Check if asset is a character 
  if(is.character(asset1) & is.character(asset2)){
    idx1 = grep(asset1, colnames(object[[1]]))
    if(length(idx1) == 0) stop("name for asset1 not in object")
    idx2 = grep(asset2, colnames(object[[1]]))
    if(length(idx2) == 0) stop("name for asset2 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = asset1
    idx2 = asset2
  }
  tmp = getCor(object, asset1, asset2)
  plot(x=time(as.zoo(tmp)), y=tmp, type="l", xlab="Time", ylab="Correlation", lwd=2, col="blue",
       main="EWMA Correlation");
  grid()
  abline(h=cor(object$R)[idx1,idx2], lwd=2, col="red")
}
