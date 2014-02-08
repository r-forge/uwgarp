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
#' @param inWnd
#' @param cor option (default = FALSE) 
#' @export
EWMA <- function(R, lambda=0.94, inWnd=10, cor=FALSE){
  # I really don't like the inWnd name, it is not informative or intuitive
  # It should be more descriptive like starting_period
  
  # These checks should be evaluated separately and not part of a loop
  if(lambda > 1 | lambda < 0) stop("lambda must be in [0, 1]")
  
  if(inWnd < ncol(R)) stop("inWnd must be greater than number of assets in R")
  
  # Separate data into a initializing window and a testing window
  inR = R[1:inWnd,]
  testR = R[(inWnd+1):nrow(R),]
  
  # Initialization of covariance matrix
  lagCov = cov(inR)
  covTmp = vector("list", nrow(testR))
  for(i in 1:nrow(testR)){
    # Extract R for the ith time step
    tmpR = testR[i,]
    covTmp[[i]] = lambda * (t(tmpR)%*%tmpR) + (1 - lambda) * lagCov
    # Update lagCov to be covTmp from the current period
    lagCov <- covTmp[[i]]
  }
  est <- covTmp
  # Properly assign list key to date
  names(est) <- index(testR)
  
  # Check correlation option
  if(cor & ncol(R)>1) {out <- lapply(out, cov2cor)
  class(out) <- c("EWMACor")
  }else if(cor & ncol(R)==1) {stop("EWMA correlation is only to be estimated for two or more assets")}
  
  if(cor == FALSE & ncol(R) > 1) { class(out) <- c("EWMACovar")
  } else if (cor == FALSE & ncol(R) == 1){class(out) <- c("EWMAVar")}
  out$y_data <- R
  return(out)
  
  } else {
           stop("For exponential decay lambda must belong to ]0:1[ and/or window is too large") 
  }
  # The final line of any function should be the return
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
  
  # object[[length(object)]] = NULL
  
  # Get the EWMA estimate from the object
  ewma_estimate <- object$EWMA
  
  # Check if asset is a character 
  if(is.character(asset1) & is.character(asset2)){
    idx1 = grep(asset1, colnames(ewma_estimate[[1]]))
    if(length(idx1) == 0) stop("name for asset1 not in object")
    idx2 = grep(asset2, colnames(ewma_estimate[[1]]))
    if(length(idx2) == 0) stop("name for asset2 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = asset1
    idx2 = asset2
  }
  out = xts(unlist(lapply(ewma_estimate, function(x) x[idx1, idx2])), as.Date(names(ewma_estimate)))
  colnames(out) = paste(asset1, asset2, sep=".")
  return(out)
}

#' @method getCov EWMAVar
#' @S3method getCov EWMAVar
getCov.EWMAVar <- function(object, asset1){
  if(!inherits(object, "EWMAVar")) stop("object must be of class EWMAVar")
  
  # object[[length(object)]] = NULL
  
  # Get the EWMA estimate from the object
  ewma_est <- object$EWMA
  
  # Check if asset is a character
  if(is.character(asset1)){
    idx1 = grep(asset1, colnames(ewma_est[[1]]))
    if(length(idx1) == 0) stop("name for asset1 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = asset1
  }
  out = xts(unlist(lapply(ewma_est, function(object) object[,idx1])), as.Date(names(ewma_est)))
  colnames(out) = paste(asset1, sep=".")
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
  # Check if asset is a character 
  object[[length(object)]] = NULL
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
  out = xts(unlist(lapply(object, function(object) object[idx1, idx2])), as.Date(names(object)))
  colnames(out) = paste(asset1, asset2, sep=".")
  return(out)
}


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
  tmp = getCov(object, asset1, asset2)
  plot(x=time(as.zoo(tmp)), y=tmp, type="l", xlab="Time", ylab="Covariance", lwd=2, col="blue",
       main="EWMA Covariance");
  grid()
  abline(h=var(object$R)[idx1,idx2], lwd=2, col="red")
}

# EWMA plotting for var
#' @export
plot.EWMAVar <- function(object,asset1){
  tmp = getCov(object, asset1)
  plot(x=time(as.zoo(tmp)),y=tmp, type="l", xlab="Time", ylab="Variance", lwd=2, col="blue",
       main="EWMA Variance");
  grid()
  abline(h=var(object$R), lwd=2, col="red")
}

# EWMA plotting for correlation
#' @export
plot.EWMACor <- function(object, asset1, asset2){
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
  tmp = getCor(object,asset1, asset2)
  plot(x=time(as.zoo(tmp)), y=tmp, type="l", xlab="Time", ylab="Correlation", lwd=2, col="blue",
       main="EWMA Correlation");
  grid()
  abline(h=cor(object$y_data)[idx1,idx2], lwd=2, col="red")
}

