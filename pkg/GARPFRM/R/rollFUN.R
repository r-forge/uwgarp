
#' Rolling Covariance Estimate
#' 
#' This function calculates the covariance estimate of the returns of two 
#' assets over a rolling window
#' 
#' @param R xts or zoo object of asset returns
#' @param width width of rolling window
#' @author Ross Bennett
#' @seealso \code{\link{cov}}
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:2]
#' tail(rollCov(R, 10))
#' @export
rollCov <- function(R, width){
  if(!inherits(R, c("xts", "zoo"))) stop("x must be an xts or zoo object")
  n <- nrow(R)
  out <- xts(vector("numeric", n), index(R))
  for(i in width:n){
    tmpR <- R[(i-width+1):i,]
    out[i] <- cov(tmpR[,1], tmpR[,2])
  }
  # pad with leading NA
  for(i in 1:(width-1)){
    out[i] <- NA
  }
  out
}

#' Rolling Correlation Estimate
#' 
#' This function calculates the correlation estimate of the returns of two 
#' assets over a rolling window
#' 
#' @param R xts or zoo object of asset returns
#' @param width width of rolling window
#' @author Ross Bennett
#' @seealso \code{\link{cor}}
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:2]
#' tail(rollCor(R, 10))
#' @export
rollCor <- function(R, width){
  if(!inherits(R, c("xts", "zoo"))) stop("x must be an xts or zoo object")
  n <- nrow(R)
  out <- xts(vector("numeric", n), index(R))
  for(i in width:n){
    tmpR <- R[(i-width+1):i,]
    out[i] <- cor(tmpR[,1], tmpR[,2])
  }
  # pad with leading NA
  for(i in 1:(width-1)){
    out[i] <- NA
  }
  out
}

# rollSD function for a univariate R
.rollSD <- function(R, width){
  # if(!inherits(R, c("xts", "zoo"))) stop("R must be an xts or zoo object")
  # this function should generally not be called by the user and we will check
  # for xts or zoo object in rollSD which calls .rollSD
  n <- length(R)
  # out <- xts(vector("numeric", n), index(R))
  out <- vector("numeric", n)
  for(i in width:n){
    tmpR <- R[(i-width+1):i,1]
    out[i] <- sd(tmpR[,1])
  }
  # pad with leading NA
  for(i in 1:(width-1)){
    out[i] <- NA
  }
  out
}

#' Rolling Standard Deviation Estimate
#' 
#' This function calculates the standard deviation estimate of asset returns 
#' over a rolling window
#' 
#' @param R xts or zoo object of asset returns
#' @param width width of rolling window
#' @author Ross Bennett
#' @seealso \code{\link{sd}}
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:4]
#' tail(rollSD(R, 10))
#' @export
rollSD <- function(R, width){
  if(!inherits(R, c("xts", "zoo"))) stop("R must be an xts or zoo object")
  if(ncol(R) == 1){
    tmp <- .rollSD(R, width)
  } else {
    tmp <- matrix(0, nrow(R), ncol(R))
    for(i in 1:ncol(R)){
      tmp[,i] <- .rollSD(R[,i], width=width)
    }
  }
  out <- xts(tmp, index(R))
  colnames(out) <- colnames(R)
  return(out)
}

# rolling simple volatility estimate for a univariate R
.rollSimpleVolatility <- function(R, width){
  n <- length(R)
  out <- vector("numeric", n)
  for(i in width:n){
    tmpR <- R[(i-width+1):i,1]
    out[i] <- .simpleVolatility(tmpR)
  }
  # pad with leading NA
  for(i in 1:(width-1)){
    out[i] <- NA
  }
  out
}

#' Rolling Simple Volatility Estimate
#' 
#' This function calculates the simple volatility estimate of asset returns 
#' over a rolling window.
#' 
#' The simple volatility of x is defined as
#' 
#' \deqn{
#'   \sigma = \sqrt{\frac{1}{n} \sum_{i=1}^n x_i^2}
#' }
#' 
#' @param R xts or zoo object of asset returns
#' @param width width of rolling window
#' @author Ross Bennett
#' @seealso \code{\link{sd}}, \code{\link{simpleVolatility}}
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:2]
#' tail(rollSimpleVolatility(R, 10))
#' @export
rollSimpleVolatility <- function(R, width){
  if(!inherits(R, c("xts", "zoo"))) stop("R must be an xts or zoo object")
  if(ncol(R) == 1){
    tmp <- .rollSimpleVolatility(R, width)
  } else {
    tmp <- matrix(0, nrow(R), ncol(R))
    for(i in 1:ncol(R)){
      tmp[,i] <- .rollSimpleVolatility(R[,i], width=width)
    }
  }
  out <- xts(tmp, index(R))
  colnames(out) <- colnames(R)
  return(out)
}
