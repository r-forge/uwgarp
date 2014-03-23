
#' Bootstrap
#' 
#' Bootstrap a function
#' 
#' @details
#' \code{R} is the data passed to \code{FUN}. \code{FUN} must have \code{x} or
#' \code{R} as arguments for the data. For example, see the functions linked to
#' in the 'See Also' section.
#' 
#' To run the bootstrap in parallael, this function uses the \code{foreach}
#' pacakge. From the \code{\link[foreach]{foreach}} documentation, the 
#' Parallel computation depends upon a parallel backend that must be 
#' registered before performing the computation. The parallel backends 
#' available will be system-specific, but include \code{doParallel}, which uses 
#' R's built-in parallel package, \code{doMC}, which uses the multicore 
#' package, and \code{doSNOW}. Each parallel backend has a specific 
#' registration function, such as \code{registerDoParallel} or 
#' \code{registerDoSNOW}.
#' 
#' @param R xts object or matrix of data passed to \code{FUN}.
#' @param FUN the function to be applied.
#' @param \dots optional arguments to \code{FUN}.
#' @param replications number of bootstrap replications.
#' @param parallel (default FALSE) to compute the bootstrap in parallel.
#' @author Ross Bennett
#' @seealso \code{\link{bootMean}}, \code{\link{bootSD}}, \code{\link{bootStdDev}},
#' \code{\link{bootSimpleVolatility}}, \code{\link{bootCor}}, \code{\link{bootCov}},
#' \code{\link{bootVaR}}, \code{\link{bootES}}
#' @export
bootFUN <- function(R, FUN="mean", ..., replications=1000, parallel=FALSE){
  # R should be a univariate xts object
  
  fun <- match.fun(FUN)
  if(!is.function(fun)) stop("FUN could not be matched")
  
  if(is.function(fun)){
    .formals <- formals(fun)
    # add the dots
    .formals <- modify.args(formals=.formals, ...=..., dots=TRUE)
    .formals$... <- NULL
  }
  # print(.formals)
  
  replications <- as.integer(replications)
  n <- nrow(R)
  out <- vector("numeric", replications)
  
  if(parallel){
    stopifnot("package:foreach" %in% search() || require("foreach",quietly = TRUE))
    out <- foreach(i=1:replications, .inorder=FALSE, .combine=c, .errorhandling='remove') %dopar% {
      tmpR <- R[sample.int(n, replace=TRUE),]
      # match the resampled data to R or x in .formals
      if("R" %in% names(.formals)){ 
        .formals <- modify.args(formals=.formals, arglist=NULL, R=tmpR, dots=TRUE)
      } else if("x" %in% names(.formals)){ 
        .formals <- modify.args(formals=.formals, arglist=NULL, x=tmpR, dots=TRUE)
      }
      do.call(fun, .formals)
    }
  } else {
    for(i in 1:replications){
      # sampled data
      tmpR <- R[sample.int(n, replace=TRUE),]
      # match the resampled data to R or x in .formals
      if("R" %in% names(.formals)){ 
        .formals <- modify.args(formals=.formals, arglist=NULL, R=tmpR, dots=TRUE)
      } else if("x" %in% names(.formals)){ 
        .formals <- modify.args(formals=.formals, arglist=NULL, x=tmpR, dots=TRUE)
      }
      # call the function
      tmp <- try(do.call(fun, .formals), silent=TRUE)
      
      # if try-error, stop the function call, else insert the resampled statistic
      # to the output vector
      if(inherits(tmp, "try-error")){
        stop("FUN could not be evaluated")
      } else {
        out[i] <- tmp
      }
    }
  }
  # compute the expected value of the statistic on resampled data
  mean(out)
}

.bootMean <- function(R, ..., replications=1000, parallel=FALSE){
  # R should be a univariate xts object
  bootFUN(R=R, FUN="mean", ...=..., replications=replications, parallel=parallel)
}

#' Bootstrap Mean
#' 
#' Bootstrap the mean of an xts object of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[base]{mean}}
#' @param replications number of bootstrap replications.
#' @param parallel TRUE/FALSE (default FALSE) to compute the bootstrap in parallel. 
#' @author Ross Bennett
#' @export
bootMean <- function(R, ..., replications=1000, parallel=FALSE){
  if(ncol(R) == 1){
    tmp <- .bootMean(R=R, ...=..., replications=replications, parallel=parallel)
  } else {
    tmp <- vector("numeric", ncol(R))
    for(i in 1:ncol(R)){
      tmp[i] <- .bootMean(R=R[,i], ...=..., replications=replications, parallel=parallel)
    }
  }
  out <- matrix(tmp, nrow=1, ncol=ncol(R))
  rownames(out) <- "mean"
  colnames(out) <- colnames(R)
  return(out)
}

.bootSD <- function(R, ..., replications=1000, parallel=FALSE){
  # R should be a univariate xts object
  bootFUN(R=R, FUN="sd", ...=..., replications=replications, parallel=parallel)
}

#' Bootstrap Standard Deviation
#' 
#' Bootstrap the standard deviation of an xts object of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[stats]{sd}}
#' @param replications number of bootstrap replications.
#' @param parallel TRUE/FALSE (default FALSE) to compute the bootstrap in parallel. 
#' @author Ross Bennett
#' @export
bootSD <- function(R, ..., replications=1000, parallel=FALSE){
  if(ncol(R) == 1){
    tmp <- .bootSD(R=R, ...=..., replications=replications, parallel=parallel)
  } else {
    tmp <- vector("numeric", ncol(R))
    for(i in 1:ncol(R)){
      tmp[i] <- .bootSD(R=R[,i], ...=..., replications=replications, parallel=parallel)
    }
  }
  out <- matrix(tmp, nrow=1, ncol=ncol(R))
  rownames(out) <- "sd"
  colnames(out) <- colnames(R)
  return(out)
}

.bootStdDev <- function(R, ..., replications=1000, parallel=FALSE){
  # R should be a univariate xts object
  bootFUN(R=R, FUN="StdDev", ...=..., replications=replications, parallel=parallel)
}

#' Bootstrap StdDev
#' 
#' Bootstrap the StdDev of an xts object of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[PerformanceAnalytics]{StdDev}}
#' @param replications number of bootstrap replications.
#' @param parallel TRUE/FALSE (default FALSE) to compute the bootstrap in parallel. 
#' @author Ross Bennett
#' @export
bootStdDev <- function(R, ..., replications=1000, parallel=FALSE){
  if(ncol(R) == 1){
    tmp <- .bootStdDev(R=R, ...=..., replications=replications, parallel=parallel)
  } else {
    tmp <- vector("numeric", ncol(R))
    for(i in 1:ncol(R)){
      tmp[i] <- .bootStdDev(R=R[,i], ...=..., replications=replications, parallel=parallel)
    }
  }
  out <- matrix(tmp, nrow=1, ncol=ncol(R))
  rownames(out) <- "StdDev"
  colnames(out) <- colnames(R)
  return(out)
}

.bootSimpleVolatility <- function(R, ..., replications=1000, parallel=FALSE){
  # R should be a univariate xts object
  bootFUN(R=R, FUN="simpleVolatility", ...=..., replications=replications, parallel=parallel)
}

#' Bootstrap Simple Volatility
#' 
#' Bootstrap the simple volatility of an xts object or matrix of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link{SimpleVolatility}}
#' @param replications number of bootstrap replications.
#' @param parallel TRUE/FALSE (default FALSE) to compute the bootstrap in parallel. 
#' @author Ross Bennett
#' @export
bootSimpleVolatility <- function(R, ..., replications=1000, parallel=FALSE){
  if(ncol(R) == 1){
    tmp <- .bootSimpleVolatility(R=R, ...=..., replications=replications, parallel=parallel)
  } else {
    tmp <- vector("numeric", ncol(R))
    for(i in 1:ncol(R)){
      tmp[i] <- .bootSimpleVolatility(R=R[,i], ...=..., replications=replications, parallel=parallel)
    }
  }
  out <- matrix(tmp, nrow=1, ncol=ncol(R))
  rownames(out) <- "SimpleVolatility"
  colnames(out) <- colnames(R)
  return(out)
}

tmpCor <- function(R, ...){
  # R should be a bivariate xts object
  cor(x=R[,1], y=R[,2], ...=...)
}

.bootCor <- function(R, ..., replications=1000, parallel=FALSE){
  # R should be a bivariate xts object
  bootFUN(R=R[,1:2], FUN="tmpCor", ...=..., replications=replications, parallel=parallel)
}

#' Bootstrap Correlation
#' 
#' Bootstrap the correlation of an xts object of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[stats]{cor}}
#' @param replications number of bootstrap replications.
#' @param parallel TRUE/FALSE (default FALSE) to compute the bootstrap in parallel. 
#' @author Ross Bennett
#' @export
bootCor <- function(R, ..., replications=1000, parallel=FALSE){
  if(ncol(R) < 2) stop("R must have 2 or more columns of asset returns")
  cnames <- colnames(R)
  if(ncol(R) == 2){
    tmp <- .bootCor(R=R, ...=..., replications=replications, parallel=parallel)
    out_names <- paste(cnames[1], cnames[2], sep=".")
    num_col <- 1
  } else {
    tmp <- vector("numeric", choose(ncol(R), 2))
    out_names <- vector("numeric", length(tmp))
    num_col <- length(tmp)
    k <- 1
    for(i in 1:(ncol(R)-1)){
      for(j in (i+1):ncol(R)){
        tmp[k] <- .bootCor(R=cbind(R[,i], R[,j]), ...=..., replications=replications, parallel=parallel)
        out_names[k] <- paste(cnames[i], cnames[j], sep=".")
        k <- k + 1
      }
    }
  }
  # out <- matrix(tmp, nrow=1, ncol=ncol(R))
  out <- matrix(tmp, nrow=1, ncol=num_col)
  rownames(out) <- "cor"
  colnames(out) <- out_names
  return(out)
}

tmpCov <- function(R, ...){
  # R should be a bivariate xts object
  cov(x=R[,1], y=R[,2], ...=...)
}

.bootCov <- function(R, ..., replications=1000, parallel=FALSE){
  # R should be a bivariate xts object
  bootFUN(R=R[,1:2], FUN="tmpCov", ...=..., replications=replications, parallel=parallel)
}

#' Bootstrap Covariance
#' 
#' Bootstrap the covariance of an xts object of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[stats]{cov}}
#' @param replications number of bootstrap replications.
#' @param parallel TRUE/FALSE (default FALSE) to compute the bootstrap in parallel. 
#' @author Ross Bennett
#' @export
bootCov <- function(R, ..., replications=1000, parallel=FALSE){
  cnames <- colnames(R)
  if(ncol(R) == 2){
    tmp <- .bootCov(R=R, ...=..., replications=replications, parallel=parallel)
    out_names <- paste(cnames[1], cnames[2], sep=".")
    num_col <- 1
  } else {
    tmp <- vector("numeric", choose(ncol(R), 2))
    out_names <- vector("numeric", length(tmp))
    num_col <- length(tmp)
    k <- 1
    for(i in 1:(ncol(R)-1)){
      for(j in (i+1):ncol(R)){
        tmp[k] <- .bootCov(R=cbind(R[,i], R[,j]), ...=..., replications=replications, parallel=parallel)
        out_names[k] <- paste(cnames[i], cnames[j], sep=".")
        k <- k + 1
      }
    }
  }
  # out <- matrix(tmp, nrow=1, ncol=ncol(R))
  out <- matrix(tmp, nrow=1, ncol=num_col)
  rownames(out) <- "cov"
  colnames(out) <- out_names
  return(out)
}

.bootVaR <- function(R, ..., replications=1000, parallel=FALSE){
  # R should be a univariate xts object
  bootFUN(R=R[,1], FUN="VaR", ...=..., replications=replications, parallel=parallel)
}

#' Bootstrap Value at Risk
#' 
#' Bootstrap the Value at Risk (VaR) of an xts object of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[PerformanceAnalytics]{VaR}}
#' @param replications number of bootstrap replications.
#' @param parallel TRUE/FALSE (default FALSE) to compute the bootstrap in parallel. 
#' @author Ross Bennett
#' @export
bootVaR <- function(R, ..., replications=1000, parallel=FALSE){
  if(ncol(R) == 1){
    tmp <- .bootVaR(R=R, ...=..., replications=replications, parallel=parallel)
  } else {
    tmp <- vector("numeric", ncol(R))
    for(i in 1:ncol(R)){
      tmp[i] <- .bootVaR(R=R[,i], ...=..., replications=replications, parallel=parallel)
    }
  }
  out <- matrix(tmp, nrow=1, ncol=ncol(R))
  rownames(out) <- "VaR"
  colnames(out) <- colnames(R)
  return(out)
}

.bootES <- function(R, ..., replications=1000, parallel=FALSE){
  # R should be a univariate xts object
  bootFUN(R=R, FUN="ES", ...=..., replications=replications, parallel=parallel)
}

#' Bootstrap Expected Shortfall
#' 
#' Bootstrap the Expected Shortfall (ES) of an xts object of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[PerformanceAnalytics]{ES}}
#' @param replications number of bootstrap replications.
#' @param parallel TRUE/FALSE (default FALSE) to compute the bootstrap in parallel. 
#' @author Ross Bennett
#' @export
bootES <- function(R, ..., replications=1000, parallel=FALSE){
  if(ncol(R) == 1){
    tmp <- .bootES(R=R, ...=..., replications=replications, parallel=parallel)
  } else {
    tmp <- vector("numeric", ncol(R))
    for(i in 1:ncol(R)){
      tmp[i] <- .bootES(R=R[,i], ...=..., replications=replications, parallel=parallel)
    }
  }
  out <- matrix(tmp, nrow=1, ncol=ncol(R))
  rownames(out) <- "ES"
  colnames(out) <- colnames(R)
  return(out)
}

# .bootMean <- function(R, replications=1000){
#   replications <- as.integer(replications)
#   n <- length(R)
#   out <- vector("numeric", replications)
#   for(i in 1:replications){
#     out[i] <- mean(R[sample.int(n, replace=TRUE)])
#   }
#   mean(out)
# }
