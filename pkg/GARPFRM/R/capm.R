# CAPM Function 
# Description for CAPM
# @param r risk-free rate
# @param mkrt market return
# @return the function returns tstat upon default & pvalue when specified
# @export
# capm.tstats = function(r,mkrt,type = FALSE) {
#   # Fiting CAPM and retrieve alpha specific tstats or pvalues
#   capm.fit = lm(r~mkrt)    
#   # Extract summary info
#   capm.summary = summary(capm.fit) 
#   if(is.null(type) | type=="pvalue"){
#     # Retrieve p-value if specified
#     p.value = coef(capm.summary)[1,4]  
#     p.value
#   }else{
#     # Otherwise retrieve t-stat if specified or on default
#     t.stat = coef(capm.summary)[1,3]  
#     t.stat
#   }
# }

#' Capital Asset Pricing Model
#' 
#' Description of CAPM
#' bla bla bla
#' 
#' @param R asset returns
#' @param Rmkt market returns
#' @export
#' @examples
#' data(crsp.short)
#' 
#' head(largecap.ts)
#' 
#' Rf <- largecap.ts[, "t90"]
#' R <- largecap.ts[, "CAT"] - Rf
#' MKT <- largecap.ts[, "market"] - Rf
#'
#' # Fit the CAPM model
#' tmp <- CAPM(R=R, Rmkt=MKT)
CAPM <- function(R, Rmkt){
  # We should have a capm_uv class for a univariate capm (i.e. R is the returns
  # of a single asset) and capm_mv for a multivariate capm (i.e. R is the returns
  # for multiple assets)
  
  capm_fit <- lm(R ~ Rmkt)
  capm_fit$x_data <- Rmkt
  capm_fit$y_data <- R
  
  if(ncol(R) > 1){
    #  multivariate capm
    class(capm_fit) <- c("capm_mv", "lm")
  } else if(ncol(R) == 1){
    #  univariate capm
    class(capm_fit) <- c("capm_uv", "lm")
  }
  return(capm_fit)
}

#' CAPM alphas
#' 
#' Description of CAPM alphas
#' 
#' @param object a capm object created by \code{\link{CAPM}}
#' @export
getAlphas <- function(object){
  UseMethod("getAlphas")
}

#' @method getAlphas capm_uv
#' @S3method getAlphas capm_uv
getAlphas.capm_uv <- function(object){
  if(!inherits(object, "capm_uv")) stop("object must be of class capm_uv")
  return(coef(object)[1])
}

#' @method getAlphas capm_mv
#' @S3method getAlphas capm_mv
getAlphas.capm_mv <- function(object){
  if(!inherits(object, "capm_mv")) stop("object must be of class capm_uv")
}

#' CAPM betas
#' 
#' Description of CAPM betas
#' 
#' @param object a capm object created by \code{\link{CAPM}}
#' @export
getBetas <- function(object){
  UseMethod("getBetas")
}

#' @method getBetas capm_uv
#' @S3method getBetas capm_uv
getBetas.capm_uv <- function(object){
  if(!inherits(object, "capm_uv")) stop("object must be of class capm_uv")
  return(coef(object)[2])
}

#' @method getBetas capm_mv
#' @S3method getBetas capm_mv
getBetas.capm_mv <- function(object){
  if(!inherits(object, "capm_mv")) stop("object must be of class capm_uv")
}

#' CAPM statistics
#' 
#' Description of CAPM statistics (standard error, t-values, and p-values)
#' @param object a capm object created by \code{\link{CAPM}}
#' @export
getStatistics <- function(object){
  UseMethod("getStatistics")
}

#' @method getStatistics capm_uv
#' @S3method getStatistics capm_uv
getStatistics.capm_uv <- function(object){
  if(!inherits(object, "capm_uv")) stop("object must be of class capm_uv")
  tmp_sm <- summary.lm(object)
  # gets the standard error, t-value, and p-value of model
  return(coef(tmp_sm)[,2:4])
}

#' @method getStatistics capm_mv
#' @S3method getStatistics capm_mv
getStatistics.capm_uv <- function(object){
  if(!inherits(object, "capm_mv")) stop("object must be of class capm_uv")
  tmp_sm <- summary.lm(object)
  # gets the standard error, t-value, and p-value of model
}

#' @export
plot.capm_uv <- function(object){
  xlab <- colnames(object$x_data)
  ylab <- colnames(object$y_data)
  plot(x=coredata(object$x_data), y=(object$y_data), xlab=xlab, ylab=ylab, main="CAPM Plot")
  abline(object)
}
