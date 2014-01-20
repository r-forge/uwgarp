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
#' Retrieves alphas, betas, as well as pvalue and tstats
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
  capm_fit <- lm(R ~ Rmkt)
  capm_fit$x_data <- Rmkt
  capm_fit$y_data <- R
  
  if(ncol(R) > 1){
    #  Multi-Beta CAPM
    class(capm_fit) <- c("capm_mv", "mlm", "lm")
  } else if(ncol(R) == 1){
    #  Univariate CAPM
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
  if(!inherits(object, "capm_mv")) stop("object must be of class capm_mv")
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
  if(!inherits(object, "capm_mv")) stop("object must be of class capm_mv")
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
  # gets t-value, and p-value of model
  return(coef(tmp_sm)[,c(3,4)])
}

#' @method getStatistics capm_mv
#' @S3method getStatistics capm_mv
getStatistics.capm_mv <- function(object){
  if(!inherits(object, "capm_mv")) stop("object must be of class capm_mv")
  # Gets t-value, and p-value of model
  # Multi-Beta CAPM
  x <- coef(summary(object))
  tmp_sm <- do.call(rbind, x)
  holder = holder<-matrix(0,nrow=1,ncol=ncol(coef(object))*2)
  n = 1
  for (i in 1:ncol(coef(object))){
    tmpHolder = cbind(c(paste("alpha.",colnames(coef(object))[i])) ,c(paste("beta.",colnames(coef(object))[i])))
    holder[,n:(i*2)] = tmpHolder
    n = n*2 +1
  }
  rownames(tmp_sm) <- c(holder)
  return(tmp_sm)
}

#' @export for univariate plot
plot.capm_uv <- function(object){
  xlab <- colnames(object$x_data)
  ylab <- colnames(object$y_data)
  plot(x=coredata(object$x_data), y=(object$y_data), xlab=xlab, ylab=ylab, main="CAPM Plot")
  abline(object)
  abline(h=0,v=0,lty=3)
  alpha = coef(summary(object))[1,1]
  a_tstat = coef(summary(object))[1,3]
  beta = coef(summary(object))[2,1]
  b_tstat = coef(summary(object))[2,3]
  legend("topleft", legend=c(paste("alpha =", round(alpha,dig=2),"(", round(a_tstat,dig=2),")"),
                             paste("beta =", round(beta,dig=2),"(", round(b_tstat,dig=2),")")), cex=.8, bty="n")
  
}

#' @export for SML line
plot.capm_mv <- function(object){
  
}