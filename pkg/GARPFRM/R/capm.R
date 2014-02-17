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
#' Retrieves alphas, betas, as well as pvalue and tstats. 
#' The Model is used to determine a theoretically appropriate rate of return
#' of an asset's non-diversifiable risk.
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
    #  Multiple Linear Model CAPM
    class(capm_fit) <- c("capm_mlm", "mlm", "lm")
  } else if(ncol(R) == 1){
    #  Univariate CAPM
    class(capm_fit) <- c("capm_uv", "lm")
  }
  return(capm_fit)
}

#' CAPM alphas
#' 
#' Description of CAPM alphas: retrieves alpha (intercept) from CAPM object.
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
  temp = getStatistics(object)[,1]
  return(temp[1])
}

#' @method getAlphas capm_mlm
#' @S3method getAlphas capm_mlm
getAlphas.capm_mlm <- function(object){
  if(!inherits(object, "capm_mlm")) stop("object must be of class capm_mlm")
  tmp_sm = getStatistics(object)
  tmp_sm = tmp_sm[seq(1,nrow(tmp_sm),2),1]
  return(tmp_sm)
}

#' CAPM betas
#' 
#' Description of CAPM betas: retrieves beta (slope) from CAPM object.
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
  temp = getStatistics(object)[,1]
  return(temp[2])
}

#' @method getBetas capm_mlm
#' @S3method getBetas capm_mlm
getBetas.capm_mlm <- function(object){
  if(!inherits(object, "capm_mlm")) stop("object must be of class capm_mlm")
  tmp_sm = getStatistics(object)
  tmp_sm = tmp_sm[seq(2,nrow(tmp_sm),2),1]
  return(tmp_sm)
}

#' CAPM statistics
#' 
#' Description of CAPM statistics: retrieves standard error, t-values, and p-values
#' 
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
  result = coef(tmp_sm)[,c(1:4)]
  rownames(result) = cbind(c(paste("alpha.", colnames(object$y_data))),c(paste("beta. ", colnames(object$y_data))))
  return(result)
}

#' @method getStatistics capm_mlm
#' @S3method getStatistics capm_mlm
getStatistics.capm_mlm <- function(object){
  if(!inherits(object, "capm_mlm")) stop("object must be of class capm_mlm")
  # Gets t-value, and p-value of model
  # Multi-Beta CAPM
  x <- coef(summary(object))
  tmp_sm <- do.call(rbind, x)
  holder = holder<-matrix(0,nrow=1,ncol=ncol(coef(object))*2)
  n=1
  for (i in 1:ncol(coef(object))){
    holder[,n:(i*2)] = cbind(c(paste("alpha.",colnames(coef(object))[i])) ,c(paste("beta. ",colnames(coef(object))[i])))
    n = i*2 +1
  }
  rownames(tmp_sm) <- c(holder)
  return(tmp_sm)
}

# CAPM plotting for UV
#' @export
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

# CAPM plotting for mlm
#' @export
plot.capm_mlm <- function(object){
  if(ncol(object$y_data) > 4) warning("Only first 4 assets will be graphically displayed")
  par(mfrow=c(2,round(ncol(coef(object))/2)))
  Rmkt = object$x_data
  nbPlot = min(ncol(coef(object)),4)
  for (i in 1:nbPlot){
    tmp = CAPM(object$y_data[,i],Rmkt)
    plot(tmp)
  }
}
#' CAPM SML
#' 
#' Description of CAPM Security Market Line (SML)
#' SML is the represesentation of the CAPM. It illustrates the expected rate of return
#' of an individual secuirty as a function of systematic, non-diversified risk (known as beta).
#' 
#' @param object a capm object created by \code{\link{CAPM}}
#' @export
chartSML <- function(object){
  if(!inherits(object, "capm_mlm")) stop("object must be of class capm_mlm")
  #' Plot expected return versus beta
  mu.hat = colMeans(object$y_data,na.rm=TRUE)
  betas = getBetas(object)
  sml.fit = lm(mu.hat~betas)
  # Plot Fitted SML
  plot(betas,mu.hat,main="Estimated SML")
  abline(sml.fit)
  legend("topleft",1, "Estimated SML",1)                  
}

#' CAPM hypTest
#' 
#' Description of CAPM beta/alpha hypothesis test
#' Generalization is termed a two-sided or two-tailed test. 
#' Returns a true (reject) or false (fail to reject).
#' 
#' @param object a capm object created by \code{\link{CAPM}}
#' @export
hypTest <- function(object,CI){
  UseMethod("hypTest")
}

#' @method hypTest capm_uv
#' @S3method hypTest capm_uv
hypTest.capm_uv <- function(object,CI = 0.05){
  if(!inherits(object, "capm_uv")) stop("object must be of class capm_uv")
  tmp_sm = getStatistics(object)
  tmp_A = tmp_sm[1,3] < CI
  tstat = (tmp_sm[2,2] - 1 )/tmp_sm[2,3]
  #' Two sided t-test
  tmp_B = (2*(1 - pt(abs(tstat),df=nrow(object$x_data)-1))) < CI
  result = list(alpha = tmp_A, beta = tmp_B)
  return(result)
}

#' @method hypTest capm_mlm
#' @S3method hypTest capm_mlm
hypTest.capm_mlm <- function(object,CI = 0.05){
  if(!inherits(object, "capm_mlm")) stop("object must be of class capm_mlm")
  tmp_sm = getStatistics(object)
  tmp_A = tmp_sm[seq(1,nrow(tmp_sm),2),4] < CI
  tstat = (tmp_sm[seq(2,nrow(tmp_sm),2),1] - 1 )/tmp_sm[seq(2,nrow(tmp_sm),2),2]
  #' Two sided t-test
  tmp_B = (2*(1 - pt(abs(tstat),df=nrow(object$x_data)-2))) < CI
  result = list(alpha = tmp_A, beta = tmp_B)  
  return(result)
}