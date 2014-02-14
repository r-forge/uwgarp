#' GARCH(1,1)
#' 
#' Description of GARCH(1,1)
#' 
#' @param R GARCH(1,1)
#' @param model “sGARCH”, “fGARCH”, “eGARCH”, “gjrGARCH”, “apARCH” and “iGARCH” and “csGARCH”
#' @param distribution.model. Valid choices are “norm” for the normal distibution, “snorm” for the skew-normal distribution, “std” for the student-t, “sstd” for the skew-student, “ged” for the generalized error distribution, “sged” for the skew-generalized error distribution, “nig” for the normal inverse gaussian distribution, “ghyp” for the Generalized Hyperbolic, and “jsu” for Johnson's SU distribution. 
#' @export
# By default we use UV N~GARCH(1,1) and Bollerslev for each series
garch11 <- function(R, model = "sGARCH", distribution.model = "norm"){
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), model = model), 
                          distribution.model)

# DCC specification: GARCH(1,1) for conditional cor
nbColumns = ncol(R)
dcc.garch11.spec = dccspec(uspec = multispec( replicate(nbColumns, garch11.spec) ), 
                           dccOrder = c(1,1), distribution = "mvnorm")
dcc.garch11.spec

dcc.fit = dccfit(dcc.garch11.spec, data = R)
class(dcc.fit)
slotNames(dcc.fit)
names(dcc.fit@mfit)
names(dcc.fit@model)
return(dcc.fit)
}

#' Forecast GARCH(1,1)
#' 
#' Description of forecast GARCH(1,1)
#' 
#' @param garch11 object created by \code{\link{GARCH(1,1)}}
#' @param window is the forecast window (default is set to window = 100)
#' @export
fcstGarch11 <- function(object, window){
  UseMethod("fcstGarch11")
}

#' @method fcstGarch11 Dccfit
#' @S3method fcstGarch11 DCCfit
fcstGarch11.DCCfit <- function(object, window = 100){
  #if ((window > nrow(object))) {stop("Window is too large to forecast")}
  result = dccforecast(garch11, n.ahead=window)
  class(result)
  slotNames(result)
  class(result@mforecast)
  names(result@mforecast)
  return(result)
}