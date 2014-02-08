#' GARCH(1,1)
#' 
#' Description of GARCH(1,1)
#' 
#' @param object GARCH(1,1)
#' @export
# UV N~GARCH(1,1) for each series
garch11 <- function(object){
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                          distribution.model = "norm")

# DCC specification: GARCH(1,1) for conditional cor
dcc.garch11.spec = dccspec(uspec = multispec( replicate(2, garch11.spec) ), 
                           dccOrder = c(1,1), distribution = "mvnorm")
dcc.garch11.spec

dcc.fit = dccfit(dcc.garch11.spec, data = object)
return(dcc.fit)
}

#' Forecast GARCH(1,1)
#' 
#' Description of forecast GARCH(1,1)
#' 
#' @param object a garch11 object created by \code{\link{GARCH(1,1)}}
#' @export
fcstGarch11 <- function(object, window){
  UseMethod("fcstGarch11")
}

#' @method fcstGarch11 Dccfit
#' @S3method fcstGarch11 DCCfit
fcstGarch11.DCCfit <- function(object,window = 100){
  result = dccforecast(garch11, n.ahead=window)
  return(result)
}