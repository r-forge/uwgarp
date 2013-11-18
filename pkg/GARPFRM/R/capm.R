#' CAPM Function
#' 
#' Description for CAPM
#' @param rf risk-free rate
#' @param mkrt
#' @return this is what the function returns
#' @export
capm.tstats = function(rf,mkrt) {
  # Fiting CAPM
  capm.fit = lm(rf~mkrt)  	
  # Extract summary info
  capm.summary = summary(capm.fit)		
  # Retrieve t-stat
  t.stat = coef(capm.summary)[1,3]	
  t.stat
}