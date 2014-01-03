#' CAPM Function
#' 
#' Description for CAPM
#' @param r risk-free rate
#' @param mkrt market return
#' @return the function returns tstat upon default & pvalue when specified
#' @export
capm.tstats = function(r,mkrt,type = FALSE) {
  # Fiting CAPM and retrieve alpha specific tstats or pvalues
  capm.fit = lm(r~mkrt)    
  # Extract summary info
  capm.summary = summary(capm.fit) 
  if(is.null(type) | type=="pvalue"){
    # Retrieve p-value if specified
    p.value = coef(capm.summary)[1,4]  
    p.value
  }else{
    # Otherwise retrieve t-stat if specified or on default
    t.stat = coef(capm.summary)[1,3]  
    t.stat
  }
}