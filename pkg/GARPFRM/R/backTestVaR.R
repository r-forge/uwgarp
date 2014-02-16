#' Backtesting VaR (backTestVaR)
#' 
#' Description of backTestVaR. The function should handle UV and MLM.
#' 
#' @param R returns
#' @param p confidence level
#' @export
backTestVaR <- function(R, p = 0.95) {
  normalVaR = as.numeric(VaR(R, p=p, method="gaussian")) 
  historicalVaR = as.numeric(VaR(R, p=p, method="historical")) 
  modifiedVaR = as.numeric(VaR(R, p=p, method="modified"))
  result = c(normalVaR, historicalVaR, modifiedVaR)
  names(result) = c("Normal", "HS", "Modified")
  return(result)
}