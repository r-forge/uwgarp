#' Backtesting VaR (backTestVaR)
#' 
#' Description of backTestVaR. The function should handle UV and MLM.
#' 
#' @param R returns
#' @param CI confidence level
#' @export
backTestVaR <- function(R, CI = 0.95) {
  if (ncol(R)>1){stop("One Asset at a time")}
  normalVaR = as.numeric(VaR(R, p=CI, method="gaussian")) 
  historicalVaR = as.numeric(VaR(R, p=CI, method="historical")) 
  modifiedVaR = as.numeric(VaR(R, p=CI, method="modified"))
  result = c(normalVaR, historicalVaR, modifiedVaR)
  names(result) = c("Normal", "HS", "Modified")  
  
  return(result)
}
