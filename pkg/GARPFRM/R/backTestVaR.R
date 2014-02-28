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

#' Count backtesting VaR
#' 
#' Description of countBacktesting VaR
#' 
#' @param backTestVaR object created by \code{\link{backTestVaR}}
#' @param initialWindow
#' @param CI
#' @param temp 
#' @export
countViolations <- function(object, temp, initialWindow, CI){
  UseMethod("countViolations")
}

#' @method countViolations xts
#' @S3method countViolations xts
countViolations.xts <- function(object, temp, initialWindow=10, CI=0.95){
  violations = matrix(0, 3, 5)
  testWindow = nrow(temp) -initialWindow
  rownames(violations) = c("Normal", "HS", "Modified")
  colnames(violations) = c("En1", "n1", "1-CI", "Percent", "VaR")
  violations[, "En1"] = (1-CI)*initialWindow
  violations[, "1-CI"] = 1 - CI
  
  for(i in colnames(object)) {
    violationVaR = temp[index(object), ] < object[, i]
    violations[i, "n1"] = sum(violationVaR, na.rm= TRUE)
    violations[i, "Percent"] = sum(violationVaR, na.rm=TRUE)/testWindow
    violations[i, "VaR"] = violations[i, "n1"]/violations[i, "En1"]
  }
  return(violations)
}