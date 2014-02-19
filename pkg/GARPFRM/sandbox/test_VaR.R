library(GARPFRM)
data(crsp.short)
R <- largecap.ts[, 1:4]
temp = R[,1]
# BackTesting Window
initialWindow = 10
testWindow = nrow(R) -initialWindow
CI = 0.95
lags = -1
resultVaR = rollapply(temp, width= initialWindow, FUN = backTestVaR, p=CI, by.column = FALSE, align = "right")
# VaR lags original data by definition
resultVaR = lag(resultVaR, k=lags)
# Chart together
resultVaR = xts(resultVaR, index(R))
temp = xts(temp, index(temp))

chart.TimeSeries(cbind(resultVaR,temp), legend.loc="topright")

# violations.mat = matrix(0, 3, 5)
# rownames(violations.mat) = c("Normal", "HS", "Modified")
# colnames(violations.mat) = c("En1", "n1", "1-CI", "Percent", "VaR")
# violations.mat[, "En1"] = (1-CI)*testWindow
# violations.mat[, "1-CI "] = 1 - CI 
