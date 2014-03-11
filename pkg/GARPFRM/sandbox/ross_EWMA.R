
##### testing #####
library(GARPFRM)

# data and parameters for EWMA estimate
data(crsp_weekly)
R <- largecap_weekly[, 1:2]
mvR <- largecap_weekly[,1:4]
lambda <- 0.94
initialWindow <- 15

# volatility estimate of univariate data
lambda <- estimateLambdaVol(R[,1], initialWindow, n=10)
vol1 <- EWMA(R[,1], lambda=NULL, initialWindow, n=10, "volatility")
vol1a <- EWMA(R[,1], lambda, initialWindow, n=10, "volatility")
all.equal(vol1$estimate, vol1a$estimate)
vol1
plot(vol1)

# Calculate realized volatility
# realizedVol(R, 10)

# covariance estimate of bivariate data
lambda <- 0.94
cov1 <- EWMA(R, lambda, initialWindow, type="covariance")
cov1a <- uvEWMAcov(R, lambda, initialWindow)
all.equal(cov1$estimate, cov1a)

# Calculate realized covariance
# realizedCov(R, 10)

# correlation estimate of bivariate data
cor1 <- EWMA(R, lambda, initialWindow, type="correlation")
cor1a <- uvEWMAcor(R, lambda, initialWindow)
all.equal(cor1$estimate, cor1a)

# Calcualte realized correlation
# realizedCor(R, 10)

cov_mv <- EWMA(mvR, lambda, initialWindow, type="covariance")
cov_mv
tail(getCov(cov_mv, assets=c(1,2)))
plot(cov_mv)

cor_mv <- EWMA(mvR, lambda, initialWindow, type="correlation")
tail(getCor(cor_mv, assets=c(1,2)))
cor_mv
plot(cor_mv)

# cor.fun <- function(x){
#   cor(x)[1,2]
# }
# 
# cov.fun <- function(x){
#   print(cov(x))
#   cov(x)[1,2]
# }
# 
# cor.fun(R)
# cor(R)
# 
# cov(R)[1,2]
# 
# rollapply(data=R, x=R[,1], y=R[,2], width=10, FUN=cov)
# rollapply(data=R, width=10, FUN=cov.fun)
