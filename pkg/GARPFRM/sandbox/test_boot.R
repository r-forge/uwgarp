# bootstrap

data(crsp_weekly)
R <- largecap_weekly[,1:4]
R1 <- R[1:100,1]

set.seed(123)
bootFUN(R1, FUN="mean", replications=10000, parallel=FALSE)
set.seed(123)
bootFUN(R1, FUN="mean", replications=10000, parallel=TRUE)

# arbitrary function 
foo <- function(R, n){
  R <- tail(R, n)
  Return.annualized(R, geometric=TRUE)
}

bootFUN(R1, FUN="foo", n=100, replications=100)

# bootstrap various statistics
# mean
bootMean(R[,1])
bootMean(R)

# sd
bootSD(R[,1])
bootSD(R)

# StdDev
bootStdDev(R[,1])
bootStdDev(R)

# simpleVolatility
bootSimpleVolatility(R[,1])
bootSimpleVolatility(R)

# cor
bootCor(R[,1:2])
bootCor(R[,1:2], method="kendall")
bootCor(R)

# cov
bootCov(R[,1:2])
bootCov(R)

# VaR
bootVaR(R[,1], p=0.9, method="historical")
bootVaR(R[,1], p=0.9, method="gaussian")
bootVaR(R, p=0.9, method="historical", invert=FALSE)

# ES
bootES(R[,1], p=0.9, method="gaussian")
bootES(R[,1], p=0.92, method="historical", invert=FALSE)
bootES(R, p=0.9, method="historical")


# foo1 <- function(x){
#   # Use sample.int and subset
#   x[sample.int(length(x), replace=TRUE)]
# }

# foo2 <- function(x){
#   # sample directly from the returns
#   sample(x, length(x), replace=TRUE)
# }

# which is faster?

# set.seed(123)
# x <- rnorm(1e6)
# rbenchmark::benchmark(foo1(R[,1]), foo2(R[,1]), replications=1e5)

# foo1 is slightly faster