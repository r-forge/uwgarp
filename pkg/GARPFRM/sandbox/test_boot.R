# bootstrap

data(crsp_weekly)
R <- largecap_weekly[,1:4]
R1 <- R[1:100,1]

set.seed(123)
bootFUN(R1, FUN="mean", replications=10000, parallel=FALSE)
set.seed(123)
bootFUN(R1, FUN="mean", replications=10000, parallel=TRUE)

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
bootCor(R)

# cov
bootCov(R[,1:2])
bootCov(R)

# VaR
bootVaR(R[,1], p=0.9, method="historical")
bootVaR(R[,1], p=0.9, method="gaussian")
bootVaR(R, p=0.9, method="historical")

# ES
bootES(R[,1], p=0.9, method="historical")
bootES(R, p=0.9, method="historical")


# maybe...
# use bootstrapped returns to imply the prices

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