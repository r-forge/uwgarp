# EWMA Demo

library(GARPFRM)
data(crsp.short)

# Use the first 5 assets in largecap.ts for the returns data
R <- largecap.ts[, 1:5]

# Estimate the covariance matrix via EWMA
covEst <- EWMA(R, 0.94, 15)
names(covEst)
covEst

# get the covariance between AMAT and CAT
covAMATCAT <- getCov(covEst, assets=c("AMAT", "CAT"))
cov13 <- getCov(covEst, assets=c(1, 3))
all.equal(covAMATCAT, cov13)

# Plot the covariance estimate between AMAT and CAT
# Note that we are passing the covEst object created by the EWMA function
plot(covEst, assets=c("AMAT", "CAT"))

# specifying a single asset will extract the variance from the EWMA estimate
varAMAT <- getCov(covEst, assets="AMAT")

# Estimate the correlation matrix
corEst <- EWMA(R, 0.94, 25, TRUE)
corEst

# get the correlation between AMGN and DD
corAMGNDD <- getCor(corEst, assets=c("AMGN", "DD"))
cor24 <- getCor(corEst, assets=c(2, 4))
all.equal(corAMGNDD, cor24)

# Plot the correlation estimate between AMGN and DD
# Note that we are passing the covEst object created by the EWMA function
plot(corEst, assets=c("AMGN", "DD"))
