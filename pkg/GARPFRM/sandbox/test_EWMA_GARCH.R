library(PerformanceAnalytics)
library(rugarch)
library(GARPFRM)
library(rmgarch)
data(managers)
options(digits=4)

# Remember: log-returns for GARCH analysis
temp_1 = managers[,3] 
temp_2 = managers[,8]
   
# create combined data series
temp = merge(temp_1,temp_2)

# scatterplot of returns
plot(coredata(temp_2), coredata(temp_2), xlab=colnames(temp_1), ylab=colnames(temp_2), 
     main ="Scatterplot of Returns")
abline(h=0,v=0,lty=3)

# compute rolling cor
# chart.RollingCorrelation( temp_1, temp_2, width=20)

cor.fun = function(x){
  cor(x)[1,2]
}

cov.fun = function(x){
  cov(x)[1,2]
}

roll.cov = rollapply(as.zoo(temp), FUN=cov.fun, width=20,
                     by.column=FALSE, align="right")
roll.cor = rollapply(as.zoo(temp), FUN=cor.fun, width=20,
                     by.column=FALSE, align="right")
par(mfrow=c(2,1))
# First Rolling Cov
plot(roll.cov, main="20-Day Rolling Cov",
     ylab="covariance", lwd=3, col="blue")
grid()
abline(h=cov(temp)[1,2], lwd=3, col="red")

# Second Rolling Cor
plot(roll.cor, main="20-Day Rolling Cor",
     ylab="correlation", lwd=3, col="blue")
grid()
abline(h=cor(temp)[1,2], lwd=3, col="red")
par(mfrow=c(1,1))

# Calculate EWMA cov and cor, applying default lambda - 0.96
covEwma <- EWMA(temp)

# Extract conditional var and cor
assetCondCov <- covEwma[,2,1];
t <- length(covEwma[,1,1]);
assetCondCov<- rep(0,t);
for (i in 1:t) {
  assetCondCov[i]<- cov2cor(covEwma[i,,])[1,2];
}
# Plots
par(mfrow=c(2,1))
plot(x=time(as.zoo(temp)), y=assetCondCov,
     type="l", xlab="Time", ylab="Covariance", lwd=2, col="blue",
     main="EWMA Covariance");
grid()
abline(h=cov(temp)[1,2], lwd=2, col="red")
plot(x=time(as.zoo(temp)), y=assetCondCov,
     type="l", xlab="Time", ylab="Correlation", lwd=2, col="blue",
     main="EWMA Correlation");
grid()
abline(h=cor(temp)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))


# Compute rolling cov and cor using new window
roll.cov = rollapply(as.zoo(temp), FUN=cov.fun, width=120,
                     by.column=FALSE, align="right")
roll.cor = rollapply(as.zoo(temp), FUN=cor.fun, width=120,
                     by.column=FALSE, align="right")
par(mfrow=c(2,1))
plot(roll.cov, main="120-Day rolling cov",
     ylab="covariance", lwd=2, col="blue")
grid()
abline(h=cov(temp)[1,2], lwd=2, col="red")
plot(roll.cor, main="120-Day rolling cor",
     ylab="correlation", lwd=2, col="blue")
grid()
abline(h=cor(temp)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))


# compute EWMA cov and cor for longer half-life of 
halfLife = log(0.5)/log(0.94) + 5
lambda = exp(log(0.5)/halfLife)
covEwma <- EWMA(as.data.frame(temp), lambda)


# Extract conditional var and cor
assetCondCov <- covEwma[,2,1]
t <- length(covEwma[,1,1])
assetCondCov<- rep(0,t)
for (i in 1:t) {
  assetCondCov[i]<- cov2cor(covEwma[i,,])[1,2]
}
# Plots
par(mfrow=c(2,1))
plot(x=time(as.zoo(temp)), y=assetCondCov,
     type="l", xlab="Time", ylab="Cov", lwd=2, col="blue",
     main="EWMA Cov")
grid()
abline(h=cov(temp)[1,2], lwd=2, col="red")
plot(x=time(as.zoo(temp)), y=assetCondCov,
     type="l", xlab="Time", ylab="Cor", lwd=2, col="blue",
     main="EWMA Cor")
grid()
abline(h=cor(temp)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))

# Dynamic Conditional Cor
# Joint is the conditonal corr param w/ cov targeting
garch11 <- garch11(temp)

class(garch11)
slotNames(garch11)
names(garch11@mfit)
names(garch11@model)

# many extractor functions - see help on DCCfit object
# coef, likelihood, rshape, rskew, fitted, sigma, residuals, plot, infocriteria, rcor, rcov show, nisurface
# show dcc fit
garch11

# conditional sd of each series
plot(garch11, which=2)

# conditional covar of each series
plot(garch11, which=3)

# conditional cor
plot(garch11, which=4)

# plot mutiple time series: extracting cor series
ts.plot(rcor(garch11)[1,2,])

# Forecasting conditional vol and cor, default wd = 100
fcstGarch11 = fGarch11(garch11)

class(fcstGarch11)
slotNames(fcstGarch11)
class(fcstGarch11@mforecast)
names(fcstGarch11@mforecast)

# many method functions - see help on DCCforecast class
# rshape, rskew, fitted, sigma, plot, rcor, rcov, show

# Show forecasts
fcstGarch11

# plot(garch11, which=3)
plot(fcstGarch11, which=3)