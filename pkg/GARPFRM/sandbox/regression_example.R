# regression example

library(GARPFRM)

# The returns data includes weekly returns
data(returns)

# Get the weekly returns of AAPL and SPY from the returns object
AAPL.ret <- returns[, "AAPL"]
SPY.ret <- returns[, "SPY"]

# Plot the AAPL vs. SPY returns
plot(x=coredata(SPY.ret), y=coredata(AAPL.ret), 
     xlab="SPY returns", ylab="AAPL returns")

# Fit the linear regression model
model.fit <- lm(AAPL.ret ~ SPY.ret)
class(model.fit)

# The print method displays the call and the coefficients of the model
print(model.fit)

# Accessor methods for the lm object
coef(model.fit)
fitted(model.fit)
resid(model.fit)
rstandard(model.fit)

# The summary method displays much more information
model.summary <- summary(model.fit)
class(model.summary)
names(model.summary)

print(model.summary)

coef(model.summary)
model.summary$sigma
model.summary$r.squared
model.summary$adj.r.squared

# Predict method
predict(object=model.fit, newdata=data.frame(SPY.ret=c(-0.1, 0, 0.1)))
model.ci <- predict(object=model.fit, interval="confidence")
model.pi <- predict(object=model.fit, interval="prediction")

# Plot AAPL vs SPY returns
plot(x=coredata(SPY.ret), y=coredata(AAPL.ret), 
     xlab="SPY returns", ylab="AAPL returns")
abline(model.fit, col="red")
lines(x=coredata(SPY.ret), y=model.ci[, "upr"], col="blue", lty=1)
lines(x=coredata(SPY.ret), y=model.ci[, "lwr"], col="blue", lty=1)
lines(x=coredata(SPY.ret), y=model.pi[, "upr"], col="red", lty=2)
lines(x=coredata(SPY.ret), y=model.pi[, "lwr"], col="red", lty=2)

plot(resid(model.fit), type="h")


##### EDA #####

# mean returns
mean(AAPL.ret)
mean(SPY.ret)

# standard deviation
sd(AAPL.ret)
sd(SPY.ret)

# skewness
skewness(x=AAPL.ret, method="sample")
skewness(x=SPY.ret, method="sample")

# kurtosis
kurtosis(x=AAPL.ret, method="sample_excess")
kurtosis(x=SPY.ret, method="sample_excess")

hist(AAPL.ret)
hist(SPY.ret)

plot(density(AAPL.ret))
plot(density(SPY.ret))

mu.est <- mean(AAPL.ret)
sd.est <- sd(AAPL.ret)
plot(density(AAPL.ret))
rug(AAPL.ret)
# sample estimates
curve(dnorm(x, mean=mean(AAPL.ret), sd=sd(AAPL.ret)), 
      add=TRUE, col="red", lty=2, lwd=2)
# robust estimates
curve(dnorm(x, mean=median(AAPL.ret), sd=mad(AAPL.ret)), 
      add=TRUE, col="blue", lty=2, lwd=2)

plot(density(SPY.ret))
rug(SPY.ret)
# sample estimates
curve(dnorm(x, mean=mean(SPY.ret), sd=sd(SPY.ret)), 
      add=TRUE, col="red", lty=2, lwd=2)
# robust estimates
curve(dnorm(x, mean=median(SPY.ret), sd=mad(SPY.ret)), 
      add=TRUE, col="blue", lty=2, lwd=2)

qqnorm(AAPL.ret)
qqline(AAPL.ret)

boxplot(coredata(returns))

# Shapiro-Wilk Normality test
# Null hypothesis that a sample came from a normally distributed population
shapiro.test(coredata(AAPL.ret))
shapiro.test(coredata(SPY.ret))

#library(MASS)
#fitdistr(x=AAPL.ret, densfun="normal")
#fitdistr(x=AAPL.ret, densfun="t", start=list(m=0.001, s=0.05, df=2), lower=c(-0.01, 0, 0.1))


