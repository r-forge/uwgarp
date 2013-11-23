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
