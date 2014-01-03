# CAPM Testing and Fitting

# 'Load the GARPFRM package and the CAPM dataset.
suppressMessages(library(GARPFRM))
options(digits=3)
data(crsp.short)
data(cons)
stock.df <-cbind(largecap.ts,cons[,"CONS"])
colnames(stock.df)= c(colnames(largecap.ts),"CONS")
colnames(stock.df)

# Summarize Start, End, and Number of Rows
#stock.z = returns
start(stock.df)
end(stock.df)
nrow(stock.df)

# Estimate excess returns: subtracting off risk-free rate
# To strip off the dates and just return a plain vector/matrix coredata() can be used.
# as.data.frame to check if an object is a data frame, or coerce it if possible.
returns.mat = coredata(stock.df)
exReturns.mat = returns.mat - returns.mat[,"t90"]
exReturns.df = as.data.frame(exReturns.mat)

# Run CAPM regression for Microsoft (MSFT) using first 5 years
# 60 months divided by 12 months in a years = 5 years
# capm_data use AAPL and MARKET (uppercase)
capm.fit = lm(MSFT~market,data=exReturns.df,subset=1:60)
summary(capm.fit)

# Plot data with regression line
plot(exReturns.df$market,exReturns.df$MSFT, main="CAPM for MSFT",
     ylab="Excess Return: MSFT",
     xlab="Excess Return: MARKET")

# Plot CAPM regression estimate
abline(capm.fit)    
# Create Axis 
abline(h=0,v=0,lty=3)
# Placing beta & tstat values on the plot for APPL
alpha = coef(summary(capm.fit))[1,1]
a_tstat = coef(summary(capm.fit))[1,3]
beta = coef(summary(capm.fit))[2,1]
b_tstat = coef(summary(capm.fit))[2,3]

legend("topleft", legend=c(paste("alpha =", round(alpha,dig=2),"(", round(a_tstat,dig=2),")"),
                           paste("beta =", round(beta,dig=2),"(", round(b_tstat,dig=2),")")), cex=.8, bty="n")

# Use a capm.tstats function:
# Estimating CAPM with alpha=0 for asset using first 5 years of data
capm.tstats = function(r,mkrt,type = FALSE) {
  # Fiting CAPM and retrieve alpha specific tstats or pvalues
  capm.fit = lm(r~mkrt)    
  # Extract summary info
  capm.summary = summary(capm.fit) 
  if(is.null(type) | type=="pvalue"){
    # Retrieve p-value if specified
    p.value = coef(capm.summary)[1,4]  
    p.value
  }else{
    # Otherwise retrieve t-stat if specified or on default
    t.stat = coef(capm.summary)[1,3]  
    t.stat
  }
}

# Retrieve tstats from function for assets
# Filter out rf and market before running
# For capm_data use -c(1,6,7)
colnames(exReturns.mat[,-c(21,22,23)])
tstats = apply(exReturns.mat[1:60,-c(21,22,23)],2, capm.tstats,
               exReturns.mat[1:60,"market"])
tstats

# Test Hypothesis for 5% CI: H0: alpha=0
abs(tstats) > 2
any(abs(tstats) > 2)

# Plot expected return versus beta
# Estimate expected returns over first 5 years
mu.hat = colMeans(exReturns.mat[1:60,-c(21,22,23)])
mu.hat

# Compute beta over first 5 years
capm.betas = function(r,market) {
  capm.fit = lm(r~market)    			
  # Fit capm regression
  capm.beta = coef(capm.fit)[2]				
  # Extract coefficients
  capm.beta
}

betas = apply(exReturns.mat[1:60,-c(21,22,23)],2,
              
              FUN=capm.betas,
              market=exReturns.mat[1:60,"market"])
betas

# Plot expected returns versus betas
plot(betas,mu.hat,main="Expected Return vs. Beta")
# Estimate regression of Expected Return vs. Beta
sml.fit = lm(mu.hat~betas)
sml.fit
summary(sml.fit)
# Ideally intercept is zero and equals the excess market return
mean(exReturns.mat[1:60,"market"])

# Plot Fitted SML
plot(betas,mu.hat,main="Estimated SML")
abline(sml.fit)
legend("topleft",1, "Estimated SML",1)


# The Consumption-Oriented CAPM is analogous to the simple form of the CAPM. Except that 
# the growth rate of per capita consumption has replaced the rate of return on the market 
# porfolio as the influence effecting returns.

# Run C-CAPM regression for CONS (Consumption) using first 5 years
# 60 months divided by 12 months in a years = 5 years
end = nrow(stock.df)
capm.fit = lm(CONS~market,data=exReturns.df,subset=(end-60):end)
summary(capm.fit)

# Plot data with regression line
plot(exReturns.df$market,exReturns.df$CONS, main="CAPM for CONS",
     ylab="Excess Return: CONS",
     xlab="Excess Return: market")
# Plot C-CAPM regression estimate
abline(capm.fit)    
# Create Axis 
abline(h=0,v=0,lty=3)
# Placing beta & tstat values on the plot for CONS
beta = coef(summary(capm.fit))[2,1]
b_stat = coef(summary(capm.fit))[2,3]
alpha = coef(summary(capm.fit))[1,1]
a_stat = coef(summary(capm.fit))[1,3]
legend("topleft", legend=
         c(paste("alpha =",round(alpha,dig=2),"(",round(a_tstat, dig=2),")"), 
           paste("beta =",round(beta,dig=2),"(",round(b_tstat,dig=2),")")), cex=.8, bty="n")
# NOTE: CCAPM it has two puzzles: the equity premium puzzle (EPP) and the
# risk-free rate puzzle (RFRP). EPP implies that investors are extremely
# risk averse to explain the existence of a market risk premium. While RFRP
# stipulates that investors save in TBills despite the low rate of return.
