# Read in the data file
suppressMessages(library(GARPFRM))
options(digits=3)
data(bonds)

# The Cash Flows from Fixed-Rate Government Coupon Bonds
# Discount Factors and the Law of One Price

# Initialize: The Cash Flows from Fixed-Rate: treasury bonds ticking in quarters
cashFlow = rbind(c(100, 0, 0, 0), c(2 + 7/8, 102 + 7/8, 0, 0), c(3 + 3/4, 3 + 3/4, 103 + 3/4, 0), c(3 + 3/4, 3 + 3/4, 3 + 3/4, 103 + 3/4))
# Initialize: Price of the bond
price <- matrix(c(96.8, 99.56, 100.86, 101.22), ncol=1)
# Estimate the Discount Factors (DF)
DF = discountFactor(price , cashFlow)

# Define a bond object to be used throughout the analysis
# Estimate bondPrice############
# example, a 2 year bond with semiannual payments
time = seq(from=0.5, to=2, by=0.5)




# Appliation: Idiosyncratic Pricing of US Treasury Notes and Bonds
t0 = as.Date("2013-08-15")
t1 = as.Date("2014-02-15")
tn = as.Date("2013-10-04")
currentDate = tn
# Apply a yield on 4.75% bond
y1 = 0.00961
bondFullPrice(0.0475, y1, 2, 8, face=100, t0, t1, tn)$clean
bondFullPrice(0.0475, y1, 2, 8, face=100, t0, t1, tn)$dirty
bondFullPrice(0.0475, y1, 2, 8, face=100, t0, t1, tn)$accruedInterest

# Estimating the term structure: compounded rates from discount factors
# Ulitzing data in the following format: Cusip,	IssueDate,	MaturityDate,	Name,	Coupon,	Bid/Ask
head(dat)
ccRate = compoundingRate(dat, initialDate=as.Date("2000-05-15"), m=4, face=100)

years = ccRate$years
rate = ccRate$ccRate
# Plot of continuously compounded spot rates
plot(x=years, y=rate, type="l", ylab="rate", xlab="Time to Maturity", main="Term Structure of Spot Rates")