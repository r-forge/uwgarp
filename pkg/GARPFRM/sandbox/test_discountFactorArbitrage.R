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



# Estimate bondPrice
# Choose a 2 year bond with semiannual payments to match number of bond prices and CFs
time = seq(from=0.5, to=2, by=0.5)
# First define a bond object to be used throughout the analysis
bond = bondSpec(time, face=100, m=2, couponRate = 0.0475)
# Estimate price, yield, convexity and duration
price = bondPrice(bond,DF)
bondYTM(bond,DF)
# Duration measures the effect of a small parallel shift in the yield curve
mDuration = bondDuration(bond,DF)
# Duration plus convexity measure the effect of a larger parallel shift in the yield curve
# Note however, they do not measure the effect of non-parallel shifts
convexity = bondConvexity(bond,DF)

# Measure a 10% increase in yield on duration
newmDuration = bondDuration(bond,DF, 0.1)



# Appliation: Idiosyncratic Pricing of US Treasury Notes and Bonds
t0 = as.Date("2013-08-15")
t1 = as.Date("2014-02-15")
tn = as.Date("2013-10-04")
currentDate = tn
# Apply a coupon rate of 4.75% bond and create a bond object
bond = bondSpec(face=100, m=2, couponRate = 0.0475)
y1 = 0.00961
bondFullPrice(bond, y1, 8, t0, t1, tn)$clean
bondFullPrice(bond, y1, 8, t0, t1, tn)$dirty
bondFullPrice(bond, y1, 8, t0, t1, tn)$accruedInterest





# Estimating the term structure: compounded rates from discount factors
# Ulitzing data in the following format: Cusip,	IssueDate,	MaturityDate,	Name,	Coupon,	Bid/Ask
head(dat)
ccRate = compoundingRate(dat, initialDate=as.Date("1995-05-15"), m=4, face=100)

years = ccRate$years
rate = ccRate$ccRate
# Plot of continuously compounded spot rates
plot(x=years, y=rate, type="l", ylab="rate", xlab="Time to Maturity", main="Term Structure of Spot Rates")