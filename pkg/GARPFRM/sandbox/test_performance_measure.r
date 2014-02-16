# 'Load the GARPFRM package and the CAPM dataset.
suppressMessages(library(GARPFRM))
options(digits=3)
data(crsp.short)
stock_rets.df <- largecap.ts
Measures = rep(NULL,4)

## keep it simple and use this thoughout
## no need to transform to zoo, matrix, or df objects
# Equal weight portfolio
R.portfolio <- Return.portfolio(largecap.ts[, 1:5])

# Market returns
R.market <- largecap.ts[, "market"]

# risk free rate
rf <- largecap.ts[, "t90"]
## 

# Definitioned/Constant variables
relation = c("Less Than","Equal To","Greater Than")

# Some of run time parameters
# Should be amongst the input for shiny
# Minimum Acceptable Return for Sortino Ratio or Downside Deviation
# defaults to 0
MAR_set = 0

# Denominator for Sortino Ratio or Downside Deviation 
# one of "full" or "subset", indicating whether to use the length of the full series
# or the length of the subset of the series below the MAR as the denominator,
# defaults to "full"
denom = "full"

# Portfolio name used in output
PortName = "Portfolio"

# Market name used in output
MrktName ="Market"

#Market Name in data set
mrktDataName = "market"

# Risk free rate name used in output
RFName="Risk_Free_Rate"

# Risk Free Name in data set
rfDataName = "t90"

Measures = rbind(Measures, c("Series Name",PortName,MrktName,RFName))
colnames(Measures) = c("Measure","Portfolio","Market","RiskFree")

# List column headers predominantly tickers/name of securities

# Summarize the first and last data values corresponding to the first 5 dates for the first 5 returns.

colnames(stock_rets.df)
head(stock_rets.df)
tail(stock_rets.df)


# Number time periods for year for data set
freQ = 12

# Estimate a zooreg object: regularly spaced zoo object.
# 
stock.z = zooreg(stock_rets.df[,-1], start=c(1993, 1), end=c(2013,11), 
                 frequency=freQ)

index(stock.z) = as.yearmon(index(stock.z))

# Summarize Start, End, and Number of Rows
start(stock.z)
end(stock.z)
nRow = nrow(stock.z)
nRow

# pointer to column with the market returns
ptrMkt = which(colnames(stock.z)==mrktDataName,arr.ind=TRUE)
ptrMkt
# pointer to column of Risk Free Ratw
ptrRF = which(colnames(stock.z)==rfDataName,arr.ind=TRUE)
ptrRF

# as.data.frame to check if an object is a data frame, or coerce it if possible.
returns.mat = as.matrix(coredata(stock.z))

# Number of time series in data set
nSec = ncol(returns.mat)

# Random create a portfolio from the non-market non RF series
# Get a normalized set of weights
# Could set up the application to create a random portfolio from a subset or 
# Input specific securities and weights
coefs = runif(nSec-2)
alphas = coefs/sum(coefs)
sum(alphas)


portfolio = returns.mat[,1:(nSec-2)]%*%as.vector(alphas)
colnames(portfolio) ="PortRets"

salient = matrix(data=c("Mean",mean(portfolio),mean(returns.mat[,ptrMkt]),mean(returns.mat[,ptrRF]),
                   "Stdev",sd(portfolio),sd(returns.mat[,ptrMkt]),sd(returns.mat[,ptrRF]),
                   "AnnuallyFrequency",rep(freQ,3),
                   "Number of Samples",rep(nRow,3)),nrow=4,ncol=4,byrow=TRUE)

Measures = rbind(Measures, salient)


# Start to compute performance measures using performance analytic and as a check
# compute from underlying formula

# Some of comparison match, but some are inexplicable different.
# Really should investigate the latter,  I am sure the users will do so

# Compute beta
beta = cov(portfolio[,1],returns.mat[,ptrMkt])/var(returns.mat[,ptrMkt])
cat('\n Beta =',beta,', between the portfolio represented by ',PortName,' and the market represented by ',MrktName,' \n')

Measures = rbind(Measures,c("Beta",beta,"",""))

# Compute Treynor

   # Performance Analytics (PA) computation of Treynor for portfolio and market
tr = TreynorRatio(portfolio[,1,drop=FALSE], returns.mat[,ptrMkt,drop=FALSE],returns.mat[,ptrRF,drop=FALSE],freQ )
trMarket = TreynorRatio(returns.mat[,ptrMkt,drop=FALSE], returns.mat[,ptrMkt,drop=FALSE],returns.mat[,ptrRF,drop=FALSE],freQ )
  
   # From formula (Hand) computation of Treynor for portfolio and market
trHand = freQ*(mean(portfolio[,1])-mean(returns.mat[,ptrRF]))/beta
# beta of Treynor for market is 1
trHandM = freQ*(mean(returns.mat[,ptrMkt])-mean(returns.mat[,ptrRF]))

   # Output results from Treynor Ratio computation
cat('\n PA computed for ',PortName,' Treynor Ratio = ',tr,'\n')
cat('\n Hand computed for ',PortName,' Treynor Ratio = ',trHand,'\n')
cat('\n PA computed for ',MrktName,' Treynor Ratio = ',trMarket,'\n')
cat('\n Hand computed for ',MrktName,' Treynor Ratio = ',trHandM,'\n')

Measures = rbind(Measures,c("Treynor Ratio",tr,trMarket,""))

   # Compare Treynor Ratio for market returns against Treynor Ratio for portfolio returns
cat('\n Treynor Ratio of ',MrktName,' is ',relation[sign(trMarket - tr) + 2],' Treynor Ratio of ',PortName,'\n' )
TR_Mrkt2Port = relation[sign(trMarket - tr) + 2]

##
# calculate excess returns of R.portfolio
R.portfolio.x <- R.portfolio - rf

# calculate excess returns of R.market
R.market.x <- R.market - rf

# run regression to get beta
fit <- lm(R.portfolio.x ~ R.market.x)
beta.lm <- coef(fit)[2]

# calculate beta another way
beta <- as.numeric(cov(R.portfolio.x, R.market.x) / var(R.market.x))

# Annualized portfolio excess returns / beta
# This should match the TreynorRatio from PerformanceAnalytics
TR.rb <- as.numeric(Return.annualized(R.portfolio.x) / beta.lm)

as.numeric(Return.annualized(R.portfolio.x) / beta)

# Calcualte Treynor Ratio with PerformanceAnalytics
TR.PA <- TreynorRatio(R.portfolio, R.market, rf)

all.equal(TR.rb, TR.PA)
cat("Treynor Ratio by hand: ", TR.rb, "\n")
cat("Treynor Ratio with PerformanceAnalytics: ", TR.PA, "\n")
##

# Compute Sharpe

   # PA computation of Sharpe
shr = SharpeRatio(portfolio[,1,drop=FALSE], returns.mat[,ptrRF,drop=FALSE], p = 0.95,
FUN = "StdDev")

   # Compute excess portfolio returns (portfolio returns net of RF returns) 
xCess = portfolio[,1] - returns.mat[,ptrRF]
   # Hand computation of Sharpe
shrHand = mean(xCess)/sd(xCess)

   # Output results from Sharpe Ratio computation
cat('\n PA computed ',PortName,' Sharpe Ratio = ',shr,'\n')
cat('\n Hand computed ',PortName,' Sharpe Ratio = ',shrHand,'\n')

Measures = rbind(Measures,c("Sharpe Ratio",shr,"",""))

# Compute Jensen's Alpha

   # Mean of risk free rate over sampling time series
meanRF = mean(returns.mat[,ptrRF])

   # PA computation of Jensen's Alpha
ja = CAPM.jensenAlpha(portfolio[,1,drop=FALSE], returns.mat[,ptrMkt,drop=FALSE],meanRF)

# Hand computation of Jensen's Alpha using regression apprach


   # Precompute excess returns
## The risk-free rate varies through time so just computing the mean
## is oversimplifying and could be part of the reason for the differences
## you are seeing between your hand calculation and PerformanceAnalytics
xCessP1 = portfolio[,1]-meanRF
xCessM1 = returns.mat[,ptrMkt]-meanRF

   # Regression computation and salient results
lm_ja1.fit = lm(xCessP1 ~ xCessM1)
values1 = summary(lm_ja1.fit)
values1
analT1 = values1[[4]]

   # Annualize Jensen's Alpha
jaHand = (1+(mean(portfolio[,1]) - meanRF - beta*(mean(returns.mat[,ptrMkt])-meanRF)))^12 -1

   # Use Delta Method to approximate standard error of analyzed Jensen's Alpha
deltaCoef = (freQ*(1+analT1[1,1])^(freQ-1))^2

   # Output salient results for Jensen's Alpha
cat('\n PA computed ',PortName,' Jensen\'s Alpha = ',ja,'\n')
cat('\n Regression computed ',PortName,' Jensen\'s Alpha, Not Annualized= ',analT1[1,1],'\n')
cat('\n Hand computed Annualized ',PortName,' Jensen Alpha = ',jaHand,'\n')
cat('\n Delta Method Coefficient Value = ', deltaCoef,'\n')

   # t stat and pvalue under H0: alpha = 0 against HA: alpha != 0 
tStat = jaHand/(analT1[1,2]*deltaCoef^0.5)
pValue = 2*(1-pt(tStat,nRow-2))
cat('\n H0: alpha = 0, HA: alpha != 0  p-value: ',pValue,'\n')

Measures = rbind(Measures,c("Jensen's Alpha",ja,"",""))
Measures = rbind(Measures,c("Jensen's Alpha P Value",pValue,"",""))

# Compute Tracking Error

   # PA computation of Tracking Error   
te = TrackingError(portfolio[,1,drop=FALSE], returns.mat[,ptrMkt,drop=FALSE], scale = freQ)

   # Precompute required values to computr Tracking Error by Hand
RR = portfolio[,1]-returns.mat[,ptrMkt]
ARR = mean(RR)

   # Hand computation of Tracking Error   
teHand = sqrt(sum((RR-ARR)^2)/(nRow-1))*sqrt(freQ)

   # Output results from Tracking Error computation
cat('\n PA computed ',PortName,' Tracking Error = ',te,'\n')
cat('\n Hand computed ', PortName,' Tracking Error = ',teHand,'\n')

Measures = rbind(Measures,c("Tracking Error",te,"",""))

# Compute Information Ratio

   # PA computation of Information Ratio
ir = InformationRatio(portfolio[,1,drop=FALSE], returns.mat[,ptrMkt,drop=FALSE], scale = freQ)

   # Hand computation of Information Ratio 
irHand = (ARR/(sqrt(sum((RR-ARR)^2)/(nRow-1))))*sqrt(freQ)

   # Output results from Information Ratio computation  
cat('\n PA computed ',PortName,' Information Ratio = ',ir,'\n')
cat('\n Hand computed ',PortName,' Information Ratio = ',irHand,'\n')

Measures = rbind(Measures,c("Information Ratio",ir,"",""))

# Compute Sortino Ratio (including Downside Deviation)

   # PA computation of Sortino Ratio
sr = SortinoRatio(portfolio[,1,drop=FALSE], MAR = MAR_set, weights = NULL)

   # Set value of denominator for Downside Deviation from the parameter denom set earlier
denomVal = nRow
if (denom != "full")
   { 
    denomVal = sum(ifelse((portfolio[,1] - MAR_set)<0,1,0))
   }
cat('\n Denominator parameter is set to: ',denom,' which results in a denominator value of ',denomVal,'\n')

   # PA computation of Downside Deviation
dwn = DownsideDeviation(portfolio[,1], MAR = MAR_set, method = "full")

   # Hand computation of Downside Deviation
dwnHand = sqrt(sum((min((portfolio[,1,drop=FALSE] - MAR_set),0))^2)/(denomVal))

   # Output computation of Downside Deviation
cat('\n PA computed ',PortName,' Downside Deviation = ',dwn,'\n')
cat('\n Hand computed ',PortName,' Downside Deviation = ',dwnHand,'\n')

   # Hand computation of Sortino Ratio
srHand = sqrt(freQ)*ARR/dwn

   # Output computation of Sortino Ratio
cat('\n PA computed ',PortName,' Sortino Ratio = ',sr,'\n')
cat('\n Hand computed ',PortName,' Sortino Ratio = ',srHand,'\n')

Measures = rbind(Measures,c("Downside Deviation",dwn,"",""))
Measures = rbind(Measures,c("Denominator DD",denomVal,"",""))
Measures = rbind(Measures,c("Sortino Ratio",sr,"",""))
Measures.df= as.data.frame(Measures,stringsAsFactors=FALSE)

print(Measures.df)
