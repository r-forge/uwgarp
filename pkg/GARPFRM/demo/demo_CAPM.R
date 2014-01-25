library(PerformanceAnalytics)
data(managers)

# Univariate CAPM
# Run CAPM regression
# Fiting CAPM
object = CAPM(managers[,3], managers[,8])

# Retrieve alpha
getAlphas(object)
# Retrieve beta
getBetas(object)
# Retrieve statistics
getStatistics(object)
# Run Hypothesis test where default is CI: 5%
# For alpha = 0 and beta = 1 two-tail test
hypTest(object)
#Run hypothesis test for CI: 10%
CI = 0.1
hypTest(object,CI)

# Plot data with regression line: with coefficients, and tstat specified on graph
par(op)
plot(object)


# Multiple Linear Model CAPM
# Run CAPM regression
# Fiting CAPM
object = CAPM(managers[,3:7], managers[,8])

# Plot security market line
par(op)
chartSML(object)

# Retrieve alpha
getAlphas(object)
# Retrieve beta
getBetas(object)
# Retrieve statistics
getStatistics(object)
# Run Hypothesis test where default is CI: 5%
# For alpha = 0 and beta = 1 two-tail test
hypTest(object)
#Run hypothesis test for CI: 10%
CI = 0.1
hypTest(object,CI)

# Plot data with regression line: with coefficients, and tstat specified on graph
par(op)
plot(object)