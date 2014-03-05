# Monte Carlo demo

library(GARPFRM)

mc <- monteCarlo(0.05, 0.25, 500, 1, 52, 10)

# plot the simulated asset paths from the monte carlo simulation
plot(mc)

# get the ending prices
ending_prices <- endingPrices(mc)

# plot the ending prices
plotEndingPrices(mc)
