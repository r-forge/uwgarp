library(GARPFRM)
library(psych)
library(GPArotation)

data(crsp.short)
data = largecap.ts[,2:6]
head(data)

# Empirical application: Linear hedge estimation 
# OLS Level-on-Level regression 
deltas = linearHedge(data[,1],data[,2:5])
# Insert the normalized hedged contract versus hedgeable contract value
deltas = c(1,deltas)

# In sample illustration: random, mean reverting spreads
hedgedInstruments = data%*%deltas
plot(hedgedInstruments, type="l", main = "Hedged Price Difference", xlab="Time",ylab="Difference")


######### Applying duration as a hedge######## TO-DO



# Have a single, empirical description of the behavior of the term structure that can be applied across all
# assets. Principal Compnents (PCs) provide such an emperical description 
# Retain components that combined account for x% of the cumulative variance
pca = PCA(data, nfactors = 3, rotate="none")
summary(pca)

# Retrieve Loadings and if loading is insignificant then omit
getLoadings(pca)

# Retrieve Weights
getWeights(pca)

## Structural Equation Modelling
# Determining the appropriate number of factors
# A graphic representation of the 3 oblique factors
fa.diagram(pca)

# Alternative to determining the number of factors to compare the solution 
# to random data with the same properties as the real data set.
fa.parallel(data)

# Plot up to the first three factors
plot(pca)
pca = PCA(data, nfactors = 2, rotate="none")
plot(pca)

# Creating factor scores: Linear composite of the weighted observed variables
  # Determine weights
  # Multiply variable for each observation by these weights
  # Sum the products
pca.r = principal(data, nfactors=2, rotate="varimax", scores=T)
scores = pca.r$scores
plot(pca$scores[,1],pca$scores[,2], xlab="PCA1", ylab="PCA2", main = "Scores: Observable Pattern")