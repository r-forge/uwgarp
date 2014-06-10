library(GARPFRM)
library(psych)
library(GPArotation)

data(crsp.short)
data = largecap.ts[,2:6]
head(data)

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

# Plot the first three factors
plot(pca$loading[,1], type="l", main="Beta from PCA regression", 
     xlab="maturity", ylab="beta")
lines(pca$loading[,2], col="blue",lty=2)
lines(pca$loading[,3], col="red",lty=2)
legend("topleft",legend=c("PCA1","PCA2","PCA3"),bty="n",lty=c(1,2,2),col=c("black","blue","red"), cex=0.8)

# Creating factor scores: Linear composite of the weighted observed variables
  # Determine weights
  # Multiply variable for each observation by these weights
  # Sum the products
pca.r = principal(data, nfactors=2, rotate="varimax", scores=T)
scores = pca.r$scores
