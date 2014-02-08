library(GARPFRM)

data(crsp.short)
R <- largecap.ts[, 1:4]

ewma_est <- EWMA(R)
# This is a list of two elements
names(ewma_est)

# extract the EWMA estimate 
ewma_est$EWMA

# extract the data
ewma_est$R

tmpCov <- getCov(ewma_est, 1, 2)

# This should throw an error because ewma_est is a covariance estimate
getCor(ewma_est, 1, 2)

# correlation between assets 1 and 4
estCor <- getCor(EWMA(R, cor=TRUE), 1, 4)

plot(ewma_est, asset1=1, asset2=2)

plot(EWMA(R, cor=TRUE), asset1=1, asset2=2)

# # might need a separate function for univariate time series of returns
# 
# # estimate covariance or correlation using EWMA for a multivariate data set
# rbEWMA <- function(R, lambda=0.94, training_period=10, cor=FALSE){
#   # check for training_period must be greater than ncol(R)
#   # check for lambda between 0 and 1
#   
#   # Separate data into a training set and a testing set
#   R_training <- R[1:training_period,]
#   R_testing <- R[(training_period+1):nrow(R),]
#   
#   # calculate a starting covariance matrix
#   cov_start <- cov(R_training)
#   cov_lag <- cov_start
#   tmp_cov <- vector("list", nrow(R_testing))
#   
#   for(i in 1:nrow(R_testing)){
#     # extract R for the ith time step
#     tmpR <- R_testing[i,]
#     tmp_cov[[i]] <- lambda * (t(tmpR) %*% tmpR) + (1 - lambda) * cov_lag
#     # update cov_lag to be tmp_cov from the current period
#     cov_lag <- tmp_cov[[i]]
#   }
#   out <- tmp_cov
#   names(out) <- index(R_testing)
#   if(cor) out <- lapply(out, cov2cor)
#   return(out)
# }
# 
# 
# 
# getCov <- function(object, asset1, asset2){
#   # check for 
#   if(is.character(asset1) & is.character(asset2)){
#     idx1 <- grep(asset1, colnames(object[[1]]))
#     if(length(idx1) == 0) stop("name for asset1 not in object")
#     idx2 <- grep(asset2, colnames(object[[1]]))
#     if(length(idx2) == 0) stop("name for asset2 not in object")
#   } else {
#     # checks for dimensions
#     idx1 <- asset1
#     idx2 <- asset2
#   }
#   out <- xts(unlist(lapply(x, function(x) x[idx1, idx2])), as.Date(index(x)))
#   colnames(out) <- paste(asset1, asset2, sep=".")
#   # detect if estimating cor or cov and set a class 
#   # (i.e. EWMA_cov or EWMA_cor classes)
#   # This will change how we handle getCov or getCor
#   # For example, if someone uses EWMA with cor = TRUE, we probably don't want
#   # them to be able to call getCov on correlations estimated with EWMA. It would
#   # still return the right value based on the index, but it is misleading that
#   # they used getCov to return the correlations.
#   return(out)
# }
# 
# 
# x <- rbEWMA(R, training_period=20)[[1]]
# x <- rbEWMA(R[,1])
# x
# covAMATAMGN <- getCov(x, "AMATGLG", "AMGN")
# 
# # should have a plot method that takes an EWMA_cov or EWMA_cor object and then
# # use getCov or getCor to extract the time series to plot
# 
# 
# plot(covAMATAMGN)
# 
