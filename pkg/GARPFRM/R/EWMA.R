#' Exponential Weight Moving Average (EWMA)
#' 
#' Description of EWMA. The function handles UV and MLE objects and returns either cov or cor.
#' 
#' @param object EWMA (either cov or corr, default = cov)
#' @export
EWMA <- function(object, lambda=0.96, cor=FALSE) {    
    if ((is.data.frame(object)) & (lambda<1 || lambda > 0)){
      object.names  = colnames(object)
      t.object      = nrow(object)
      k.object      = ncol(object)
      object        = as.matrix(object)
      t.names       = rownames(object)
     
      covEWMA = array(,c(t.object,k.object,k.object))
      # Note it is unconditional cov
      cov.f = var(object)  
      FF = (object[1,]- mean(object)) %*% t(object[1,]- mean(object))
      covEWMA[1,,] = (1-lambda)*FF  + lambda*cov.f
      for (i in 2:t.object) {
        FF = (object[i,]- mean(object)) %*% t(object[i,]- mean(object))
        covEWMA[i,,] = (1-lambda)*FF  + lambda*covEWMA[(i-1),,]
      }
      
    } else {
      stop("object handled as data.frame class || exp-decay lambda must be ]0:1[") 
    }

    dimnames(covEWMA) = list(t.names, object.names, object.names)
    
    if(cor) {
        corEWMA = covEWMA
      for (i in 1:dim(corEWMA)[1]) {
        corEWMA[i, , ] = cov2cor(covEWMA[i, ,])
      }
      return(corEWMA)
    } else{
      return(covEWMA)  
    }
  }
