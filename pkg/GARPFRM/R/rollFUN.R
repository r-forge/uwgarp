rollCov <- function(R, width){
  n <- nrow(R)
  out <- xts(vector("numeric", n), index(R))
  for(i in width:n){
    tmpR <- R[(i-width+1):i,]
    out[i] <- cov(tmpR[,1], tmpR[,2])
  }
  # pad with leading NA
  for(i in 1:(width-1)){
    out[i] <- NA
  }
  out
}

rollCor <- function(R, width){
  n <- nrow(R)
  out <- xts(vector("numeric", n), index(R))
  for(i in width:n){
    tmpR <- R[(i-width+1):i,]
    out[i] <- cor(tmpR[,1], tmpR[,2])
  }
  # pad with leading NA
  for(i in 1:(width-1)){
    out[i] <- NA
  }
  out
}

rollSD <- function(R, width){
  n <- nrow(R)
  out <- xts(vector("numeric", n), index(R))
  for(i in width:n){
    tmpR <- R[(i-width+1):i,]
    out[i] <- sd(tmpR[,1])
  }
  # pad with leading NA
  for(i in 1:(width-1)){
    out[i] <- NA
  }
  out
}
