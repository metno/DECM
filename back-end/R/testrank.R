testrank <- function(x,m,N,d1) {
  ## Find the rank number of m observations
  ## Need to unwrap the dimensions
  dim(x) <- c(N,d1)
  number <- apply(x,1,function(x) order(x)[1:m])
  return(c(number))
}