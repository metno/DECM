# Do we need a new standard deviation function? Is this not the same as the sd function in stats?
SD <- function(x, subn=FALSE) {
  meanx <- mean(x, na.rm = TRUE)
  devx <- x - meanx
  ssd <- sqrt(sum(devx * devx, na.rm = TRUE)/(length(x[!is.na(x)]) -
                                                subn))
  return(ssd)
}