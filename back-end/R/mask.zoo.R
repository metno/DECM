#apply mask to a zoo object by setting values outside the mask to NA
mask.zoo <- function(zoo.object,mask) {
  mask <- flip(mask,direction='y')
  zoo.object[,which(is.na(getValues(mask)))] <- NA
  return(zoo.object)
}
