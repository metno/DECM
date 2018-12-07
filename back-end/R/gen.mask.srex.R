# Create a raster mask for the selected SREX sub-region from the CMIP5 netcdf file.
gen.mask.srex <- function(destfile, mask.polygon=NULL, ind=FALSE, inverse=FALSE, 
                          mask.values=1, verbose=FALSE) {
  if(verbose) print("gen.mask.srex")
  if(verbose) print(destfile)
  r <- raster::raster(destfile)
  r <- setValues(r,NA)
  extent.r <- extent(r)
  if(extent.r[2]==360) extent(r) <- c(-180,180,-90,90)
  indices <- extract(r,mask.polygon,cellnumbers=TRUE)[[1]][,1]
  if(extent(mask.polygon)[2]>180){
    extent(r) <- c(180,540,-90,90)
    indices <- sort(c(indices,extract(r,mask.polygon,cellnumbers=TRUE)[[1]][,1]))
  }
  if(inverse){
    tmp <- seq(1,length(getValues(r)))
    indices <- tmp[which(is.na(match(tmp,indices)))]
  }
  mask.raster <- r
  extent(mask.raster) <- c(0,360,-90,90)
  mask.raster[indices] <- mask.values
  if(ind) return(indices)
  return(mask.raster)
}
