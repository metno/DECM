# Get coordinates of a chosen prudence region
# Data about the regions are included in the folder back-end/data/PRUDENCE 
getPrudenceCoords <- function(prudence,region,destfile="coords.txt"){
  i <- region
  if(is.character(region)) i <- which(prudence[,2]==region)
  lat <- as.numeric(prudence[i,6:7])
  lon <- as.numeric(prudence[i,4:5])
  coords1 <- expand.grid(lon[1],lat)
  coords2 <- expand.grid(lon[2],lat)
  coords <- rbind(coords1,coords2[c(2,1),])
  write(t(coords),file=destfile,ncolumns = 2)
}