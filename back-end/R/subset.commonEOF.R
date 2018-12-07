subset.commonEOF <- function(x,it=NULL,is=NULL,ip=NULL,im=NULL,verbose=FALSE) {
  if(verbose) print("subset.commonEOF")
  Y <- esd::subset.dsensemble.multi(x,it=it,is=is,ip=ip,im=im,verbose=verbose)
  Y.mean <- attr(x$eof,"mean")
  if(!is.null(is)) {
    ok.lon <- attr(x[[2]],"longitude")>=min(is$lon) & 
      attr(x[[2]],"longitude")<=max(is$lon)
    ok.lat <- attr(x[[2]],"latitude")>=min(is$lat) & 
      attr(x[[2]],"latitude")<=max(is$lat)
    clim.i <- Y.mean[[i]][ok.lon,ok.lat]
  }
  attr(Y,"mean") <- Y.mean
  return(Y)
}