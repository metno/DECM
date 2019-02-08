## What is this function supposed to do? 
## It does not work anymore due to changes in esd
map.commonEOF <- function(x,it=NULL,is=NULL,ip=NULL,im=NULL,FUN=NULL,plot=FALSE,
                          colbar=list(pal=NULL,rev=FALSE,n=10,breaks=NULL,show=TRUE),
                          verbose=FALSE) {
  if(verbose) print("map.commonEOF")
  x <- subset.commonEOF(x,it=it,is=is,ip=ip,im=im,verbose=verbose)
  Y <- esd::map.eof(x$eof,it=it,anomaly=TRUE,plot=FALSE,FUN=FUN,verbose=verbose)
  if(is.null(FUN)) FUN <- "" ## If FUN = NULL the following line doesn't work:
  if( FUN %in% c("mean","median","q5","q95") ) {
    clim <- attr(x$eof,"mean")
    Y <- Y + clim
  }
  if(plot) esd::map.eof(Y)
  invisible(Y)
}
