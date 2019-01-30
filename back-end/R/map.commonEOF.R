map.commonEOF <- function(x,it=NULL,is=NULL,ip=NULL,im=NULL,FUN=NULL,plot=FALSE,
                          colbar=list(pal=NULL,rev=FALSE,n=10,breaks=NULL,show=TRUE),
                          verbose=FALSE) {
  if(verbose) print("map.commonEOF")
  x <- subset.commonEOF(x,it=it,is=is,ip=ip,im=im,verbose=verbose)
  Y <- esd::map.eof(x,it=it,anomaly=TRUE,plot=FALSE,FUN=FUN,verbose=verbose)
  if(is.null(FUN)) FUN <- "" ## If FUN = NULL the following line doesn't work:
  if( FUN %in% c("mean","median","q5","q95") ) {
    if(is.null(im)) im <- seq(length(x)-2)
    clim <- attr(x$eof,"mean")
    Y <- Y + clim#aperm(array(rep(clim,nrow(Y)),rev(dim(Y))),c(2,1))
  }
  if(plot) esd::map.eof(Y)
  invisible(Y)
}
