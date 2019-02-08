dx <- function(ceof,im=NULL,is=NULL,ip=NULL,
               it1=c(1981,2010),it2=c(2071,2100),
               FUN="mean",verbose=FALSE,type="point") {
  if(verbose) print("dx")
  if(verbose) print("subset common eofs")
  ceof <- subset.commonEOF(ceof,is=is,ip=ip)
  ceof1 <- subset.commonEOF(ceof,it=it1)
  ceof2 <- subset.commonEOF(ceof,it=it2)
  if(verbose) print("transform to fields")
  if(is.null(im)) im <- seq(length(ceof)-2)
  x1 <- lapply(im,function(i) map.commonEOF(ceof1,it=it1,im=i,FUN=FUN,plot=FALSE))
  x2 <- lapply(im,function(i) map.commonEOF(ceof2,it=it2,im=i,FUN=FUN,plot=FALSE))
  if(verbose) print("calculate change")
  dx <- lapply(seq(length(x1)), function(i) apply(x2[[i]],2,FUN)-apply(x1[[i]],2,FUN))
  if(type=="point") {
    if(verbose) print("return change as an aggregated mean value")
    DX <- unlist(lapply(dx,mean,na.rm=TRUE))
  } else if(type=="field") {
    if(verbose) print("return change as a field")
    DX <- list()
    for(i in seq(length(dx))) {
      dx.i <- unlist(dx[[i]])
      dim(dx.i) <- c(1,length(dx.i))
      dx.i <- esd::as.field(dx.i, 1, attr(x1[[i]],"longitude"), attr(x1[[i]],"latitude"), 
                       paste(attr(x1[[i]],"variable"),"change"),attr(x1,"unit"))
      attr(dx.i,"longname") <- paste(attr(x1[[i]],"variable"),"change from",
                                     paste(it1,collapse="-"),"to",paste(it2,collapse="-"))
      DX[[i]] <- dx.i
    }
  } else {
    print("unexpected input 'type' - acceptable options are 'point' or 'field'")
  }
  if(verbose) print("dx - end")
  return(DX)
}
