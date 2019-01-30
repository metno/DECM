map.ensemble <- function(ceof,im=NULL,ip=NULL,is=NULL,type=NULL,new=TRUE,FUN="mean",
                         colbar=list(pal=NULL,breaks=NULL,show=TRUE,rev=FALSE),
                         verbose=FALSE) {
  if(verbose) print("map.ensemble")
  if(grepl("cc",type)) {
    if(grepl("t2m|tas|temp",attr(ceof[[2]],"variable"))) {
      colbar$breaks <- seq(-10,10,1)
      if(is.null(colbar$pal)) colbar$pal <- "burd"
    }
    if(grepl("pr",attr(ceof[[2]],"variable"))) {
      colbar$breaks <- seq(-2,2,0.2)
      if(is.null(colbar$pal)) {colbar$pal <- "burd"; colbar$rev <- TRUE} 
    }
    it1 <- c(1971,2000)
    if(grepl("ff",type)) it2 <- c(2071,2100) 
    if(grepl("nf",type)) it2 <- c(2021,2050)
    label.title <- paste(attr(ceof[[2]],"longname")," change\n",
                         "ensemble ",FUN," (",
                         paste(it1,collapse="-")," to ",paste(it2,collapse="-"),")",sep="")
  } else if(grepl("mv",type)) {
    if(is.null(colbar$pal)) {
      if(grepl("t2m|tas|temp",attr(ceof[[2]],"variable"))) {
        if(is.null(colbar$pal)) colbar$pal <- "t2m"
        colbar$breaks <- seq(-60,40,5)
      }
      if(grepl("pr",attr(ceof[[2]],"variable"))) {
        if(is.null(colbar$pal)) colbar$pal <- "precip"
        colbar$breaks <- seq(0,15,0.5)
      }
    }
    if(grepl("ff",type)) it1 <- c(2071,2100) 
    if(grepl("nf",type)) it1 <- c(2021,2050)
    if(grepl("pd",type)) it1 <- c(1971,2000)
    it2 <- NULL
    label.title <- paste("Ensemble mean of ",attr(ceof[[2]],"longname")," (",
                         paste(it1,collapse="-"),")",sep="")
  }
  Y1 <- map.commonEOF(ceof,is=is,im=im,ip=ip,it=it1,FUN=FUN,plot=FALSE)
  Y <- apply(Y1,2,mean,na.rm=TRUE)
  if(!is.null(it2)) {
    Y2 <- map.commonEOF(ceof,is=is,im=im,ip=ip,it=it2,FUN=FUN,plot=FALSE)
    dY <- apply(Y2,2,mean,na.rm=TRUE) - Y
    Y <- dY
  } 
  dim(Y) <- c(1,length(Y))
  Y <- esd::as.field(Y,1,attr(Y1,"longitude"),attr(Y1,"latitude"),
                param=attr(Y1,"variable"),unit=attr(Y1,"unit"))
  if(is.null(colbar$breaks))  {
    if(grepl("cc",type)) {
      colbar$breaks <- pretty(c(-max(abs(Y),na.rm=TRUE),max(abs(Y),na.rm=TRUE)),n=10)
    } else {
      colbar$breaks <- pretty(range(Y,na.rm=TRUE),n=10)
    }
  }
  esd::map.field(Y,new=new,colbar=colbar,main=label.title)
  invisible(Y)
} 
