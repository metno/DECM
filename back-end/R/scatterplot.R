scatterplot <- function(x,y,ix=NULL,xlim=NULL,ylim=NULL,xlab=NULL,ylab=NULL,im=NULL,
                        main=NULL,legend=NULL,show.legend=TRUE,pal="cat",pch=21,cex=1.5,lwd=1.5,
                        new=FALSE,verbose=FALSE) {
  if(verbose) print("scatterplot")
  if(is.null(xlab)) xlab <- paste(attr(x,"variable")," (",attr(x,"unit"),")",sep="")
  if(is.null(ylab)) ylab <- paste(attr(y,"variable")," (",attr(y,"unit"),")",sep="")
  if(is.null(main)) main <- ""
  if(is.null(legend) & show.legend) legend <- names(x)
  if(is.null(xlim)) xlim <- range(x,na.rm=TRUE) + c(-1,1)*diff(range(x,na.rm=TRUE))*0.1 
  if(is.null(ylim)) ylim <- range(y,na.rm=TRUE) + c(-1,1)*diff(range(y,na.rm=TRUE))*0.1
  #if(is.null(im)) im <- seq(x)
  if(!is.null(pal)) {
    col <- esd::colscal(n=length(x),col=pal)
  } else {
    col <- rep("blue",length(x))
  }
  if(!is.null(im)) {
    nim <- !(seq(length(col)) %in% im)
    if(any(nim)) col[nim] <- adjustcolor(col[nim], alpha.f=0.2)
  }
  bg <- col
  if(!is.null(ix)) col[im] <- "black"
  if(!is.null(ix)) {
    if(length(cex)==1) cex <- rep(cex,length(x))
    cex[ix] <- cex[1]*1.5
  }
  if(new) dev.new()
  if(is.numeric(pch)) {
    plot(unlist(x),unlist(y),type="n",new=FALSE,xlim=xlim,ylim=ylim,
         xlab=xlab,ylab=ylab,main=main)
    text(unlist(x),unlist(y),labels=as.character(pch),col=col,cex=cex,lwd=lwd)
  } else {
    plot(unlist(x),unlist(y),col=col,pch=pch,cex=cex,lwd=lwd,new=FALSE,
         bg=bg,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,main=main)
  }
  lines(xlim*1.5,rep(0,2),lwd=0.2)
  lines(rep(0,2),ylim*1.5,lwd=0.2)
  grid()
  if(show.legend) legend("bottomleft",ncol=floor(length(x)/3),pch=pch,cex=1,col=col,pt.bg=bg,
                         bg=adjustcolor("white",alpha=0.6),box.lwd=0.5,legend=legend)
}
