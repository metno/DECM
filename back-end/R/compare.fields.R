compare.fields <- function(x,y=NULL,lplot=FALSE,type=c("correlation","rmsd"),
                           filename=NULL,verbose=FALSE,...) {
  if(verbose) print("compare.fields")
  if(!is.null(y) & inherits(x,"field") & inherits(y,"field")) {
    if(verbose) print("Calculate comparative statistics")
    x <- subset(x,is=list(lon=range(lon(y)),lat=range(lat(y))))
    y <- subset(y,is=list(lon=range(lon(x)),lat=range(lat(x))))
    if(inherits(x,"annual") & !inherits(y,"annual")) y <- annual(y)
    if(inherits(y,"annual") & !inherits(x,"annual")) x <- annual(x)
    if(inherits(x,"seasonal") & !inherits(y,"seasonal")) y <- subset(as.4seasons(y),it=season(x)[1])
    if(inherits(y,"seasonal") & !inherits(x,"seasonal")) x <- subset(as.4seasons(x),it=season(y)[1])
    x <- subset(x,it=index(x) %in% index(y))
    y <- subset(y,it=index(y) %in% index(x))
    if(verbose) print("Calculate correlation")
    r <- corfield(x,y,plot=FALSE)
    attr(r,"variable") <- "correlation"
    attr(r,"longname") <- "correlation between fields"
    attr(r,"unit") <- "-1 to -1"
    if(verbose) print("Calculate difference in means")
    mdiff <- apply(x,2,mean)-apply(y,2,mean)
    mdiff <- attrcp(r,mdiff)
    attr(mdiff,"variable") <- "meandiff"
    attr(mdiff,"longname") <- "difference in means"
    attr(mdiff,"unit") <- attr(x,"unit")
    class(mdiff) <- "corfield"
    if(verbose) print("Calculate difference in trend")
    fn <- function(x) if(any(!is.na(x))) return(trend.coef(x)) else return(NA)
    tdiff <- apply(x,2,fn) - apply(y,2,fn)
    tdiff <- attrcp(r,tdiff)
    attr(tdiff,"variable") <- "trenddiff"
    attr(tdiff,"longname") <- "difference in trend"
    attr(tdiff,"unit") <- paste(attr(x,"unit"),"/decade",sep="")
    class(tdiff) <- "corfield"
    if(verbose) print("Calculate RMSD and normalised RMSD")
    rmsd <- apply((x-y)^2,2,function(x) sqrt(sum(x,na.rm=TRUE)/sum(!is.na(x))))
    nrmsd <- 100*rmsd/apply(x,2,function(x) diff(range(x,na.rm=TRUE)))
    rmsd <- attrcp(r,rmsd)
    attr(rmsd,"variable") <- "RMSD"
    attr(rmsd,"longname") <- "root mean square deviation"
    attr(rmsd,"unit") <- attr(x,"unit")
    class(rmsd) <- "corfield"
    nrmsd <- attrcp(r,nrmsd)
    attr(nrmsd,"variable") <- "NRMSD"
    attr(rmsd,"longname") <- "root mean square deviation normalised by the range of the data"
    attr(nrmsd,"unit") <- "%"
    class(nrmsd) <- "corfield"
    z <- list(correlation=r,meandiff=mdiff,trenddiff=tdiff,rmsd=rmsd,nrmsd=nrmsd)
  } else {
    z <- x
  }
  if(lplot) {
    if(verbose) print("Plot comparison between fields")
    stopifnot(inherits(z,"corfield") |
                (inherits(z,"list") & inherits(z[[1]],"corfield")))
    if(inherits(z,"corfield")) {
      eval(parse(text=paste("z <- list(",attr(z,"variable"),"=z)",sep="")))
      type <- c(attr(z,"variable"))
    }
    type <- type[type %in% names(z)]
    if(is.null(type)) type <- names(z)
    if(!is.null(filename)) {
      pdf(filename, 3.2*length(type), 4.0)
    } else {
      dev.new(width=3.2*length(type),height=4.0)
    }
    par(mar=c(4.5,2.5,3.5,0.5),mgp=c(1.5,0.5,0))
    for (i in 1:length(type)) {
      z.i <- z[[which(names(z)==type[[i]])]]
      cb.i <- select.colbar(z.i)
      fig.i <- c((i-1)/length(type),i/length(type),0,1)
      if(i==1) par(fig=fig.i) else par(fig=fig.i,new=TRUE)
      map(z.i,colbar=cb.i)
    }
    if(!is.null(filename)) dev.off()
  }
  invisible(z)
}