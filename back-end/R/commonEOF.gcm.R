## Compute the common EOFs for GCMs and save the results for the front-end
commonEOF.gcm <- function(select=1:9,varid='tas',destfile=NULL,destfile.ceof=NULL,
                          it='annual',is=NULL,verbose=FALSE) {
  if(verbose) print("commonEOF.gcm")
  if(is.null(destfile)) destfile <- paste('GCM',select,'.',varid,'.nc',sep='')
  getGCMs(select=select,varid=varid,destfile=destfile)
  X <- NULL
  for (fname in destfile) {
    if(verbose) print(paste("retrieve",fname))
    x <- esd::retrieve(fname,verbose=verbose)
    if (!is.null(it)) {
      if (tolower(it)=='annual') {
        x <- esd::annual(esd::subset(x,is=is),verbose=verbose) 
      } else {
        x <- esd::subset(x,it=it,is=is,verbose=verbose)
      }
    }
    if (is.null(X)) {
      X <- x 
    } else {
      X <- esd::combine(X,x,verbose=verbose)
    }
  }
  if(verbose) print("Calculate common EOF")
  ceof <- esd::EOF(X,verbose=verbose)
  esd::plot(ceof)
  
  ## Need to reformat the ceof-object to fit the set-up for the R-shiny app in the front-end.
  if(verbose) print("Reformat the common EOF object")
  x1 <- zoo::coredata(ceof)
  attributes(x1) <- NULL
  dim(x1) <- dim(ceof)
  eof <- esd::as.eof(ceof)
  attr(eof,"standard.error") <- NULL
  Z <- list(info='CMIP5 runs',eof=eof,rcm.1=zoo::zoo(x1,order.by=zoo::index(ceof)))
  clim <- list(rcm.1=esd::map.field(X,plot=FALSE))
  gcmnames <- attr(ceof,'model_id')
  gcmrip <- attr(ceof,'parent_experiment_rip')
  for (i in 1:attr(ceof,'n.apps')) {
    x1 <- attr(ceof,paste('appendix.',i,sep=''))
    Z[[paste('rcm.',i+1,sep='')]] <- zoo::zoo(coredata(x1),order.by=zoo::index(x1))
    gcmnames <- c(gcmnames,attr(x1,'model_id'))
    gcmrip <- c(gcmrip,attr(x1,'parent_experiment_rip'))
    clim[[paste('rcm.',i+1,sep='')]] <- esd::map.field(attr(X,paste('appendix.',i,sep='')),plot=FALSE)
  }
  attr(Z,'mean') <- clim
  attr(Z,'model_id') <- list(gcm=gcmnames,gcm_rip=gcmrip)
  class(Z) <- c('dsensemble','eof','list')
  ceof <- Z
  if(is.null(destfile.ceof)) {
    destfile.ceof <- paste('ceof.gcm',varid,it,sep=".")
    if(!is.null(is)) fname <- paste(destfile.ceof,".",paste(round(is$lon),collapse="-"),"E.",
                                    paste(round(is$lat),collapse="-"),"N",sep="")
    destfile.ceof <- paste(destfile.ceof,"rda",sep='.')
  }
  save(ceof,file=destfile.ceof)
  invisible(ceof)
}
