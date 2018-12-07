## Compute the common EOFs for RCMs save the results for the front-end
commonEOF.rcm <- function(select=1:9,varid='tas',destfile=NULL,destfile.ceof=NULL,
                          it='annual',is=NULL,plot=FALSE,verbose=FALSE) {
  if(verbose) print("commonEOF.rcm")
  if(is.null(destfile)) destfile <- paste(rep('CM',length(select)),select,'.',varid,'.nc',sep='')
  getRCMs(select=select,varid=varid,destfile=destfile,verbose=verbose)
  X <- NULL
  for (fname in destfile) {
    if(verbose) print(paste("retrieve",fname))
    x <- retrieve(fname)
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
  if(plot) esd::plot(ceof)
  
  ## Need to reformat the ceof-object to fit the set-up for the R-shiny app in the front-end.
  if(verbose) print("Reformat the common EOF object")
  x1 <- zoo::coredata(ceof)
  attributes(x1) <- NULL
  dim(x1) <- dim(ceof)
  eof <- esd::as.eof(ceof)
  attr(eof,"standard.error") <- NULL
  Z <- list(info='CORDEX runs',eof=eof,rcm.1=zoo::zoo(x1,order.by=zoo::index(ceof)))
  clim <- list(rcm.1=esd::map.field(X,plot=FALSE))
  rcmnames <- attr(ceof,'model_id')
  gcmnames <- attr(ceof,'driving_model_id')
  gcmrip <- attr(ceof,'driving_model_ensemble_member')
  for (i in 1:attr(ceof,'n.apps')) {
    xi <- attr(ceof,paste('appendix.',i,sep=''))
    rcmnames <- c(rcmnames,attr(xi,'model_id'))
    gcmnames <- c(gcmnames,attr(xi,'driving_model_id'))
    gcmrip <- c(gcmrip,attr(xi,'driving_model_ensemble_member'))
    Z[[paste('rcm.',i+1,sep='')]] <- zoo::zoo(coredata(xi),order.by=zoo::index(xi))
    clim[[paste('rcm.',i+1,sep='')]] <- esd::map.field(attr(X,paste('appendix.',i,sep="")),plot=FALSE)
  }
  attr(Z,'mean') <- clim
  attr(Z,'model_id') <- list(rcm=rcmnames,gcm=gcmnames,gcm_rip=gcmrip)
  class(Z) <- c('dsensemble','eof','list')
  ceof <- Z
  if(is.null(destfile.ceof)) {
    destfile.ceof <- paste('ceof.rcm',varid,it,sep=".")
    if(!is.null(is)) destfile.ceof <- paste(destfile.ceof,".",paste(round(is$lon),collapse="-"),"E.",
                                            paste(round(is$lat),collapse="-"),"N",sep="")
    destfile.ceof <- paste(destfile.ceof,"rda",sep='.')
  }
  save(ceof,file=destfile.ceof)
  invisible(ceof)
}
