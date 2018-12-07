## Specific model to retrieve RCMs
getRCMs <- function(select=1:9,varid='tas',experiment='rcp45',destfile=NULL,path=NULL,verbose=FALSE) {
  if(verbose) print("getRCMs")
  ## Set destfiles
  if(is.null(destfile)) destfile <- paste('CM',select,'.',varid,'.',experiment,'.nc',sep='')
  if(!is.null(path)) destfile <- file.path(path,destfile)
  ## Get the urls
  url <- cordex.urls(varid=varid,experiment=experiment)[select]
  ## Set up a list variable to contain all the metadata
  X <- list()
  for (i in seq_along(select)) {
    if(verbose) print(paste("Get rcm.",select[i],sep=""))
    xi <- getCM(url=url[i],destfile=destfile[i],verbose=verbose)
    if(!is.null(xi)) {
      X[[paste('rcm',varid,select[i],sep='.')]] <- xi
    } else{
      if(verbose) print(paste("Failed to download rcm.",select[i]))
    }
  }
  invisible(X)
}
