testGCM <- function(select=1:9,varid='tas',path=NULL,verbose=FALSE) {
  if(verbose) print("testGCM")
  if(is.null(path)) path <- getwd()
  fnames <- list.files(path=path,pattern=varid,full.names = TRUE)
  X <- list()
  for (i in select) {
    if(verbose) print(fnames[i])
    x <- retrieve(fnames[i],varid=varid)
    ncid <- ncdf4::nc_open(fnames[i])
    ncdf4::nc_close(ncid)
    ncid$area.mean <- aggregate.area(x,FUN='mean')
    ncid$area.sd <- aggregate.area(x,FUN='sd')
    ncid$url <- fnames[i]
    X[[as.character(i)]] <- ncid
  }
  return(X)
}
