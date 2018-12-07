#Get daily EOBS data and convert it to monthly averages. Version and resolution
#selection not implemented yet.
getEOBS <- function(variable="tas", destfile=NULL, resolution="0.50", version="14",
                    lon=NULL, lat=NULL, verbose=FALSE) {
  if(verbose) print("getEOBS")
  url.path <- "http://www.ecad.eu/download/ensembles/data/Grid_0.50deg_reg"
  if(variable=="tas") {
    filename <- "tg_0.50deg_reg_v16.0.nc.gz"
  } else if(variable=="pr") {
    filename <- "rr_0.50deg_reg_v16.0.nc.gz"
  } else{
    return("Not implemented yet!")
  }
  if(!file.exists(filename)) download.file(paste(url.path,filename,sep="/"),destfile=filename)
  gunzip(filename)
  filename <- sub("\\.[[:alnum:]]+$", "", filename, perl=TRUE)
  if(is.null(destfile)) destfile <- paste(sub("\\.[[:alnum:]]+$", "", filename, perl=TRUE),"mon.nc",sep="_")
  #if(is.null(destfile)) destfile <- file.path(system("echo $EXTERNAL_DATA",intern=T),
  #                                            paste(sub("\\.[[:alnum:]]+$", "", filename, perl=TRUE),"mon.nc",sep="_"))
  if(verbose) print(destfile)
  commands <- c("-f","nc","-copy","-monavg")
  input <- c("","","","")
  if(!file.exists(destfile)) cdo.command(commands,input,infile=filename,outfile=destfile)
  X <- esd::retrieve(destfile,lon=lon,lat=lat,verbose=verbose)
  cid <- getatt(destfile) 
  cid$url <- paste(url.path,filename,sep="/")
  cid$area.mean <- esd::aggregate.area(X,FUN='mean',na.rm=T)
  cid$area.sd <- esd::aggregate.area(X,FUN='sd',na.rm=T)
  ncid <- ncdf4::nc_open(destfile)
  model <- ncdf4::ncatt_get(ncid,0)
  ncdf4::nc_close(ncid)
  cid$model <- model
  return(cid)
}
