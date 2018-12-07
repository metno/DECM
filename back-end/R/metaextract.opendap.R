#Function to extract the metadata form NetCDF files on OPeNDAP servers
metaextract.opendap <- function(url=NULL, verbose=FALSE) {
  if(verbose) print("metaextract_opendap")
  
  #Make connection to file and get global attributes, variable and dimension lists
  nc <- ncdf4::nc_open(url)
  globat <- ncdf4::ncatt_get(nc,0)
  varlist <- nc$var
  dimlist <- nc$dim
  ncdf4::nc_close(nc)
  
  #Get the time range in the data (and convert to standard calender)
  tunit <- dimlist$time$units
  tsplit <- unlist(strsplit(tunit,split=" "))
  torigin <- paste(tsplit[3:length(tsplit)],collapse=" ")
  tvals <- dimlist$time$vals
  tdiff <- as.difftime(tvals,units=tsplit[1])
  tdiffsecs <- as.numeric(tdiff, units = "secs")
  caltype <- dimlist$time$calendar
  if (requireNamespace("PCICt", quietly = TRUE)) {
    timeline <- PCICt::as.PCICt(torigin,cal=caltype) + tdiffsecs
  } else {
    print(paste("Warning! Package 'PCICt' needed to read data files with special calendar types.",
          "Please install it if your data has e.g., a 360 day year."))
    timeline <- as.POSIXct(torigin,cal=caltype) + tdiffsecs
  }
  
  #Return a list with: Global attributes, list of variables, list of dimensions, time range
  return(list(globat=globat,varlist=varlist,dimlist=dimlist,url=url,range=range(timeline)))
}

