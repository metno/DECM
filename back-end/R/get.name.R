# Generate name of GCM or RCM file path/prefix.number.variable.nc
get.name <- function(number,variable,is.rcm=FALSE,verbose=FALSE) {
  if(verbose) print("get.name")
  path <- system("echo $EXTERNAL_DATA",intern=TRUE)
  prefix <- "GCM"
  if(is.rcm) prefix <- "RCM"
  file.name <- paste(paste(prefix,number,sep=""),variable,"nc",sep=".")
  invisible(file.path(path,file.name))
}
