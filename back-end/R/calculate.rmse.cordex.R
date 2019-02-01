calculate.rmse.cordex <- function(reference="eobs", period=c(1981,2010), variable="tas", 
                                  nfiles=4, continue=TRUE, path=NULL, path.gcm=NULL,
                                  experiment="rcp45", verbose=FALSE) {
  if(verbose) print("calculate.rmse.cordex")
  
  store <- list()
  store.file <- paste("statistics.cordex", reference, variable, paste(period, collapse="-"),
                      experiment, "rda", sep=".")
  if(!is.null(path)) store.file <- file.path(path,store.file)
  if(file.exists(store.file)) load(store.file)
  
  ## Pre-process reference file if necessary
  ref.file <- getReference(reference,variable)
  if(is.null(path)) {
    path <- dirname(ref.file)
  } else if (!dir.exists(path)) {
    path <- getwd()
  }
  if(is.null(path.gcm)) path.gcm <- path
  
  ref.mulc <- paste(reference,"mulc",variable,"nc",sep=".")
  if(!is.character(find.file(ref.mulc)[1])) ref.mulc <- file.path(path,ref.mulc)
  ref.mon.file <- paste(reference,"monmean",variable,"nc",sep=".")
  if(!is.character(find.file(ref.mon.file)[1])) ref.mon.file <- file.path(path,ref.mon.file)
  
  if(variable=="pr") {
    if(!file.exists(ref.mulc)) cdo.command("mulc",1000,ref.file,ref.mulc)
  } else {
    if(!file.exists(ref.mulc)) ref.mulc <- ref.file
  }
  
  if(!file.exists(ref.mon.file)) {
    cdo.command(c("-ymonmean","-selyear"),c("",paste(period,collapse="/")),
                ref.mulc, ref.mon.file)
  }
  
  ## Check which files are processed
  ngcm <- length(cordex.urls(varid=variable,experiment=experiment))
  start <- 1
  if(continue && file.exists(store.file)) {
    start <- as.numeric(tail(sub('.*\\.', '', names(store), perl=TRUE), n=1)) + 1
  }
  if(nfiles=="all") {
    end <- ngcm
  } else {
    end <- min(start + nfiles - 1, ngcm) 
  }
  
  ref <- esd::retrieve(ref.mon.file)
  for(i in start:end) {
    store.name <- paste("cm",i,sep=".")
    gcm.file <- file.path(path.gcm,paste("CM",i,".",variable,".",experiment,".nc",sep=""))
    if(!file.exists(gcm.file)) getGCMs(i, varid=variable, experiment=experiment,
                                       destfile=file.path(path.gcm,gcm.file))
    gcm.mon.file <- file.path(path,"cm.monmean.nc")
    cdo.command(c("-ymonmean","-selyear"),c("",paste(period,collapse="/")),
                gcm.file, gcm.mon.file)
    gcm <- zoo::coredata(esd::retrieve(gcm.mon.file))
    ref.i <- subset(ref,is=list(lon=range(attr(gcm,"longitude")),lat=range(attr(gcm,"latitude"))))
    weights <- calculate.mon.weights(attr(ref.i,"longitude"),attr(ref.i,"latitude"))
    ref.i <- zoo::coredata(ref.i)
    dim(gcm) <- dim(ref.i) <- c(12,length(attr(gcm,"longitude")),length(attr(gcm,"latitude")))
    store[[store.name]]$rms <- sqrt(sum(weights*(gcm-ref.i)^2,na.rm=TRUE)/sum(weights))
    file.remove(gcm.mon.file)
  }
  
  median.rms <- median(unlist(lapply(store, "[","rms")))  
  for(i in start:end){
    store.name <- paste("cm",i,sep=".")
    store[[store.name]]$e <- (store[[store.name]]$rms-median.rms)/median.rms
  }
  save(file=store.file, store)
  file.remove(ref.mon.file)
  invisible(store)
}
