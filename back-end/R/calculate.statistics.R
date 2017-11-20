## Functions to calculate basic statistics

## Calculate the mean annual cycle and spatial correlation for CMIP models
calculate.statistics.cmip <- function(reference="era", period=c(1981,2010), variable="tas", 
                                       nfiles=5, continue=TRUE, mask="coords.txt", 
                                       verbose=FALSE) {
  if(verbose) print("calculate.statistics.cmip")
  shape <- get.shapefile("referenceRegions.shp")
  srex.regions <- as.character(shape$LAB)
  if(max(period)>2015) reference <- NULL
  if(!is.null(reference)) {
    ref.file <- getReference(reference,variable)
    if(!is.character(ref.file)) {
      reference <- NULL
      print("Warning! Reference file not found. Continuing without reference data.")
    }
  }
  if(!is.null(reference)) {
    store.file <- paste("statistics.cmip", reference, variable, paste(period, collapse="-"), "rda", sep=".")
  } else {
    store.file <- paste("statistics.cmip", variable, paste(period, collapse="-"), "rda", sep=".")
  }
  store <- list()
  if(file.exists(store.file)) load(store.file)
  
  units <- NULL
  if(!is.null(reference)) {
    reference.raster <- raster(ref.file)
    store.name <- paste(reference,variable,sep=".")
    store[[store.name]]$global$spatial.sd <- c(cdo.spatSd(ref.file,period), cdo.spatSd(ref.file,period,monthly=TRUE))
    store[[store.name]]$global$mean <- c(cdo.mean(ref.file,period), cdo.mean(ref.file,period,monthly=TRUE))
    if (variable=="tas") {
      if(max(abs(store[[store.name]]$global$mean),na.rm=TRUE)>273) {
        units <- "K"
      } else {
        units <- "degrees~Celsius"
      }
    } else if(variable=="pr") {
      if(!is.null(ref.file)) {
        ncid <- nc_open(ref.file)
        units <- ncid$var[[1]]$units
        nc_close(ncid)
      } else {
        if(max(abs(store[[store.name]]$mean),na.rm=TRUE)<0.001) {
          units <- "kg m-2 s-1"
        } else {
          units <- "mm/day"
        }
      }
    }
    
    for(i in 1:length(srex.regions)) {
      getPolCoords(i,shape=shape,destfile=mask)
      store[[ store.name ]][[ srex.regions[i] ]]$spatial.sd <- 
        c(cdo.spatSd(ref.file,period,mask=mask), 
          cdo.spatSd(ref.file,period,mask=mask,monthly=TRUE))
      store[[ store.name ]][[ srex.regions[i] ]]$mean <- 
        c(cdo.mean(ref.file,period,mask=mask), 
          cdo.mean(ref.file,period,mask=mask,monthly=TRUE))
    }
  }
  
  ngcm <- length(cmip5.urls(varid=variable))
  start <- 1
  if(continue && file.exists(store.file)) {
    start <- as.numeric(tail(sub('.*\\.', '', names(store), perl=TRUE),n=1))+1
  }
  if(nfiles=="all") {
    end <- ngcm
  } else {
    end <- min(start+nfiles-1,ngcm) 
  }
  
  for(i in start:end) {
    X <- getGCMs(select=i,varid=variable)
    gcm.file <- X[[1]]$filename
    store.name <- paste("gcm",i,sep=".")
    store[[store.name]]$global$spatial.sd <- c(cdo.spatSd(gcm.file,period),
                                               cdo.spatSd(gcm.file,period,monthly=TRUE))
    store[[store.name]]$global$mean <- c(cdo.mean(gcm.file,period),
                                         cdo.mean(gcm.file,period,monthly=TRUE))
    if(!is.null(reference)) {
      store[[store.name]]$global$corr <- 
          c(cdo.gridcor(gcm.file,ref.file,period),
            cdo.gridcor(gcm.file,ref.file,period,monthly=TRUE))
    }
    for(j in 1:length(srex.regions)) {
      getPolCoords(j,shape=shape,destfile=mask)
      store[[store.name]][[srex.regions[j]]]$spatial.sd <- 
        c(cdo.spatSd(gcm.file,period,mask=mask), 
          cdo.spatSd(gcm.file,period,mask=mask,monthly=TRUE))
      store[[store.name]][[srex.regions[j]]]$mean <- 
        c(cdo.mean(gcm.file,period,mask=mask), 
          cdo.mean(gcm.file,period,mask=mask,monthly=T))
      if(!is.null(reference)) {
        store[[store.name]][[srex.regions[j]]]$corr <- 
          c(cdo.gridcor(gcm.file,ref.file,period,mask=mask), 
            cdo.gridcor(gcm.file,ref.file,period,mask=mask,monthly=TRUE))
      }
    }
    save(file=store.file,store)
    if (variable=="tas") {
      if(max(abs(store[[store.name]]$global$mean),na.rm=TRUE)>273) {
        units <- c(units,"K")
      } else {
        units <- c(units,"degrees~Celsius")
      }
    } else if(variable=="pr") {
      if(!is.null(gcm.file)) {
        ncid <- nc_open(gcm.file)
        units <- c(units,ncid$var[[1]]$units)
        nc_close(ncid)
      }
    }
    #if(i==ngcm) return(store)
  }
  attr(store,"variable") <- variable
  save(file=store.file,store)
  return(store)
}

calculate.statistics.cordex <- function(reference="eobs", period=c(1981,2010), variable="tas", 
                                        nfiles=5, continue=TRUE, verbose=FALSE) {
  
  region <- read.csv(find.file("RegionSpecifications.csv"))
  region.id <- as.character(region$Code)
  if(max(period)>2015) reference <- NULL
  if(!is.null(reference)) {
    store.file <- paste("statistics.cordex", reference, variable, paste(period, collapse="-"), "rda", sep=".")
  } else {
    store.file <- paste("statistics.cordex", variable, paste(period, collapse="-"), "rda", sep=".")
  }
  
  store <- list()
  if(file.exists(store.file)) load(store.file)
  
  if(!is.null(reference)) {
    ref.file <- getReference(reference,variable)
    reference.raster <- raster(ref.file)
    
    store.name <- paste(reference,variable,sep=".")
    store[[store.name]]$spatial.sd <- c(cdo.spatSd(ref.file,period), 
                                        cdo.spatSd(ref.file,period,monthly=TRUE))
    store[[store.name]]$mean <- c(cdo.mean(ref.file,period), 
                                  cdo.mean(ref.file,period,monthly=TRUE))
  }
  
  ngcm <- length(cordex.urls(varid=variable))
  start <- 1
  if(continue && file.exists(store.file))
    start <- as.numeric(tail(sub('.*\\.', '', names(store), perl=TRUE),n=1))+1
  if(nfiles=="all") {
    end <- ngcm
  } else {
    end <- min(start+nfiles-1,ngcm) 
  }
  
  for(i in start:end){
    X <- getRCMs(select=i,varid=variable)
    gcm.file <- X[[1]]$filename
    store.name <- paste("rcm",i,sep=".")
    store[[store.name]]$spatial.sd <- c(cdo.spatSd(gcm.file,period),
                                        cdo.spatSd(gcm.file,period,monthly=TRUE))
    store[[store.name]]$mean <- c(cdo.mean(gcm.file,period),
                                  cdo.mean(gcm.file,period,monthly=T))
    if(!is.null(reference)) store[[store.name]]$corr <- 
      c(cdo.gridcor(gcm.file,ref.file,period),
        cdo.gridcor(gcm.file,ref.file,period,monthly=TRUE))
    save(file=store.file,store)
    gc()
    if(i==ngcm) return(store)
  }
  return(store)
}

## Calculate weights for the weighted average of rms, assuming monthly time step.
calculate.mon.weights <- function(lon,lat) {
  
  weights <- array(NA,dim=c(12,length(lon),length(lat)))
  time.weights <- c(31,28,31,30,31,30,31,31,30,31,30,31)/365
  lat.weights <- rep(cos(pi*lat/180),length(lon))
  dim(lat.weights) <- c(length(lat),length(lon))
  lat.weights <- t(lat.weights)
  image(lat.weights)
  for(i in 1:length(lat)){
    weights[,,i] <- time.weights
  }
  
  for(i in 1:12){
    weights[i,,] <- weights[i,,]*lat.weights
  }
  
  return(weights)
}

## Calculate the root mean square error (rms) and relative rms (e)
calculate.rmse.cmip <- function(reference="era", period=c(1981,2010), variable="tas", nfiles=4,
                                continue=TRUE, verbose=FALSE, path=NULL) {
  if(verbose) print("calculate.rmse.cmip")
  shfile <- find.file("referenceRegions.shp")[1]
  shape <-  get.shapefile(shfile, with.path = TRUE)

  store <- list()
  store.file <- paste("statistics.cmip", reference, variable, paste(period, collapse="-"),
                      "rda", sep=".")
  #if(is.character(find.file(store.file)[1])) store.file <- find.file(store.file)[1]
  if(file.exists(store.file)) load(store.file)

  ## Pre-process reference file if necessary
  ref.file <- getReference(reference,variable)
  if(is.null(path)) {
    path <- dirname(ref.file)
  } else if (!dir.exists(path)) {
    path <- getwd()
  }
  
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
             infile=ref.mulc,outfile=ref.mon.file)
  }
  ref <- coredata(retrieve(ref.mon.file))
  
  ## Calculate weights only once
  r <- raster(ref)#.mon.file)
  ## KMP 2017-11-13: these weights don't fit the gcm and ref on line 211
  ## and xFromCol/yFromRow(r) doesn't match longitude/latitude(ref)
  #lon <- xFromCol(r)
  #lat <- yFromRow(r)
  lon <- longitude(ref)
  lat <- latitude(ref)
  weights <- calculate.mon.weights(lon,lat)
 
  ## Check which files are processed
  ngcm <- length(cmip5.urls(varid=variable))
  start <- 1
  if(continue && file.exists(store.file)) {
    start <- as.numeric(tail(sub('.*\\.', '', names(store), perl=TRUE), n=1)) + 1
  }
  if(nfiles=="all") {
    end <- ngcm
  } else {
    end <- min(start + nfiles - 1, ngcm) 
  }

  for(i in start:end) {
    store.name <- paste("gcm",i,sep=".")
    gcm.file <- file.path(path,paste("GCM",i,".",variable,".nc",sep=""))
    if(!file.exists(gcm.file)) getGCMs(i, varid=variable, destfile=file.path(path,gcm.file))
    gcm.mon.file <- file.path(path,"gcm.monmean.nc")
    cdo.command(c("-ymonmean","-selyear"),c("",paste(period,collapse="/")),
                infile=gcm.file,outfile=gcm.mon.file)
    gcm <- coredata(retrieve(gcm.mon.file))
    dim(gcm) <- dim(ref) <- c(12,length(longitude(gcm)),length(latitude(gcm)))
    store[[store.name]]$global$rms <- sqrt(sum(weights*(gcm-ref)^2)/sum(weights))
    for(region in levels(shape$LAB)) {
      polygon <- shape[which(levels(shape$LAB)==region),]
      mask <- gen.mask.srex(destfile=gcm.file,mask.polygon=polygon)
      dim(gcm) <- dim(ref) <- c(12,length(longitude(ref))*length(latitude(ref)))
      gcm.masked <- mask.zoo(gcm,mask)
      ref.masked <- mask.zoo(ref,mask)
      dim(gcm.masked) <- dim(ref.masked) <- 
        c(12,length(longitude(gcm)),length(latitude(gcm)))
      store[[store.name]][[region]]$rms <- 
        sqrt(sum(weights*(gcm.masked-ref.masked)^2,na.rm=TRUE)/
             sum(weights[!is.na(gcm.masked)]))
    }
    file.remove(gcm.mon.file)
  }
  
  median.rms <- list()
  for(region in names(store[[1]])) median.rms[[region]] <- median(unlist(lapply(store, function(x) x[[region]]$rms)))
  for(i in start:end){
    store.name <- paste("gcm",i,sep=".")
    for(region in names(store[[1]])){
      store[[store.name]][[region]]$e <- 
        (store[[store.name]][[region]]$rms-median.rms[[region]])/median.rms[[region]]
    }
  }
  save(file=store.file, store)
  file.remove(ref.mon.file)
  invisible(store)
}

calculate.rmse.cordex <- function(reference="eobs", period=c(1981,2010), variable="tas", nfiles=4,
                                  continue=TRUE, verbose=FALSE, path=NULL) {
  if(verbose) print("calculate.rmse.cordex")

  store <- list()
  store.file <- paste("statistics.cordex", reference, variable, paste(period, collapse="-"),
                      "rda", sep=".")
  #if(is.character(find.file(store.file)[1])) store.file <- find.file(store.file)[1]
  if(file.exists(store.file)) load(store.file)
  
  ## Pre-process reference file if necessary
  ref.file <- getReference(reference,variable)
  if(is.null(path)) {
    path <- dirname(ref.file)
  } else if (!dir.exists(path)) {
    path <- getwd()
  }
  
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
                infile=ref.mulc,outfile=ref.mon.file)
  }
  ref <- coredata(retrieve(ref.mon.file))
  
  ## Calculate weights only once
  r <- raster(ref)#.mon.file)
  ## KMP 2017-11-13: these weights don't fit the gcm and ref on line 211
  ## and xFromCol/yFromRow(r) doesn't match longitude/latitude(ref)
  #lon <- xFromCol(r)
  #lat <- yFromRow(r)
  lon <- longitude(ref)
  lat <- latitude(ref)
  weights <- calculate.mon.weights(lon,lat)
  
  ## Check which files are processed
  ngcm <- length(cordex.urls(varid=variable))
  start <- 1
  if(continue && file.exists(store.file)) {
    start <- as.numeric(tail(sub('.*\\.', '', names(store), perl=TRUE), n=1)) + 1
  }
  if(nfiles=="all") {
    end <- ngcm
  } else {
    end <- min(start + nfiles - 1, ngcm) 
  }
  
  for(i in start:end) {
    store.name <- paste("cm",i,sep=".")
    gcm.file <- file.path(path,paste("CM",i,".",variable,".nc",sep=""))
    if(!file.exists(gcm.file)) getGCMs(i, varid=variable, destfile=file.path(path,gcm.file))
    gcm.mon.file <- file.path(path,"cm.monmean.nc")
    cdo.command(c("-ymonmean","-selyear"),c("",paste(period,collapse="/")),
                infile=gcm.file,outfile=gcm.mon.file)
    gcm <- coredata(retrieve(gcm.mon.file))
    ref.i <- subset(ref,is=gcm)
    dim(gcm) <- dim(ref) <- c(12,length(longitude(gcm)),length(latitude(gcm)))
    store[[store.name]]$rms <- sqrt(sum(weights*(gcm-ref)^2)/sum(weights))
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
