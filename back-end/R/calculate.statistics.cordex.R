calculate.statistics.cordex <- function(reference="eobs", period=c(1981,2010), 
                                        variable="tas", path.rcm=NULL, continue=TRUE,
                                        mask="PrudenceCoords.txt",
                                        experiment="rcp45", verbose=FALSE) {
  
  if(verbose) print("calculate.statistics.cordex")
  shfile <- find.file("RegionSpecifications.csv")[1]
  if(!file.exists(shfile)) print("Warning! Couldn't find PRUDENCE shapefile.")
  prudence <- read.csv(shfile)
  prudence.regions <- as.character(prudence$Code)
  country_shape <- get.shapefile("TM_WORLD_BORDERS-0.3.shp")
  country.regions <- as.character(country_shape$ISO3)
  if(max(period)>2015) reference <- NULL
  
  if(!is.null(reference)) {
    store.file <- paste("statistics.cordex", reference, variable, paste(period, collapse="-"), 
                        experiment, "rda", sep=".")
  } else {
    store.file <- paste("statistics.cordex", variable, paste(period, collapse="-"), 
                        experiment, "rda", sep=".")
  }
  
  store <- list()
  if(file.exists(store.file) & continue) load(store.file)
  
  if(!is.null(reference)) {
    ref.file <- switch(paste(reference, variable, sep = "."), 
                       eobs.tas = "tg_0.50deg_reg_small_v16.0_ymon.nc", 
                       eobs.pr = "rr_0.50deg_reg_small_v16.0_ymon.nc")
    ref.file <- paste(path.rcm,ref.file,sep="")
    
    store.name <- paste(reference,variable,sep=".")
    store[[store.name]]$europe$spatial.sd <- c(cdo.spatSdymon(ref.file), 
                                               cdo.spatSdymon(ref.file,monthly=TRUE))
    store[[store.name]]$europe$mean <- c(cdo.meanymon(ref.file), 
                                         cdo.meanymon(ref.file,monthly=TRUE))
    
    for(i in 1:length(prudence.regions)) {
      getPrudenceCoords(prudence=prudence,region=i,destfile=mask)
      store[[ store.name ]][[ prudence.regions[i] ]]$spatial.sd <- 
        c(cdo.spatSdymon(ref.file,mask=mask), 
          cdo.spatSdymon(ref.file,mask=mask,monthly=TRUE))
      store[[ store.name ]][[ prudence.regions[i] ]]$mean <- 
        c(cdo.meanymon(ref.file,mask=mask), 
          cdo.meanymon(ref.file,mask=mask,monthly=TRUE))
    }
    
    for(i in which(country_shape$REGION==150 & country_shape$AREA > 50*50)) { #All European countries with more than 2500km^2
      pol.coords <- coordinates(country_shape@polygons[[i]]@Polygons[[1]])
      write(t(pol.coords),file="countrycoord.txt",ncolumns = 2)
      
      for (p in 1:length(country_shape@polygons[[i]]@Polygons))
      {
        pol.coords <- coordinates(country_shape@polygons[[i]]@Polygons[[p]])
        write("&",file="countrycoord.txt",append=T)
        write(t(pol.coords),file="countrycoord.txt",ncolumns = 2,append=T)
      }
      
      store[[ store.name ]][[ country.regions[i] ]]$spatial.sd <-
        c(cdo.spatSdymon(ref.file,mask="countrycoord.txt"),
          cdo.spatSdymon(ref.file,mask="countrycoord.txt",monthly=TRUE))
      store[[ store.name ]][[ country.regions[i] ]]$mean <-
        c(cdo.meanymon(ref.file,mask="countrycoord.txt"),
          cdo.meanymon(ref.file,mask="countrycoord.txt",monthly=TRUE))
    }
    
  }
  
  for(m in 0:15){
    rcm.file <- paste(path.rcm,"masked_ymonmean_",paste(period, collapse="-"),
                      "_",variable,"_EUR-44_cordex_",experiment,"_mon_0",
                      sprintf("%02d", m),".nc",sep="")
    store.name <- paste("rcm",m,sep=".")
    store[[store.name]]$europe$spatial.sd <- c(cdo.spatSdymon(rcm.file),
                                               cdo.spatSdymon(rcm.file,monthly=TRUE))
    store[[store.name]]$europe$mean <- c(cdo.meanymon(rcm.file),
                                         cdo.meanymon(rcm.file,monthly=T))
    
    for(i in 1:length(prudence.regions)) {
      getPrudenceCoords(prudence=prudence,region=i,destfile=mask)
      store[[ store.name ]][[ prudence.regions[i] ]]$spatial.sd <- 
        c(cdo.spatSdymon(rcm.file,mask=mask), 
          cdo.spatSdymon(rcm.file,mask=mask,monthly=TRUE))
      store[[ store.name ]][[ prudence.regions[i] ]]$mean <- 
        c(cdo.meanymon(rcm.file,mask=mask), 
          cdo.meanymon(rcm.file,mask=mask,monthly=TRUE))
    }
    
    for(i in which(country_shape$REGION==150 & country_shape$AREA > 50*50)) { #All European countries with more than 2500km^2
      pol.coords <- coordinates(country_shape@polygons[[i]]@Polygons[[1]])
      write(t(pol.coords),file="countrycoord.txt",ncolumns = 2)
      
      for (p in 1:length(country_shape@polygons[[i]]@Polygons))
      {
        pol.coords <- coordinates(country_shape@polygons[[i]]@Polygons[[p]])
        write("&",file="countrycoord.txt",append=T)
        write(t(pol.coords),file="countrycoord.txt",ncolumns = 2,append=T)
      }
      
      store[[ store.name ]][[ country.regions[i] ]]$spatial.sd <-
        c(cdo.spatSdymon(rcm.file,mask="countrycoord.txt"),
          cdo.spatSdymon(rcm.file,mask="countrycoord.txt",monthly=TRUE))
      store[[ store.name ]][[ country.regions[i] ]]$mean <-
        c(cdo.meanymon(rcm.file,mask="countrycoord.txt"),
          cdo.meanymon(rcm.file,mask="countrycoord.txt",monthly=TRUE))
    }
    
    
    if(!is.null(reference)) {
      values <- as.numeric(c(system(paste("cdo -output -fldcor -timmean",rcm.file,ref.file),intern=T),
                             system(paste("cdo -output -fldcor",rcm.file,ref.file),intern=T)))
      names(values) <-     c("ann", "jan", "feb", "mar", "apr", "may", "jun", 
                             "jul", "aug", "sep", "oct", "nov", "dec")              
      store[[store.name]]$europe$corr <- values
      
      
      for(i in 1:length(prudence.regions)) {
        getPrudenceCoords(prudence=prudence,region=i,destfile="PrudenceCoords.txt")
        values <- as.numeric(c(system(paste("cdo -output -fldcor -maskregion,PrudenceCoords.txt -timmean",rcm.file,ref.file),intern=T),
                               system(paste("cdo -output -fldcor -maskregion,PrudenceCoords.txt",rcm.file,ref.file),intern=T)))
        names(values) <-     c("ann", "jan", "feb", "mar", "apr", "may", "jun", 
                               "jul", "aug", "sep", "oct", "nov", "dec")              
        store[[store.name]][[ prudence.regions[i] ]]$corr <- values
        
      }
      
      for(i in which(country_shape$REGION==150 & country_shape$AREA > 50*50)) { #All European countries with more than 2500km^2
        
        pol.coords <- coordinates(country_shape@polygons[[i]]@Polygons[[1]])
        write(t(pol.coords),file="countrycoord.txt",ncolumns = 2)
        
        for (p in 1:length(country_shape@polygons[[i]]@Polygons))
        {
          pol.coords <- coordinates(country_shape@polygons[[i]]@Polygons[[p]])
          write("&",file="countrycoord.txt",append=T)
          write(t(pol.coords),file="countrycoord.txt",ncolumns = 2,append=T)
        }
        
        values <- as.numeric(c(system(paste("cdo -output -fldcor -maskregion,countrycoord.txt -timmean",rcm.file,ref.file),intern=T),
                               system(paste("cdo -output -fldcor -maskregion,countrycoord.txt",rcm.file,ref.file),intern=T)))
        names(values) <-     c("ann", "jan", "feb", "mar", "apr", "may", "jun",
                               "jul", "aug", "sep", "oct", "nov", "dec")
        store[[store.name]][[ country.regions[i] ]]$corr <- values
        
      }
    }
    
    save(file=store.file,store)
  }
  
  return(store)
}



