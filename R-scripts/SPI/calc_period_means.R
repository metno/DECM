# This script calculates both SPI and SPEI using SPEI package available for R.
# The script requires monthly precipitation as input for SPI. The script assumes that the unit is kg/m²s⁻¹
# which is converted to monthly sum.
# For calculating SPEI, monthly mean minimum and maximum temperature are also required.

library(ncdf4)
library(lubridate)
library(Hmisc)

#----------------------------------------
#' createDates
#'
#' A helper function for creating date vector
#'
#' @param refTime reference time
#' @param timeSteps numeric values of the model time steps
#'
#' @return
#' @export
#'
#' @examples
createDates <- function(refTime, timeSteps){
  refTime <- as.Date(strsplit(refTime, " ")[[1]][3])
  dates <- refTime+timeSteps
  return(dates)
}

#----------------------------------------
#' getTimeBounds
#'
#' A helper function to find start and end of a particular period from the model dates
#'
#' @param period the period to be searched
#' @param dates a vector of dates
#'
#' @return
#' @export
#'
#' @examples
getTimeBounds <- function(period,dates){
  start <- min(which(year(dates) == period[1]))
  end <- max(which(year(dates) == period[2]))
  if(is.infinite(end)) end <- length(dates)
  return(c(start,end))
}

#-----------------------------------
#Main loops are here

period <- c(2071,2100)


path <- "/wrk/oraty/DONOTREMOVE/DECM/CORDEX_monthly" #Change if needed
pathOut <- "/homeappl/home/oraty/appl_taito/R/DECM_tests/SPI_interactive"
pr.files <- list.files(path, full.names = T, pattern = "pr_")
tas.files <- list.files(path, full.names = T, pattern = "tas_")
tasmin.files <- list.files(path, full.names = T, pattern = "tasmin_")
tasmax.files <- list.files(path, full.names = T, pattern = "tasmax_")
mask.file <- list.files(path, full.names = T, pattern = "sftlf_")
lsmask <- paste(path,"lsmask.nc",sep="/")
ls.id <- nc_open(lsmask)

pr.mean <- tas.mean <- tasmax.mean <- tasmin.mean <- list()

for(i in 1:length(pr.files)){
    name <- paste("rcm",(i-1),sep=".")
    pr.id <- nc_open(pr.files[i])  
    print(pr.files[i])
    timeSteps <- ncvar_get(pr.id,"time")
    refTime <- ncatt_get(pr.id,"time")$units
    dates <- createDates(refTime,timeSteps)
    monthDays <- monthDays(dates)
    refBounds <- getTimeBounds(period,dates)  
    
    lon <- ncvar_get(pr.id,"longitude")  
    lat <- ncvar_get(pr.id,"latitude")  
    
    if(i==1){
      ls.lon <- ncvar_get(ls.id,"lon")  
      ls.lat <- ncvar_get(ls.id,"lat")  
      lon.ind <- match(lon,ls.lon)
      lat.ind <- match(lat,ls.lat)
      ls.mask <- ncvar_get(ls.id,"lsmask")[lon.ind,lat.ind] 
      ls.mask[ls.mask==0] <- NA
    }
    
    pr <- ncvar_get(pr.id,varid="pr",
                    start=c(1,1,refBounds[1]),
                    count=c(length(lon),length(lat),(refBounds[2]-refBounds[1])+1))*86400
    
    pr.mean[[name]] <- apply(pr,c(1,2),mean,na.rm=T)
    
    for(lonInd in 1:dim(ls.mask)[1]){
      for(latInd in 1:dim(ls.mask)[2]){
        if(is.na(ls.mask[lonInd,latInd]))pr.mean[[name]][lonInd,latInd] <- NA
      }
    }
    rownames(pr.mean[[name]]) <- lon
    colnames(pr.mean[[name]]) <- lat
    nc_close(pr.id)
}
periodOut <- paste(period,collapse="-")
outName <- paste(paste(paste(pathOut, "pr_EUR-44_cordex_rcp45_period_mean", sep = "/"), periodOut, sep = "_"),"rda",sep = ".")
save(pr.mean, file = outName)

for(i in 1:length(tas.files)){
    tas.id <- nc_open(tas.files[i])
    print(tas.files[i])
    name <- paste("rcm",(i-1),sep=".")
    timeSteps <- ncvar_get(tas.id,"time")
    refTime <- ncatt_get(tas.id,"time")$units
    dates <- createDates(refTime,timeSteps)
    monthDays <- monthDays(dates)
    refBounds <- getTimeBounds(period,dates)  
    
    lon <- ncvar_get(tas.id,"longitude")  
    lat <- ncvar_get(tas.id,"latitude")  
    
    tas <- ncvar_get(tas.id,varid="tas",
                    start=c(1,1,refBounds[1]),
                    count=c(length(lon),length(lat),(refBounds[2]-refBounds[1])+1))
    if(max(tas,na.rm=T)>100) tas <- tas-273.15
    
    tas.mean[[name]] <- apply(tas,c(1,2),mean,na.rm=T)
    
    for(lonInd in 1:dim(ls.mask)[1]){
      for(latInd in 1:dim(ls.mask)[2]){
        if(is.na(ls.mask[lonInd,latInd]))tas.mean[[name]][lonInd,latInd] <- NA
      }
    }
    rownames(tas.mean[[name]]) <- lon
    colnames(tas.mean[[name]]) <- lat
    nc_close(tas.id)
}


outName <- paste(paste(paste(pathOut, "tas_EUR-44_cordex_rcp45_period_mean", sep = "/"), periodOut, sep = "_"),"rda",sep = ".")
save(tas.mean, file = outName)

for(i in 1:length(tasmax.files)){
  tasmax.id <- nc_open(tasmax.files[i])
  print(tasmax.files[i])
  name <- paste("rcm",(i-1),sep=".")
  timeSteps <- ncvar_get(tasmax.id,"time")
  refTime <- ncatt_get(tasmax.id,"time")$units
  dates <- createDates(refTime,timeSteps)
  monthDays <- monthDays(dates)
  refBounds <- getTimeBounds(period,dates)  
  
  lon <- ncvar_get(tasmax.id,"longitude")  
  lat <- ncvar_get(tasmax.id,"latitude")  
  
  tasmax <- ncvar_get(tasmax.id,varid="tasmax",
                   start=c(1,1,refBounds[1]),
                   count=c(length(lon),length(lat),(refBounds[2]-refBounds[1])+1))
  if(max(tasmax,na.rm=T)>100) tasmax <- tasmax-273.15
  
  tasmax.mean[[name]] <- apply(tasmax,c(1,2),mean,na.rm=T)
  
  for(lonInd in 1:dim(ls.mask)[1]){
    for(latInd in 1:dim(ls.mask)[2]){
      if(is.na(ls.mask[lonInd,latInd]))tasmax.mean[[name]][lonInd,latInd] <- NA
    }
  }
  rownames(tasmax.mean[[name]]) <- lon
  colnames(tasmax.mean[[name]]) <- lat
  nc_close(tasmax.id)
}

outName <- paste(paste(paste(pathOut, "tasmax_EUR-44_cordex_rcp45_period_mean", sep = "/"), periodOut, sep = "_"),"rda",sep = ".")
save(tasmax.mean, file = outName)


for(i in 1:length(tasmin.files)){
  tasmin.id <- nc_open(tasmin.files[i])
  print(tasmin.files[i])
  name <- paste("rcm",(i-1),sep=".")
  timeSteps <- ncvar_get(tasmin.id,"time")
  refTime <- ncatt_get(tasmin.id,"time")$units
  dates <- createDates(refTime,timeSteps)
  monthDays <- monthDays(dates)
  refBounds <- getTimeBounds(period,dates)  
  
  lon <- ncvar_get(tasmin.id,"longitude")  
  lat <- ncvar_get(tasmin.id,"latitude")  
  
  tasmin <- ncvar_get(tasmin.id,varid="tasmin",
                   start=c(1,1,refBounds[1]),
                   count=c(length(lon),length(lat),(refBounds[2]-refBounds[1])+1))
  if(max(tasmin,na.rm=T)>100) tasmin <- tasmin-273.15
  
  tasmin.mean[[name]] <- apply(tasmin,c(1,2),mean,na.rm=T)
  
  for(lonInd in 1:dim(ls.mask)[1]){
    for(latInd in 1:dim(ls.mask)[2]){
      if(is.na(ls.mask[lonInd,latInd]))tasmin.mean[[name]][lonInd,latInd] <- NA
    }
  }
  rownames(tasmin.mean[[name]]) <- lon
  colnames(tasmin.mean[[name]]) <- lat
  nc_close(tasmin.id)
}

outName <- paste(paste(paste(pathOut, "tasmin_EUR-44_cordex_rcp45_period_mean", sep = "/"), periodOut, sep = "_"),"rda",sep = ".")
save(tasmin.mean, file = outName)

