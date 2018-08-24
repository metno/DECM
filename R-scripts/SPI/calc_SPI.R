#!/usr/bin/env Rscript

# This script calculates both SPI and SPEI using SPEI package available for R.
# The script requires monthly precipitation as input for SPI. The script assumes that the unit is kg/m²s⁻¹
# which is converted to monthly sum.
# For calculating SPEI, monthly mean minimum and maximum temperature are also required.

setwd("/homeappl/home/oraty/appl_taito/R/MBC/")

#Command-line parameters
suppressPackageStartupMessages({
  require(optparse)
  library(ncdf4)
  library(SPEI)
  library(SCI)
  library(lubridate)
  library(Hmisc)
  library(evd)
})

option_list <- list(
  make_option(c("-v", "--verbose"), action = "store_true", default = TRUE,
              help = "Make the script verbose [default: %default]"),
  make_option(c("-q", "--quietly"), action = "store_false",
              dest = "verbose", help = "Suppress run-time information"),
  make_option(c("-i", "--index"), action = "store", type = "character", default = "SPI",
              help = "Select the index to be calculated [default %default]"),
  make_option(c("-p", "--package"), action = "store", type = "character",
              default = "SPEI", help = "Name of the selected RCM [default %default]"),
  make_option(c("-s", "--scale"), action = "store", type = "numeric",
              default = 3, help = "Select the temporal aggregation scale in months [default %default]"),
  make_option(c("-m", "--mask"), action = "store", type = "character",
              default = "/homeappl/home/oraty/appl_taito/R/DECM/back-end/data/EUR-44_CORDEX/tg_0.50deg_reg_small_v16.0_ymon.nc", help = "file for the land-sea mask")
)

#--------------------------
#
#--------------------------
#' Print information to stdout.
#'
#' @param ... Information to be written to stdout
#'
#' @return Nothing
#' @export
#'
#' @examples 
#' output <- "Hello"
#' stdoutWrite("%s There!",output)
stdoutWrite <- function(...){
  cat(sprintf(...), sep='', file=stdout())
}

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

#----------------------------------------
#' calcHargreaves
#'
#'A wrapper function for calculating evapotranspiration based on Hargreaves formulation
#'
#'
#' @param tasmin monthly average of daily minimum temperature
#' @param tasmax monthly average of daily maximum temperature 
#' @param pr monthly total precipitation (default = NULL). If provided, used to refine monthly evapotranspiration estimates
#' @param lat latitude values for grid boxes in degrees
#'
#' @return
#' @export
#'
#' @examples
calcHargreaves <- function(tasmin, tasmax, pr = NULL, lat){
  dims <- dim(tasmin)
  output <- array(NA,dim=dims)
  for(i in 1:dims[1]){
    print(i)
    for(j in 1:dims[2]){
      
      if(!is.null(pr)){
        output[i,j,] <- hargreaves(Tmin = ts(tasmin[i,j,], frequency = 12, start = c(1951,1)),
                                   Tmax = ts(tasmax[i,j,], frequency = 12, start = c(1951,1)),
                                   Pre = ts(pr[i,j,], frequency = 12, start = c(1951,1)),
                                   lat = lat[j], na.rm = T)
      }else{
        output[i,j,] <- hargreaves(Tmin = ts(tasmin[i,j,], frequency = 12, start = c(1951,1)),
                                   Tmax = ts(tasmax[i,j,], frequency = 12, start = c(1951,1)),
                                   lat = lat[j], na.rm = T)
      }
    }
  }
  return(output)
}

#----------------------------------------
#' removeInf
#'
#' Remove inf values , which might occur if prec or prec-epot values are too 
#' far in the tails of the fitted distribution.
#'
#' @param dat data from which inf values are removed
#'
#' @return dat same but the with inf values replaced with the previous valid value 
#' @export
#'
#' @examples
removeInf <- function(dat){
  first.non.inf <- head(which(!is.infinite(dat)), 1)
  replacement <- dat[first.non.inf]
  for(i in (first.non.inf+1):length(dat)){
    if(is.infinite(dat[i])){
      dat[i] <- replacement
    }else{
      replacement <- dat[i]
    }
  }
  dat[which(is.infinite(dat))] <- 0
  return(dat)
}

#----------------------------------------
#' calcSPI
#'
#'A wrapper function for calculating standardized precipitation index
#'
#' @param data monthly precipitation data used estimate the SPI values
#' @param dates a date vector 
#' @param refBounds data bounds for the reference period
#' @param scale temporal scale, i.e, the size of window over which monthly values are aggregated when calculating SPI
#'
#' @return matrix of SPI time series for the land grid boxes in the CORDEX domain
#' @export
#'
#' @examples
calcSPI <- function(data, dates, refBounds, scale, package = "SPEI", distr = "gamma"){
  
  ref.start.year <- year(dates[refBounds[1]])
  ref.start.month <- month(dates[refBounds[1]])
  
  ref.end.year <- year(dates[refBounds[2]])
  ref.end.month <- month(dates[refBounds[2]])
  
  if(is.null(dim(data))){
    if(any(is.na(data))){
      output <- NA
    }else{
      tmp <- ts.object <- ts(data, frequency = 12, start = c(year(dates[1]),month(dates[1])))
      output <- tmp <- spi(ts.object, scale = scale, 
                           ref.start = c(ref.start.year,ref.start.month), 
                           ref.end = c(ref.end.year,ref.end.month))
    return(output)
    }
  }
  
  output <- array(NA, dim = dim(data))
  inds <- which(!is.na(data[,,1]), arr.ind = T)
  
  for(row in 1:dim(inds)[1]){
#    print(row)
    
    if(package == "SPEI"){
      ts.object <- ts(as.double(data[inds[row,1],inds[row,2],]), frequency = 12, 
                      start = c(year(dates[1]),month(dates[1])))
      tmp <- spi(ts.object, scale = scale, fit = "ub-pwm",na.rm = T,
                  ref.start = c(ref.start.year,ref.start.month), 
                  ref.end = c(ref.end.year,ref.end.month), x = T)
      output[inds[row,1],inds[row,2],] <- c(tmp$fitted)
    }else{
      fit <- fitSCI(data[inds[row,1],inds[row,2],], first.mon = 1, time.scale = scale, 
                    distr = distr, p0 = T, start.fun.fix = T)
      output[inds[row,1],inds[row,2],] <- transformSCI(data[inds[row,1],inds[row,2],], first.mon = 1, obj = fit)
    }
    output[inds[row,1],inds[row,2],]<- removeInf(output[inds[row,1],inds[row,2],])
  }
  
  return(output)
}

#----------------------------------------
#' calcSPEI
#'
#'A wrapper function for calculating standardized precipitation and evapotranspiration index
#'
#' @param data monthly precipitation deficit (prec-epot) data used estimate the SPEI values
#' @param dates a date vector 
#' @param refBounds data bounds for the reference period
#' @param scale temporal scale, i.e, the size of window over which monthly values are aggregated when calculating SPEI
#'
#' @return matrix of SPI time series for the land grid boxes in the CORDEX domain
#' @export
#'
#' @examples
calcSPEI <- function(data, dates, refBounds, scale, package = "SPEI", distr = "gev"){
  
  ref.start.year <- year(dates[refBounds[1]])
  ref.start.month <- month(dates[refBounds[1]])
  
  ref.end.year <- year(dates[refBounds[2]])
  ref.end.month <- month(dates[refBounds[2]])
  
  if(is.null(dim(data))){
    if(any(is.na(data))){
      output <- NA
    }else{
      ts.object <- ts(as.double(data), frequency = 12, start = c(year(dates[1]),month(dates[1])))
      output <- spei(ts.object, scale = scale, fit = "ub-pwm",na.rm = T,
                  ref.start = c(ref.start.year,ref.start.month), 
                  ref.end = c(ref.end.year,ref.end.month), x = T)
      return(output)
    }
  }
  
  output <- array(NA, dim = dim(data))
  inds <- which(!is.na(data[,,1]), arr.ind = T)
  
  for(row in 1:dim(inds)[1]){
    if(package == "SPEI"){
#      print("Using SPEI")
      ts.object <- ts(as.double(data[inds[row,1],inds[row,2],]), frequency = 12, 
                      start = c(year(dates[1]),month(dates[1])))
      tmp <- spei(ts.object, scale = scale, fit = "ub-pwm",na.rm = T,
                  ref.start = c(ref.start.year,ref.start.month), 
                  ref.end = c(ref.end.year,ref.end.month), x = T)
      output[inds[row,1],inds[row,2],] <- c(tmp$fitted)
    }else if(package == "SCI"){
#      print("Using SCI")
      fit <- fitSCI(data[inds[row,1],inds[row,2],], first.mon = 1, time.scale = scale, 
                    distr = distr, p0 = F, start.fun.fix = T)
      output[inds[row,1],inds[row,2],] <- transformSCI(data[inds[row,1],inds[row,2],], first.mon = 1, obj = fit)
    }else{
    print("Wrong package definition, when calculating SPEI")
     return() 
    }
    
    output[inds[row,1],inds[row,2],]<- removeInf(output[inds[row,1],inds[row,2],])
  }
  
  return(output)
}

#----------------------------------------
#' createNcdf
#'
#' @param outName output file name
#' @param dimNames dimension names
#' @param data data for either SPI or SPEI
#' @param lon data for lon dimension
#' @param lat data for lat dimension
#' @param time data for time dimension
#' @param refTime reference time
#' @param name name of the variable to be stored
#' @param long_name long name of the file to be stored
#' @param scale time scale over which the indices have been calculated
#'
#' @return
#' @export
#'
#' @examples
createNcdf <- function(outName, dimNames, data, lon, lat, time, refTime, name, long_name, scale){
  fillvalue <- 1e32
  lonRef <- c("lon","Lon","long","Long","longitude","Longitude")
  latRef <- c("lat","Lat","lati","Lati","latitude","Latitude")
  timeRef <- c("time","tim","Time","Tim")
  lonName <- dimNames[!is.na(match(dimNames,lonRef))]
  latName <- dimNames[!is.na(match(dimNames,latRef))]
  timeName <- dimNames[!is.na(match(dimNames,timeRef))]
  
  timeDim <- ncdim_def(name = timeName, units = refTime, vals = time)
  lonDim <- ncdim_def(name = lonName, units = "degrees_east",vals = as.double(lon))
  latDim <- ncdim_def(name = latName, units = "degrees_north",vals = as.double(lat))
  
  var_def <- ncvar_def(name, "unitless", list(lonDim,latDim,timeDim), fillvalue, long_name, prec = "double")
  
  print(outName)
  ncout <- nc_create(outName, var_def, force_v4 = F)
  
  ncvar_put(ncout, var_def, data)
  ncatt_put(ncout, lonName, "axis", "X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout, latName, "axis", "Y")
  ncatt_put(ncout, timeName, "axis", "T")
  ncatt_put(ncout, name, "scale", scale)
  nc_close(ncout)
}

#-----------------------------------
#Main loops start here

opt <- parse_args(OptionParser(option_list = option_list))
if(opt$verbose){
  stdoutWrite("Command-line Parameters: \n \n")
  stdoutWrite("%s = %s \n",names(opt),opt)
  stdoutWrite("\n")
}

reference <- c(1981,2010)
scale <- opt$scale
package <- opt$package
index <- opt$index
lsmask <- opt$mask

path <- "/wrk/oraty/DONOTREMOVE/DECM/CORDEX_monthly" #Change if needed
pr.files <- list.files(path, full.names = T, pattern = "pr_")
tasmin.files <- list.files(path, full.names = T, pattern = "tasmin_")
tasmax.files <- list.files(path, full.names = T, pattern = "tasmax_")
ls.id <- nc_open(lsmask)

for(i in 1:length(pr.files)){
  pr.id <- nc_open(pr.files[i])  
  print(pr.id$filename)
  pr <- array(NA, dim = c(pr.id$dim$longitude$len,pr.id$dim$latitude$len,pr.id$dim$time$len))
  
  #time dimension
  timeSteps <- ncvar_get(pr.id,"time")
  refTime <- ncatt_get(pr.id,"time")$units
  dates <- createDates(refTime,timeSteps)
  monthDays <- monthDays(dates)
  refBounds <- getTimeBounds(reference,dates)  
  
  #lon and lat dimensions
  lon <- ncvar_get(pr.id,"longitude")  
  lat <- ncvar_get(pr.id,"latitude")  
  
  ls.lon <- ncvar_get(ls.id,"longitude")  
  ls.lat <- ncvar_get(ls.id,"latitude")  
  lon.ind <- match(lon,ls.lon)
  lat.ind <- match(lat,ls.lat)
  ls.mask <- ncvar_get(ls.id,"tg")[lon.ind,lat.ind,1] 
#  ls.mask[ls.mask==0] <- NA
  
  inds <- which(!is.na(ls.mask),arr.ind = T)
  pr.tmp <- ncvar_get(pr.id,varid="pr")*86400
  
  for(k in 1:dim(inds)[1]){
    pr[inds[k,1],inds[k,2],] <- pr.tmp[inds[k,1],inds[k,2],]
  }
  
  for(t in 1:dim(pr)[3]){
    pr[,,t] <- pr[,,t]*monthDays[t] #Scale to mm
  }
  
  if(index == "SPEI"){
    tasmin.id <- nc_open(tasmin.files[i])
    tasmax.id <- nc_open(tasmax.files[i])
    tasmin <- array(NA, dim = c(tasmin.id$dim$longitude$len,tasmin.id$dim$latitude$len,tasmin.id$dim$time$len))
    tasmax <- array(NA, dim = c(tasmax.id$dim$longitude$len,tasmax.id$dim$latitude$len,tasmax.id$dim$time$len))
    
    tasmin.tmp <- ncvar_get(tasmin.id,varid="tasmin")
    for(k in 1:dim(inds)[1]){
      tasmin[inds[k,1],inds[k,2],] <- tasmin.tmp[inds[k,1],inds[k,2],]
    }
    if(max(tasmin,na.rm=T)>100) tasmin <- tasmin-273.15
    
    tasmax.tmp <- ncvar_get(tasmax.id,varid="tasmax")
    for(k in 1:dim(inds)[1]){
      tasmax[inds[k,1],inds[k,2],] <- tasmax.tmp[inds[k,1],inds[k,2],]
    }
    if(max(tasmax,na.rm=T)>100) tasmax <- tasmax-273.15
    
    epot <- calcHargreaves(tasmin,tasmax,pr=NULL,lat)
    dPrec <- pr-epot
    output <- calcSPEI(data = dPrec, dates = dates, refBounds = refBounds, 
                       scale = scale, package = package, distr = "gev")
    name <- "spei"
    long_name <- "standardized precipitation and evapotranspitation index"
    nc_close(tasmax.id)
    nc_close(tasmin.id)
  }else{
    output <- calcSPI(data = pr, dates = dates, refBounds = refBounds, scale = scale, package = package)
    name <- "spi"
    long_name <- "standardized precipitation index"
  }
  
  dimNames <- names(pr.id$dim)
  outName <- gsub("mon_", paste("mon_",scale,"_",package,"_",sep=""), gsub("pr",name,pr.id$filename))
  if(file.exists(outName)) file.remove(outName)
  createNcdf(outName, dimNames, output, lon, lat, time = timeSteps, refTime = refTime, name, long_name, scale)
  nc_close(pr.id)
}
