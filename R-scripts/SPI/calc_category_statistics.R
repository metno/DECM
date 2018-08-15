# Calculate changes for a specified SPI (and SPEI) category
library(ncdf4)
setwd("/home/ubuntu/Data/CORDEX_monthly/")

#Functions

#----------------------------------------
#' calcDroughtStatistics
#'
#' @param ID an ID of an open netcdf file 
#' @param varid ID for the variable
#' @param period period over which the drought statistics are calculated
#' @param category drought category
#'
#' @return
#' @export
#'
#' @examples
calcDroughtStatistics <- function(ID, pattern = "spei", period = c(1981,2010), category = "ED"){
  require(ncdf4)
  dims <- lapply(ID$dim[c(1,2,3)], function(x) x$len)
  dates <- createDateVector(ncvar_get(ID, "time"), ncatt_get(ID, "time", "units")$value)
  periodSteps <- which(year(dates) %in% seq(period[1],period[2]))
  start <- periodSteps[1]
  count <- length(periodSteps)
  data <- ncvar_get(ID, varid = pattern, start = c(1,1,start), 
                    count = c(unlist(dims[1:2]), count))
  tempStats <- apply(data, c(1,2), occurrenceStatistics, cat = category)
  return(tempStats)
}

#----------------------------------------
#' occurrenceStatistics
#'
#' Calculate basic occurrence statistics for a specific drought category
#'
#' @param x time series of SPI or SPEI from which basic statistics for changes in 
#' @param category name of the category
#'
#' @return
#' @export
#'
#' @examples
occurrenceStatistics <- function(x, category){
  categories <- list(EW = c(2,999), VW = c(1.5,2), MW = c(1,1.5), NN = c(-1,1),
                     MD = c(-1.5,-1), SD = c(-2,-1.5), ED=c(-999,-2),
                     EW_VW_MW = c(1,999), EW_VW = c(1.5,999), MD_SD_ED = c(-999,-1),
                     SD_ED = c(-999,-1.5))
  lims <- unlist(categories[category])
  if(any(!is.na(x))){
    isInCategory <- (x>=lims[1] & x<lims[2])
    nMonths <- length(which(isInCategory))
    events <- rle(isInCategory)
    nEvents <- length(events$lengths[events$values == T])
    meanEventLength <- mean(events$lengths[events$values == T])
    if(is.nan(meanEventLength)) meanEventLength <- 0
 #   print(meanEventLength)
    return(list(nMonths = nMonths, nEvents = nEvents, meanEventLength = meanEventLength))
  }else{
    return(NA)
  }
}

#----------------------------------------
#' createDateVector
#'
#' @param an array of time steps 
#' @param units reference date, i.e., "units" field for time dimension from a netcdf file.
#'
#' @return a date sequence
#' @export
#'
#' @examples
createDateVector <- function(steps,units){
  require(lubridate)
  reference <- as.Date(unlist(strsplit(units, split = " "))[3])
  start <- reference + steps[1]
  end <- reference + tail(steps, 1)
  return(seq.Date(start, end, "month"))
}

#----------------------------------------
#Main loop

pattern <- "spei"
path <- "/wrk/oraty/DONOTREMOVE/DECM/CORDEX_monthly/"
outPath <- paste("/wrk/oraty/DONOTREMOVE/DECM/SPEI/",toupper(pattern),sep="")

categories <- c("EW","VW","MW","NN","MD","SD","ED","EW_VW_MW","EW_MW","MD_SD_ED","SD_ED")
windows <- c(3,6,12)
period <- c(1981,2010)

for(category in categories){
  
  indexStatistics <- droughtStatistics <- list()
  #tempStats <- list(nMonths=array(),nEvents=array(),meanEventLength=array())

  for(window in windows){

    listFiles <- list.files(path = path, pattern = pattern, full.names = T)
    listFiles <- listFiles[grep(paste("mon","_",window,"_",sep=""),listFiles)]
    
    for(file in 1:length(listFiles)){
      print(file)
      ID <- nc_open(listFiles[file])    
      lat <- ncvar_get(ID, "latitude")
      lon <- ncvar_get(ID, "longitude")
      lat.mat <- matrix(rep(lat, length(lon)), nrow = length(lon), byrow = T)
      tmp <- calcDroughtStatistics(ID = ID, pattern = pattern, period = period, category = category)
      output <- list(nMonths = array(NA, dim = dim(tmp)), nEvents = array(NA, dim = dim(tmp)), 
                  meanEventLength = array(NA, dim = dim(tmp)))
      dimnames(output$nMonths) <- dimnames(output$nEvents) <- dimnames(output$meanEventLength) <- list(lon,lat)
      
      for(i in 1:dim(tmp)[1]){
        for(j in 1:dim(tmp)[2]){
          if(!is.na(tmp[i,j])){
            output$nMonths[i,j] <- tmp[i,j][[1]]$nMonths
            output$nEvents[i,j] <- tmp[i,j][[1]]$nEvents
            output$meanEventLength[i,j] <- tmp[i,j][[1]]$meanEventLength
          }else{
            output$nMonths[i,j] <- output$nEvents[i,j] <- output$meanEventLength[i,j] <- NA
          }
        }
      }
      
      # nMonths <- weighted.mean(output$nMonths,lat.mat,na.rm=T)
      # nEvents <- weighted.mean(output$nEvents,lat.mat,na.rm=T)
      # meanEventLength <- weighted.mean(output$meanEventLength,lat.mat,na.rm=T)
      # indexStatistics[[rcm.name]]$nMonths <- nMonths
      # indexStatistics[[rcm.name]]$nEvents <- nEvents
      # indexStatistics[[rcm.name]]$meanEventLength <- meanEventLength
      
      rcm.name <- paste("rcm",file-1,sep=".")
      droughtStatistics[[rcm.name]]$nMonths <- output$nMonths
      droughtStatistics[[rcm.name]]$nEvents <- output$nEvents
      droughtStatistics[[rcm.name]]$meanEventLength <- output$meanEventLength
      nc_close(ID)
    }
    
    out.name <- paste(paste(pattern, "statistics",
                            window, "mon", category, paste(period, collapse = "-"), sep = "_"), "rda", sep = ".")
    
    save(droughtStatistics, file = out.name)
  }
}
