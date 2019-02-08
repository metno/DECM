## Script to extract metadata and calculate statistics.
## The metadata (metaextract.rda) and statistics files (statistics.cmip.era.tas.1981-2010.rda)
## should then be moved to the back-end/data folder.

#!/usr/bin/env Rscript
#setwd(system("find $HOME -name calculate_statistics.R -exec dirname {} \\;",intern=TRUE))
setwd("/home/kajsamp/git/DECM/R-scripts/")
## To install DECM package: 
## R CMD INSTALL DECM/back-end 
## Requires rgdal, raster, esd (zoo, ncdf4), PCICt, RCurl
library(DECM)

get.meta <- FALSE
cmip.rmse <- TRUE
cmip.stats <- FALSE
cordex.rmse <- FALSE
cordex.stats <- FALSE

## Calculate statistics for CMIP5 data
opt <- list(ref.cmip="era", ref.cordex="eobs", verbose=FALSE, it=c(1981,2010),
            nfiles="all", continue=TRUE, mask="coords.txt", help=FALSE,
            path="~/git/DECM/R-scripts")

# Download reference data
if(cmip.rmse | cmip.stats) {
  for (varid in c("tas","pr")) {
    ref <- getReference(opt$ref.cmip,varid)
    # ref=FALSE if file is missing, else ref=filename
    if(is.logical(ref)) { 
      if(opt$verbose) print("Download reference data")
      if(opt$ref.cmip=="era") getERA(variable=varid,verbose=opt$verbose)
    }
  }
}

## Download reference data
if(cordex.rmse | cordex.stats) {
  for (varid in c("tas","pr")) {
    ref <- getReference(opt$ref.cordex,varid) 
    # ref=FALSE if file is missing, else ref=filename
    if(is.logical(ref)) {
      if(opt$verbose) print("Download reference data")
      if(opt$ref.cordex=="eobs") {
        getEOBS(variable=varid,verbose=opt$verbose)
      }
    }
  }
}

if(get.meta) {
  add <- FALSE # Set add=FALSE to create new metadata file or TRUE to add to old file
  for(varid in c("tas","pr")) {
    for(rcp in c("rcp45","rcp85")) {
      x1 <- getGCMs(select=1:110,varid=varid,experiment=rcp,
                   verbose=opt$verbose,path=opt$path)
      y <- metaextract(x1,verbose=opt$verbose,add=add)
      add <- TRUE # change add to TRUE so metadata is added to old file
      x2 <- getRCMs(select=1:20,varid=varid,experiment=rcp,
                    verbose=opt$verbose,path=opt$path)
      y <- metaextract(x2,verbose=opt$verbose,add=add)
    }
  }
}

source("~/git/DECM/back-end/R/calculate.rmse.cmip.R")
# Calculate regional statistics (mean, sd, spatial corr) for CMIP5
if(cmip.stats) {
  for (varid in c("pr","tas")) {
    print(paste("Calculate annual cycle statistics of",varid))
    for (it in list(c(2071,2100),c(2021,2050),opt$it)) {
      for(rcp in c("rcp85","rcp45")) {
        print(paste("period:",paste(it,collapse="-"),"; scenario:",rcp))
        calculate.statistics.cmip(reference=opt$ref.cmip, period=it, variable=varid,
         	path.gcm=opt$path, nfiles=opt$nfiles, continue=opt$continue,
				  mask=opt$mask, experiment=rcp, verbose=opt$verbose)
      }
    }
  }
}

# Calculate rmse and CMPI for CMIP5
if(cmip.rmse) {
  for (varid in c("pr","tas")) {
    print(paste("Calculate weighted RMSE of",varid))
    for(rcp in c("rcp85","rcp45")) {
      print(paste("Scenario:",rcp))
      calculate.rmse.cmip(reference=opt$ref.cmip, period=opt$it, variable=varid, 
                          path.gcm=opt$path, nfiles=opt$nfiles, continue=opt$continue, 
                          experiment=rcp, verbose=opt$verbose)
    }
  }
}

## Calculate statistics for CORDEX data
## Calculate mean, sd, spatial correlation etc
if(cordex.stats) {
  for (varid in c("pr","tas")) {
    print(paste("Calculate annual cycle statistics of",varid))
    for(rcp in c("rcp45","rcp85")) {
      for (it in list(c(2021,2050),c(2071,2100),opt$it)) {
        print(paste("period:",paste(it,collapse="-"),"; scenario:",rcp))
        calculate.statistics.cordex(reference=opt$ref.cordex, period=it, variable=varid,
	        path.gcm=opt$path, nfiles=opt$nfiles, experiment=rcp,
				  continue=opt$continue, verbose=opt$verbose)
      }
    }
  }
}

## Calculate rmse and CMPI
if(cordex.rms) {
  for (varid in c("tas","pr")) {
    print(paste("Calculate weighted RMSE of",varid))
    for(rcp in c("rcp45","rcp85")) {
      print(paste("Scenario:",rcp))
      calculate.rmse.cordex(reference=opt$ref.cordex, period=opt$it, variable=varid, 
                            path.gcm=opt$path, experiment=rcp, nfiles=opt$nfiles,
                            continue=opt$continue, verbose=opt$verbose)
    }
  }
}

