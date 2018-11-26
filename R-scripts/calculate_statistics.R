## Script to extract metadata and calculate statistics.
## The metadata (metaextract.rda) and statistics files (statistics.cmip.era.tas.1981-2010.rda)
## should then be moved to the back-end/data folder.

#!/usr/bin/env Rscript
setwd(system("find $HOME -name calculate_statistics.R -exec dirname {} \\;",intern=TRUE))
## To install DECM package: 
## R CMD INSTALL DECM/back-end 
## Requires rgdal, raster, esd (zoo, ncdf4), PCICt, RCurl
library(DECM)

## Calculate statistics for CMIP5 data
opt <- list(ref.cmip="era",ref.cordex="eobs",verbose=TRUE,it=c(1981,2010),
            nfiles="all",continue=FALSE,mask="coords.txt",help=FALSE,
            path="~/git/DECM/R-scripts")

# Set add=FALSE so that a new metadata file is created.
add <- FALSE
for(varid in c("tas","pr")) {
  for(rcp in c("rcp85","rcp45")) {
    x <- getGCMs(select=1:110,varid=varid,experiment=rcp,
                 verbose=opt$verbose,path=opt$path)
    y <- metaextract(x,verbose=opt$verbose,add=add)
    # Change add to TRUE so that the metadata is added to the old file.
    add <- TRUE
    x <- getRCMs(select=1:20,varid=varid,experiment=rcp,
                 verbose=opt$verbose,path=opt$path)
    y <- metaextract(x,verbose=opt$verbose,add=add)
  }
}

# Download reference data
for (varid in c("tas","pr")) {
  ref <- getReference(opt$ref.cmip,varid)
  if(!ref) {
    if(opt$verbose) print("Download reference data")
    if(opt$ref.cmip=="era") getERA(variable=varid,verbose=opt$verbose)
  }
}

# Calculate rmse and CMPI for CMIP5
for (varid in c("tas","pr")) {
  print(paste("Calculate weighted RMSE of",varid))
  for(experiment in c("rcp45","rcp85")) {
    calculate.rmse.cmip(reference=opt$ref.cmip, period=opt$it, variable=varid, 
                        path.gcm=opt$path, nfiles=opt$nfiles, continue=opt$continue, 
                        experiment=rcp, verbose=opt$verbose)
  }
}

# Calculate regional statistics (mean, sd, spatial corr) for CMIP5
for (varid in c("tas","pr")) {
  print(paste("Calculate annual cycle statistics for",varid))
  print(paste("period:",paste(opt$it,collapse="-")))
  for(rcp in c("rcp45","rcp85")) {
    calculate.statistics.cmip(reference=opt$ref.cmip, period=opt$it, variable=varid, 
                              path.gcm=opt$path, nfiles=opt$nfiles, continue=opt$continue, 
                              mask=opt$mask, experiment=rcp, verbose=opt$verbose)
    for (it in list(c(2071,2100),c(2021,2050))) {
      print(paste("period:",paste(it,collapse="-"),rcp))
      calculate.statistics.cmip(reference=NULL, period=it, variable=varid, path.gcm=opt$path,
                                nfiles=opt$nfiles, continue=opt$continue, mask=opt$mask, 
                                experiment=rcp, verbose=opt$verbose)
    }
  }
}

## Calculate statistics for CORDEX data
## Download reference data
for (varid in c("tas","pr")) {
  ref <- getReference(opt$ref.cordex,varid)
  if(!ref) {
    if(opt$verbose) print("Download reference data")
    if(opt$ref.cordex=="eobs") getEOBS(variable=varid,verbose=opt$verbose)
  }
}

## Calculate rmse and CMPI
for (varid in c("tas","pr")) {
  print(paste("Calculate weighted RMSE of",varid))
  for(rcp in c("rcp45","rcp85")) {
    calculate.rmse.cordex(reference=opt$ref.cordex, period=opt$it, variable=varid, 
                          path.gcm=opt$path, experiment=rcp, nfiles=opt$nfiles,
                          continue=opt$continue, verbose=opt$verbose)
  }
}
 
## Calculate mean, sd, spatial correlation etc
for (varid in c("pr","tas")) {
  print(paste("Calculate annual cycle statistics for",varid))
  print(paste("period:",paste(opt$it,collapse="-")))
  for(rcp in c("rcp45","rcp85")) {
    calculate.statistics.cordex(reference=opt$ref.cordex, period=opt$it, variable=varid, 
                                path.gcm=opt$path, experiment=rcp, nfiles=opt$nfiles,
                                continue=opt$continue, verbose=opt$verbose)
    for (it in list(c(2021,2050),c(2071,2100))) {
      print(paste("period:",paste(it,collapse="-")))
      calculate.statistics.cordex(reference=NULL, period=it, variable=varid, path.gcm=opt$path,
                                  nfiles=opt$nfiles, experiment=rcp, continue=opt$continue, 
                                  verbose=opt$verbose)
    }
  }
}
