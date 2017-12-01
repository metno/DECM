## Script to extract metadata and calculate statistics.
## The metadata (metaextract.rda) and statistics files (statistics.cmip.era.tas.1981-2010.rda)
## should then be moved to the back-end/data folder.

#!/usr/bin/env Rscript
setwd(system("find $HOME -name calculate_statistics.R -exec dirname {} \\;",intern=TRUE))
## To install DECM package: 
## R CMD INSTALL abc4cde_wp4/back-end 
## Requires rgdal, raster, esd (zoo, ncdf4), PCICt, RCurl
library(DECM)

## Extract metadata for CMIP5 and CORDEX data
verbose <- TRUE
path.data <- getwd() # set path to where you keep the CMIP5 and CORDEX data, if not in working directory
x <- c(getGCMs(select=1:110,varid="tas",verbose=verbose,path=path.data),
       getGCMs(select=1:110,varid="pr",verbose=verbose,path=path.data),
       getRCMs(select=1:20,varid="tas",verbose=verbose,path=path.data),
       getRCMs(select=1:20,varid="pr",verbose=verbose,path=path.data))
Y <- metaextract(x,verbose=verbose)

## Calculate statistics for CMIP5 data
opt <- list(ref.cmip="era",ref.cordex="eobs",verbose=TRUE,it=c(1981,2010),
            nfiles="all",continue=FALSE,mask="coords.txt",help=FALSE,
            path="/lustre/storeA/users/kajsamp/Data/ClimExpl")

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
  calculate.rmse.cmip(reference=opt$ref.cmip, period=opt$it, variable=varid, path.gcm=opt$path,
                      nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose)
}

# Calculate regional statistics (mean, sd, spatial corr) for CMIP5
for (varid in c("tas","pr")) {
  print(paste("Calculate annual cycle statistics for",varid))
  print(paste("period:",paste(opt$it,collapse="-")))
  calculate.statistics.cmip(reference=opt$ref.cmip, period=opt$it, variable=varid, path.gcm=opt$path,
                            nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose,
                            mask=opt$mask)
  for (it in list(c(2021,2050),c(2071,2100))) {
    print(paste("period:",paste(it,collapse="-")))
    calculate.statistics.cmip(reference=NULL, period=it, variable=varid, path.gcm=opt$path,
                              nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose,
                              mask=opt$mask)
  }
}

## Calculate statistics for CORDEX data
## Download reference data
# for (varid in c("tas","pr")) {
#   ref <- getReference(opt$ref.cordex,varid)
#   if(!ref) {
#     if(opt$verbose) print("Download reference data")
#     if(opt$ref.cordex=="eobs") getEOBS(variable=varid,verbose=opt$verbose)
#   }
# }
# 
## Calculate rmse and CMPI
# for (varid in c("tas","pr")) {
#   print(paste("Calculate weighted RMSE of",varid))
#   calculate.rmse.cordex(reference=opt$ref.cordex, period=opt$it, variable=varid, path.gcm=opt$path,
#                       nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose)
# }
# 
## Calculate mean, sd, spatial correlation etc
# for (varid in c("pr","tas")) {
#   print(paste("Calculate annual cycle statistics for",varid))
#   print(paste("period:",paste(opt$it,collapse="-")))
#   calculate.statistics.cordex(reference=opt$ref.cordex, period=opt$it, variable=varid, path.gcm=opt$path,
#                               nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose)
#   for (it in list(c(2021,2050),c(2071,2100))) {
#     print(paste("period:",paste(it,collapse="-")))
#     calculate.statistics.cordex(reference=NULL, period=it, variable=varid, path.gcm=opt$path,
#                                 nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose)
#   }
# }
