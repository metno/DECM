#!/usr/bin/env Rscript
setwd(system("find $HOME -name calculate_statistics.R -exec dirname {} \\;",intern=TRUE))
## To install DECM package: 
## R CMD INSTALL abc4cde_wp4/back-end 
## Requires rgdal, raster, esd (zoo, ncdf4), PCICt 
library(DECM)

## Extract metadata for CMIP5 and CORDEX data
verbose <- TRUE
path.data <- "/lustre/storeA/users/kajsamp/Data/ClimExpl"
x <- c(getGCMs(select=1:110,varid="tas",verbose=verbose,path=path.data),
       getGCMs(select=1:110,varid="pr",verbose=verbose,path=path.data),
       getRCMs(select=1:20,varid="tas",verbose=verbose,path=path.data),
       getRCMs(select=1:20,varid="pr",verbose=verbose,path=path.data))
Y <- metaextract(x,verbose=verbose)

## Calculate statistics for CMIP5 data
opt <- list(reference="era",verbose=TRUE,it=c(1981,2010),variable="tas",
            nfiles="all",continue=FALSE,mask="coords.txt",help=FALSE)

for (varid in c("tas","pr")) {
  ref <- getReference(opt$reference,varid)
  if(!file.exists(ref)) {
    if(opt$verbose) print("Download reference data")
    if(opt$reference=="era") getERA(variable=varid,verbose=opt$verbose)
  }
}

for (varid in c("tas","pr")) {
  print(paste("Calculate weighted RMSE of",varid))
  calculate.rmse.cmip(reference=opt$reference, period=opt$it, variable=varid,
                      nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose)
}

for (varid in c("pr","tas")) {
  print(paste("Calculate annual cycle statistics for",varid))
  print(paste("period:",paste(opt$it,collapse="-")))
  calculate.statistics.cmip(reference=opt$reference, period=opt$it, variable=varid,
                            nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose,
                            mask=opt$mask)
  for (it in list(c(2021,2050),c(2071,2100))) {
    print(paste("period:",paste(it,collapse="-")))
    calculate.statistics.cmip(reference=NULL, period=it, variable=varid,
                              nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose,
                              mask=opt$mask)
  }
}

## Calculate statistics for CORDEX data
# for (varid in c("tas","pr")) {
#   ref <- getReference("eobs",varid)
#   if(!file.exists(ref)) {
#     if(opt$verbose) print("Download reference data")
#     getEOBS(variable=varid,verbose=opt$verbose)
#   }
#   calculate.rmse.cordex(reference="eobs", period=opt$it, variable=varid,
#                         nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose)
#   calculate.statistics.cordex(reference=opt$reference, period=opt$it, variable=opt$variable, 
#                               nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose, 
#                               mask=opt$mask)
#   for (it in list(c(2021,2050),c(2071,2100))) {
#     print(paste("period:",paste(it,collapse="-")))
#     calculate.statistics.cordex(reference=opt$reference, period=opt$it, variable=opt$variable, 
#                                 nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose, 
#                                 mask=opt$mask)
#   }
# }