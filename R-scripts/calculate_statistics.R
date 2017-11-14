#!/usr/bin/env Rscript
setwd(system("find $HOME -name calculate_statistics.R -exec dirname {} \\;",intern=TRUE))
## To install DECM package: 
## R CMD INSTALL abc4cde_wp4/back-end 
## Requires rgdal, raster, esd (zoo, ncdf4), PCICt 
library(DECM)

## Extract metadata for CMIP5 and CORDEX data
verbose <- FALSE
x <- c(getGCMs(select=1:30,varid="tas",verbose=verbose),
       getGCMs(select=1:30,varid="pr",verbose=verbose),
       getRCMs(select=1:20,varid="tas",verbose=verbose),
       getRCMs(select=1:20,varid="pr",verbose=verbose))
metaextract(x,verbose=verbose)

## Calculate statistics for CMIP5 data
opt <- list(verbose=TRUE,reference="era",it=c(1981,2010),variable="tas",
            nfiles=30,continue=FALSE,mask="coords.txt",help=FALSE)
for (varid in c("tas","pr")) {
  print(paste("Statistics of CMIP5",varid))
  print(paste("Calculate weighted RMSE for the period",paste(opt$it,collapse="-")))
  calculate.rmse.cmip(reference=opt$reference, period=opt$it, variable=varid,
                      nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose)
  print("Calculate annual cycle statistics")
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

#calculate.statistics.cordex(reference=opt$reference, period=opt$it, variable=opt$variable, 
#                          nfiles=opt$nfiles, continue=opt$continue, verbose=opt$verbose, 
#                          mask=opt$mask)

