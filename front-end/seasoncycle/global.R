## global.R
## Help functions for the shiny app "seasoncycle"
library(esd)
library(shiny)
library(sp)
library(DECM)

## Load geographical data for map
data(package="esd", "geoborders",envir=environment())
  
data(package="DECM", "metaextract", envir=environment())

## Load statistics calculated with script 'calculate_statistics.R'
stats <- NULL
data(package="DECM", "statistics.cmip.era.tas.1981-2010.rcp45",
     envir=environment())
stats$tas$present <- store
data(package="DECM","statistics.cmip.tas.2021-2050.rcp45",
     envir=environment())
stats$tas$nf <- store
data(package="DECM","statistics.cmip.tas.2071-2100.rcp45",
     envir=environment())
stats$tas$ff <- store
data(package="DECM","statistics.cmip.era.pr.1981-2010.rcp45",
     envir=environment())
stats$pr$present <- store
data(package="DECM","statistics.cmip.pr.2021-2050.rcp45",
     envir=environment())
stats$pr$nf <- store
data(package="DECM","statistics.cmip.pr.2071-2100.rcp45",
     envir=environment())
stats$pr$ff <- store

im.tas <- meta$project_id=="CMIP5" & meta$var=="tas" & meta$experiment=="RCP4.5"
im.pr <- meta$project_id=="CMIP5" & meta$var=="pr" & meta$experiment=="RCP4.5"
gcms.tas <- paste(meta$gcm[im.tas],".",meta$gcm_rip[im.tas],sep="")
gcms.pr <- paste(meta$gcm[im.pr],".",meta$gcm_rip[im.pr],sep="")
gcms.both <- gcms.tas[gcms.tas %in% gcms.pr]
im.tas <- which(gcms.tas %in% gcms.both)
im.pr <- which(gcms.pr %in% gcms.both)
gcmnames <- paste(seq(length(gcms.both)),": ",gcms.both,sep="")
  
regions <- function(type=c("srex","prudence"),region=NULL) {
  if(is.null(type) | length(type)>1) region <- NULL
  if(is.null(type) | "srex" %in% tolower(type)) {
    f <- "../../back-end/inst/extdata/SREX_regions/referenceRegions.shp"
    x <- get.shapefile(f, with.path=TRUE)
    ivec <- 1:nrow(x)
    if(!is.null(region)) {
      if(is.numeric(region)) {
        ivec <- region
      } else if(region %in% x$LAB) {
        ivec <- sapply(region, function(y) which(y==x$LAB))
      } else if(region %in% x$NAME) {
        ivec <- sapply(region, function(y) which(y==x$NAME))
      } else {
        print(paste("Unknown region",region))
      }
    }
    y <- list(name=as.character(x$NAME[ivec]), 
              label=as.character(x$LAB[ivec]), 
              usage=as.character(x$USAGE[ivec]),
              type=rep("srex",length(ivec)),
              coords=lapply(ivec, function(i) t(coordinates(x@polygons[[i]]@Polygons[[1]]))))
  } else {
    y <- NULL
  }
  if(is.null(type) | "prudence" %in% tolower(type)) {
    f <- "../../back-end/inst/extdata/PRUDENCE_regions/RegionSpecifications.csv"
    x <- read.table(f,sep=",")
    ivec <- 2:nrow(x)
    names <- as.character(x[2:nrow(x),1])
    labels <- as.character(x[2:nrow(x),2])
    if(!is.null(region)) {
      if(is.numeric(region)) {
        ivec <- region
      } else if(region %in% labels) {
        ivec <- sapply(region, function(y) which(y==labels)+1)
      } else if(region %in% names) {
        ivec <- sapply(region, function(y) which(y==names)+1)
      } else {
        print(paste("Unknown region",region))
      }
    }
    prudence <- list(name=as.character(x[ivec,1]),
                  label=as.character(x[ivec,2]),
                  usage=rep("land",length(ivec)),
                  type=rep("prudence",length(ivec)),
                  coords=lapply(ivec, function(i) 
                    t(matrix(sapply(c(4,5,5,4,4,6,6,7,7,6), 
                             function(j) factor2numeric(x[i,j])),
                             nrow=5,ncol=2))))
    if(is.null(y)) {
      y <- prudence 
    } else {
      y <- mapply(c, y, prudence, SIMPLIFY=FALSE)
    }
  }
  invisible(y)
}

## Function 'regions' is defined in helpers.R
srex <- regions("srex")
