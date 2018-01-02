## Rasmus Benestad, Met Norway, 2016-11-02
## R-shiny app that presents empirical-statistical downscaled results. The results include the CMIP5 ensemble simulations
## for RCP4.5, RCP2.6, and RCP 8.5 for a number of stations and for the four different seasons. PCAs and EOFs have been used
## to minimise the needed data volume, and this app expand information embedded in the PCAs/EOFs to corresponding information
## in the form of station series or gridded maps.

library(shiny)
library(esd)
library(leaflet)
library(raster)
library(rgdal)
library(plotly)
library(shinydashboard)
library(car)
library(DECM)
#library(fields)

#if ('RgoogleMaps' %in% installed.packages()) install.packages('RgoogleMaps')
#library(RgoogleMaps)

initialExp <- TRUE

## Messages section / initialization
msgs <- data.frame(from=c('abdelkaderm','mezghani'),
                      message = c('Welcome to DECM prototype tool','Enjoy the prototype')) 

# msgs <- apply(msgData, 1, function(row) {
#   messageItem(from = row[["from"]], message = row[["message"]])
# })

ntfs <- NULL # data.frame(message='abdelkaderm',status = 'success') 
# ntfs <- apply(notData, 1, function(row) {
#   notificationItem(text = row[["message"]], status = row[["status"]])
# })


## Preparations - grid the station data and reduce the size of the data by keeping only
## the most important PCA modes.

## for Input data
Z4 <- Z3 <- list()
load('data/dse.kss.t2m.rcp45.djf.eof.rda')
Z4$tas <- Z
load('data/dse.kss.mu.rcp45.djf.eof.rda')
Z4$pre <- Z
t2m.locs <- sort(loc(Z4[[1]]$pca))
pre.locs <- sort(loc(Z4[[2]]$pca))
#gcmnames <<- names(Z4[[1]])[-c(1,2,length(Z4[[1]]))]
locs2 <- t2m.locs

## For the server
Z4 <- Z3 <- list()
load('data/dse.kss.t2m.rcp45.djf.eof.rda')
Z4$t2m.djf.45 <- Z
if (!file.exists('data/dse.kss.t2m.rcp45.djf.station.rda')) {
  attr(Z,'eof') <- NULL 
  Z<- as.station(Z)
  save(Z,file = 'data/dse.kss.t2m.rcp45.djf.station.rda')
} else 
  load('data/dse.kss.t2m.rcp45.djf.station.rda')
Z3$t2m.djf.45 <- Z

load('data/dse.kss.t2m.rcp45.mam.eof.rda')
Z4$t2m.mam.45 <- Z
if (!file.exists('data/dse.kss.t2m.rcp45.mam.station.rda')) {
  attr(Z,'eof') <- NULL  
  Z <- as.station(Z)
  save(Z,file = 'data/dse.kss.t2m.rcp45.mam.station.rda')
} else 
  load('data/dse.kss.t2m.rcp45.mam.station.rda')
Z3$t2m.mam.45 <- Z

load('data/dse.kss.t2m.rcp45.jja.eof.rda')
Z4$t2m.jja.45 <- Z
if (!file.exists('data/dse.kss.t2m.rcp45.jja.station.rda')) {
  attr(Z,'eof') <- NULL 
  Z <- as.station(Z)
  save(Z,file = 'data/dse.kss.t2m.rcp45.jja.station.rda')
} else 
  load('data/dse.kss.t2m.rcp45.jja.station.rda')
Z3$t2m.jja.45 <- Z

load('data/dse.kss.t2m.rcp45.son.eof.rda')
Z4$t2m.son.45 <- Z
gcmnames.45 <- names(Z)[grep('_',names(Z))]
if (!file.exists('data/dse.kss.t2m.rcp45.son.station.rda')) {
  attr(Z,'eof') <- NULL 
  Z <- as.station(Z)
  save(Z,file = 'data/dse.kss.t2m.rcp45.son.station.rda')
} else 
  load('data/dse.kss.t2m.rcp45.son.station.rda')
Z3$t2m.son.45 <- Z

load('data/dse.kss.t2m.rcp26.djf.eof.rda')
Z4$t2m.djf.26 <- Z
load('data/dse.kss.t2m.rcp26.mam.eof.rda')
Z4$t2m.mam.26 <- Z
load('data/dse.kss.t2m.rcp26.jja.eof.rda')
Z4$t2m.jja.26 <- Z
load('data/dse.kss.t2m.rcp26.son.eof.rda')
Z4$t2m.son.26 <- Z
gcmnames.26 <- names(Z)[grep('_',names(Z))]

load('data/dse.kss.t2m.rcp85.djf.eof.rda')
Z4$t2m.djf.85 <- Z
load('data/dse.kss.t2m.rcp85.mam.eof.rda')
Z4$t2m.mam.85 <- Z
load('data/dse.kss.t2m.rcp85.jja.eof.rda')
Z4$t2m.jja.85 <- Z
load('data/dse.kss.t2m.rcp85.son.eof.rda')
Z4$t2m.son.85 <- Z
gcmnames.85 <- names(Z)[grep('_',names(Z))]

## Wet-day frequency
load('data/dse.kss.fw.rcp45.djf.eof.rda')
Z4$fw.djf.45 <- Z
if (!file.exists('data/dse.kss.fw.rcp45.djf.station.rda')) {
  attr(Z,'eof') <- NULL 
  Z <- as.station(Z)
  save(Z,file = 'data/dse.kss.fw.rcp45.djf.station.rda')
} else 
  load('data/dse.kss.fw.rcp45.djf.station.rda')
Z3$fw.djf.45 <- Z

load('data/dse.kss.fw.rcp45.mam.eof.rda')
Z4$fw.mam.45 <- Z
if (!file.exists('data/dse.kss.fw.rcp45.mam.station.rda')) {
  attr(Z,'eof') <- NULL 
  Z <- as.station(Z)
  save(Z,file = 'data/dse.kss.fw.rcp45.mam.station.rda')
} else 
  load('data/dse.kss.fw.rcp45.mam.station.rda')
Z3$fw.mam.45 <- Z

load('data/dse.kss.fw.rcp45.jja.eof.rda')
Z4$fw.jja.45 <- Z
if (!file.exists('data/dse.kss.fw.rcp45.jja.station.rda')) {
  attr(Z,'eof') <- NULL 
  Z <- as.station(Z)
  save(Z,file = 'data/dse.kss.fw.rcp45.jja.station.rda')
} else 
  load('data/dse.kss.fw.rcp45.jja.station.rda')
Z3$fw.jja.45 <- Z


load('data/dse.kss.fw.rcp45.son.eof.rda')
Z4$fw.son.45 <- Z
if (!file.exists('data/dse.kss.fw.rcp45.son.station.rda')) {
  attr(Z,'eof') <- NULL 
  Z <- as.station(Z)
  save(Z,file = 'data/dse.kss.fw.rcp45.son.station.rda')
} else 
  load('data/dse.kss.fw.rcp45.son.station.rda')
Z3$fw.son.45 <- Z

load('data/dse.kss.fw.rcp26.djf.eof.rda')
Z4$fw.djf.26 <- Z
load('data/dse.kss.fw.rcp26.mam.eof.rda')
Z4$fw.mam.26 <- Z
load('data/dse.kss.fw.rcp26.jja.eof.rda')
Z4$fw.jja.26 <- Z
load('data/dse.kss.fw.rcp26.son.eof.rda')
Z4$fw.son.26 <- Z

load('data/dse.kss.fw.rcp85.djf.eof.rda')
Z4$fw.djf.85 <- Z
load('data/dse.kss.fw.rcp85.mam.eof.rda')
Z4$fw.mam.85 <- Z
load('data/dse.kss.fw.rcp85.jja.eof.rda')
Z4$fw.jja.85 <- Z
load('data/dse.kss.fw.rcp85.son.eof.rda')
Z4$fw.son.85 <- Z

## Wet-day mean precipitation
load('data/dse.kss.mu.rcp45.djf.eof.rda')
Z4$mu.djf.45 <- Z
if (!file.exists('data/dse.kss.mu.rcp45.djf.station.rda')) {
  attr(Z,'eof') <- NULL 
  Z <- as.station(Z)
  save(Z,file = 'data/dse.kss.mu.rcp45.djf.station.rda')
} else 
  load('data/dse.kss.mu.rcp45.djf.station.rda')
Z3$mu.djf.45 <- Z

load('data/dse.kss.mu.rcp45.mam.eof.rda')
Z4$mu.mam.45 <- Z
if (!file.exists('data/dse.kss.mu.rcp45.mam.station.rda')) {
  attr(Z,'eof') <- NULL 
  Z <- as.station(Z)
  save(Z,file = 'data/dse.kss.mu.rcp45.mam.station.rda')
} else 
  load('data/dse.kss.mu.rcp45.mam.station.rda')
Z3$mu.mam.45 <- Z

load('data/dse.kss.mu.rcp45.jja.eof.rda')
Z4$mu.jja.45 <- Z
if (!file.exists('data/dse.kss.mu.rcp45.jja.station.rda')) {
  attr(Z,'eof') <- NULL 
  Z <- as.station(Z)
  save(Z,file = 'data/dse.kss.mu.rcp45.jja.station.rda')
} else 
  load('data/dse.kss.mu.rcp45.jja.station.rda')
Z3$mu.jja.45 <- Z

load('data/dse.kss.mu.rcp45.son.eof.rda')
Z4$mu.son.45 <- Z
if (!file.exists('data/dse.kss.mu.rcp45.son.station.rda')) {
  attr(Z,'eof') <- NULL 
  Z <- as.station(Z)
  save(Z,file = 'data/dse.kss.mu.rcp45.son.station.rda')
} else 
  load('data/dse.kss.mu.rcp45.son.station.rda')
Z3$mu.son.45 <- Z


load('data/dse.kss.mu.rcp26.djf.eof.rda')
Z4$mu.djf.26 <- Z
load('data/dse.kss.mu.rcp26.mam.eof.rda')
Z4$mu.mam.26 <- Z
load('data/dse.kss.mu.rcp26.jja.eof.rda')
Z4$mu.jja.26 <- Z
load('data/dse.kss.mu.rcp26.son.eof.rda')
Z4$mu.son.26 <- Z

load('data/dse.kss.mu.rcp85.djf.eof.rda')
Z4$mu.djf.85 <- Z
load('data/dse.kss.mu.rcp85.mam.eof.rda')
Z4$mu.mam.85 <- Z
load('data/dse.kss.mu.rcp85.jja.eof.rda')
Z4$mu.jja.85 <- Z
load('data/dse.kss.mu.rcp85.son.eof.rda')
Z4$mu.son.85 <- Z

load('data/t2m.nordic.rda')
t2m <- Y
load('data/rr.nordic.rda')
rr <- Y
rm('Y')
load('data/quality.rda')
srt.t2m <- order(loc(Z4[[1]]$pca))
srt.pre <- order(loc(Z4[[13]]$pca))                 
t2m.locs <- sort(loc(Z4[[1]]$pca))
pre.locs <- sort(loc(Z4[[13]]$pca))
iview <- 0

## Estimate the probabilities for trend in observation is within the population trends based on of downscaled results
## zoo objects are slow so extract the core data
trendscore <- function(x) {
  it.X <- year(x) 
  X <- coredata(x)
  it.y <- year(attr(x,'station'))
  y <- coredata(attr(x,'station'))
  X <- X[is.element(it.X,it.y),]
  ty <- trend.coef(y)
  tX <- apply(X,2,FUN='trend.coef')
  score <- pnorm(ty,mean=mean(tX),sd=sd(tX))
  return(c(score,lon(x),lat(x)))
}

## Estimate the probabilities for observed values are within the 90% conf. int. of population of downscaled results
## zoo objects are slow so extract the core data
varscore <- function(x) {
  it.X <- year(x) 
  X <- coredata(x)
  it.y <- year(attr(x,'station'))
  y <- coredata(attr(x,'station'))
  X <- X[is.element(it.X,it.y),]
  nX <- sum(apply(cbind(y,X),1,FUN=function(x) x[1] < quantile(x[-1],probs=0.05) | x[1] > quantile(x[-1],probs=0.95)))
  score <- pbinom(nX,size=length(y),prob=0.1)
  return(c(score,lon(x),lat(x)))
}

# Scatter plot data ...
stats <- NULL
load("~/git/DECM/back-end/data/statistics.cmip.era.tas.1981-2010.rda")
stats$tas$present <- store
load("~/git/DECM/back-end/data/statistics.cmip.tas.2021-2050.rda")
stats$tas$nf <- store
load("~/git/DECM/back-end/data/statistics.cmip.tas.2071-2100.rda")
stats$tas$ff <- store
load("~/git/DECM/back-end/data/statistics.cmip.era.pr.1981-2010.rda")
stats$pr$present <- store
load("~/git/DECM/back-end/data/statistics.cmip.pr.2021-2050.rda")
stats$pr$nf <- store
load("~/git/DECM/back-end/data/statistics.cmip.pr.2071-2100.rda")
stats$pr$ff <- store

# regions <- function(type=c("srex","prudence"),region=NULL) {
#   if(is.null(type) | length(type)>1) region <- NULL
#   if(is.null(type) | "srex" %in% tolower(type)) {
#     f <- "~/shiny/DECM/front-end/dpdt/referenceRegions.shp"#find.file("referenceRegions.shp")
#     x <- get.shapefile(f,with.path=TRUE)
#     ivec <- 1:nrow(x)
#     if(!is.null(region)) {
#       if(is.numeric(region)) {
#         ivec <- region
#       } else if(region %in% x$LAB) {
#         ivec <- sapply(region, function(y) which(y==x$LAB))
#       } else if(region %in% x$NAME) {
#         ivec <- sapply(region, function(y) which(y==x$NAME))
#       } else {
#         print(paste("Unknown region",region))
#       }
#     }
#     y <- list(name=as.character(x$NAME[ivec]), 
#               label=as.character(x$LAB[ivec]), 
#               usage=as.character(x$USAGE[ivec]),
#               type=rep("srex",length(ivec)),
#               coords=lapply(ivec, function(i) t(coordinates(x@polygons[[i]]@Polygons[[1]]))))
#     #west=sapply(ivec, function(i) xmin(extent(x[i,]))),
#     #east=sapply(ivec, function(i) xmax(extent(x[i,]))),
#     #south=sapply(ivec, function(i) ymin(extent(x[i,]))),
#     #north=sapply(ivec, function(i) ymax(extent(x[i,]))))
#   } else {
#     y <- NULL
#   }
#   if(is.null(type) | "prudence" %in% tolower(type)) {
#     f <- "RegionSpecifications.csv"#find.file("RegionSpecifications.csv")
#     x <- read.table(f,sep=",")
#     ivec <- 2:nrow(x)
#     names <- as.character(x[2:nrow(x),1])
#     labels <- as.character(x[2:nrow(x),2])
#     if(!is.null(region)) {
#       if(is.numeric(region)) {
#         ivec <- region
#       } else if(region %in% labels) {
#         ivec <- sapply(region, function(y) which(y==labels)+1)
#       } else if(region %in% names) {
#         ivec <- sapply(region, function(y) which(y==names)+1)
#       } else {
#         print(paste("Unknown region",region))
#       }
#     }
#     prudence <- list(name=as.character(x[ivec,1]),
#                      label=as.character(x[ivec,2]),
#                      usage=rep("land",length(ivec)),
#                      type=rep("prudence",length(ivec)),
#                      coords=lapply(ivec, function(i) 
#                        t(matrix(sapply(c(4,5,5,4,4,6,6,7,7,6), 
#                                        function(j) factor2numeric(x[i,j])),
#                                 nrow=5,ncol=2))))
#     #west=as.numeric(x[2:nrow(x),4]),
#     #east=as.numeric(x[2:nrow(x),5]),
#     #south=as.numeric(x[2:nrow(x),6]),
#     #north=as.numeric(x[2:nrow(x),7]))
#     if(is.null(y)) {
#       y <- prudence 
#     } else {
#       y <- mapply(c, y, prudence, SIMPLIFY=FALSE)
#     }
#   }
#   invisible(y)
# }

#srex <- regions("srex")
## helpers.R
## Help functions for the shiny app "dpdt"


## Function to get instute name from gcmnames
getModel <- function(i) strsplit(x,split =' ')[[i]][2]
x <- gsub('_',' ', gcmnames.45)
models.45 <- apply(as.matrix(1:length(gcmnames.45)),1, getModel)

x <- gsub('_',' ', gcmnames.26)
models.26 <- apply(as.matrix(1:length(gcmnames.26)),1, getModel)

x <- gsub('_',' ', gcmnames.85)
models.85 <- apply(as.matrix(1:length(gcmnames.85)),1, getModel)


### For the seasonal cycle menu Item

## Load statistics calculated with script 'calculate_statistics.R'
stats <- NULL
data("statistics.cmip.era.tas.1981-2010")
stats$tas$present <- store
data("statistics.cmip.tas.2021-2050")
stats$tas$nf <- store
data("statistics.cmip.tas.2071-2100")
stats$tas$ff <- store
data("statistics.cmip.era.pr.1981-2010")
stats$pr$present <- store
data("statistics.cmip.pr.2021-2050")
stats$pr$nf <- store
data("statistics.cmip.pr.2071-2100")
stats$pr$ff <- store

## Help functions for the shiny app "seasoncycle"

regions <- function(type=c("srex","prudence"),region=NULL) {
  if(is.null(type) | length(type)>1) region <- NULL
  if(is.null(type) | "srex" %in% tolower(type)) {
    f <- "data/referenceRegions.shp"#find.file("referenceRegions.shp")
    x <- get.shapefile(f[1],with.path=TRUE)
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
    f <- "RegionSpecifications.csv"#find.file("RegionSpecifications.csv")
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
if (!file.exists('data/srex.rda')) {
  srex <- regions("srex")
  save(srex, file = 'data/srex.rda')
} else
  load('data/srex.rda')

region.names <- c("Global", srex$name)
# 
#   "Alaska/N.W. Canada [ALA:1]","Amazon [AMZ:7]",
#   "Central America/Mexico [CAM:6]","small islands regions Caribbean",
#   "Central Asia [CAS:20]","Central Europe [CEU:12]",
#   "Canada/Greenland/Iceland [CGI:2]","Central North America [CNA:4]",
#   "East Africa [EAF:16]","East Asia [EAS:22]",
#   "East North America [ENA:5]","South Europe/Mediterranean [MED:13]",
#   "North Asia [NAS:18]","North Australia [NAU:25]",
#   "North-East Brazil [NEB:8]","North Europe [NEU:11]",
#   "Southern Africa [SAF:17]","Sahara [SAH:14]",
#   "South Asia [SAS:23]","South Australia/New Zealand [SAU:26]",
#   "Southeast Asia [SEA:24]","Southeastern South America [SSA:10]",
#   "Tibetan Plateau [TIB:21]","West Africa [WAF:15]",
#   "West Asia [WAS:19]","West North America [WNA:3]",
#   "West Coast South America [WSA:9]","Antarctica",
#   "Arctic","Pacific Islands region[2]",
#   "Southern Topical Pacific","Pacific Islands region[3]",
#   "West Indian Ocean")

