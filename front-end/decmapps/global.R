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

# GCM statistics ...
stats <- NULL
load("/home/ubuntu/git/DECM/back-end/data/statistics.cmip.era.tas.1981-2010.rda")
stats$tas$present <- store
load("/home/ubuntu/git/DECM/back-end/data/statistics.cmip.tas.2021-2050.rda")
stats$tas$nf <- store
load("/home/ubuntu/git/DECM/back-end/data/statistics.cmip.tas.2071-2100.rda")
stats$tas$ff <- store
load("/home/ubuntu/git/DECM/back-end/data/statistics.cmip.era.pr.1981-2010.rda")
stats$pr$present <- store
load("/home/ubuntu/git/DECM/back-end/data/statistics.cmip.pr.2021-2050.rda")
stats$pr$nf <- store
load("/home/ubuntu/git/DECM/back-end/data/statistics.cmip.pr.2071-2100.rda")
stats$pr$ff <- store

## Function to get instute name from gcmnames
getModel <- function(i) strsplit(x,split =' ')[[i]][2]
x <- gsub('_',' ', gcmnames.45)
models.45 <- apply(as.matrix(1:length(gcmnames.45)),1, getModel)

x <- gsub('_',' ', gcmnames.26)
models.26 <- apply(as.matrix(1:length(gcmnames.26)),1, getModel)

x <- gsub('_',' ', gcmnames.85)
models.85 <- apply(as.matrix(1:length(gcmnames.85)),1, getModel)

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

if (!file.exists('data/srex.rda')) {
  srex <- regions("srex")
  save(srex, file = 'data/srex.rda')
} else
  load('data/srex.rda')

region.names <- c("Global", srex$name)

# function to be used to combine META data with IPCC table
merge.meta.ipcc <- function(meta = meta, ipcc = IPCC.AR5.Table.9.A.1) {
  ## browser()
  meta$model_id <- gsub('\\.','-',meta$model_id)
  ipcc$Model.Name <- gsub('\\.','-',IPCC.AR5.Table.9.A.1$Model.Name)
  ipcc$Model.Name <- gsub('\\(','-',ipcc$Model.Name)
  ipcc$Model.Name <- gsub('\\)','',ipcc$Model.Name)
  
  for (i in 1:dim(meta)[1]) {
    igcm <- meta$model_id[i]
    #cat(i,igcm)
    #cat(sep='\n')
    id <- which(is.element(tolower(ipcc$Model.Name),tolower(igcm)))
    if (length(id) > 0) 
      y <- ipcc[id,]
    else {
      y <- data.frame(t(rep(NA,dim(ipcc)[2])), stringsAsFactors = TRUE)
      colnames(y) <- colnames(ipcc)
    } 
    
    if (i ==1) 
      X <- cbind(meta[i,],y)
    else 
      X <- rbind(X,cbind(meta[i,],y))
  }
  invisible(X)
}

## Load metadata for GCMs
load('data/metaextract_v2.rda')
data(package = 'esd','IPCC.AR5.Table.9.A.1')
meta <- as.data.frame(meta)
META <- meta[,c('source','experiment','institute_id','model_id','parent_experiment_rip','realization','longname','variable','units','timeunit',
                'resolution','longitude','latitude',"experiment_id",'dim1','index','calendar','creation_date',
                'tracking_id','physics_version','forcing','reference','contact','comment','table_id','file')]
#browser()
gcm.meta.tas <- subset(META, subset = (source == 'CMIP5') & (variable == 'tas'))
gcm.meta.tas <- merge.meta.ipcc(meta = gcm.meta.tas,ipcc = IPCC.AR5.Table.9.A.1)

load('data/metaextract_pr.rda')
meta <- as.data.frame(meta)
META <- meta[,c('source','experiment','institute_id','model_id','parent_experiment_rip','realization','longname','variable','units','timeunit',
                'resolution','longitude','latitude',"experiment_id",'dim1','index','calendar','creation_date',
                'tracking_id','physics_version','forcing','reference','contact','comment','table_id','file')]

gcm.meta.pr <- subset(META, subset = (source == 'CMIP5') & (variable == 'pr'))
gcm.meta.pr <- merge.meta.ipcc(gcm.meta.pr,IPCC.AR5.Table.9.A.1)

# Common meta data for all variables
id <- intersect(gcm.meta.pr$model_id,gcm.meta.tas$model_id)
gcm.meta.all <- subset(gcm.meta.tas, subset = is.element(model_id,id))
gcm.meta.all <- merge.meta.ipcc(gcm.meta.all,IPCC.AR5.Table.9.A.1)

## Load meta data for RCMs
data(package = 'DECM','metaextract')
meta <- as.data.frame(meta)
META <- meta[,c('project_id','gcm','gcm_rip','rcm','longname','var','longname','unit','frequency','dates',
                'resolution','lon','lon_unit','lat','lat_unit',
                'frequency','creation_date','url')]

rcm.meta.tas <- subset(META, subset = (project_id == 'CORDEX') & (var == 'tas'))

rcm.meta.pr <- subset(META, subset = (project_id == 'CORDEX') & (var == 'pr'))

id <- intersect(rcm.meta.pr$model_id,rcm.meta.tas$model_id)
rcm.meta.all <- subset(rcm.meta.tas, subset = is.element(rcm,id))

# RCM statistics ...
rcms <- NULL
load("/home/ubuntu/git/DECM/back-end/data/statistics.cordex.eobs.tas.1981-2010.rda")
rcms$tas$present <- store
load("/home/ubuntu/git/DECM/back-end/data/statistics.cordex.tas.2021-2050.rda")
rcms$tas$nf <- store
load("/home/ubuntu/git/DECM/back-end/data/statistics.cordex.tas.2071-2100.rda")
rcms$tas$ff <- store
load("/home/ubuntu/git/DECM/back-end/data/statistics.cordex.eobs.pr.1981-2010.rda")
rcms$pr$present <- store
load("/home/ubuntu/git/DECM/back-end/data/statistics.cordex.pr.2021-2050.rda")
rcms$pr$nf <- store
load("/home/ubuntu/git/DECM/back-end/data/statistics.cordex.pr.2071-2100.rda")
rcms$pr$ff <- store



