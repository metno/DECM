## Abdelkader Mezgahni, Rasmus Benestad, Met Norway, 2016-11-02
## R-shiny app for DECM project. The results include the CMIP5 ensemble simulations and EURO-CORDEX simulations 
## for RCP4.5 and RCP 8.5 emission scenarios over predefined global and regional domains.  

library(shiny)
library(esd)
library(leaflet)
library(raster)
library(rgdal)
library(rgeos)
library(plotly)
library(shinydashboard)
library(car)
library(DECM)
library(shinyBS)
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

# CMIP data ...
## GCMs RCP85
## Load statistics calculated with script 'calculate_statistics.R'

gcms <- NULL
load("../../back-end/data/statistics.cmip.era.tas.1981-2010.rda")
gcms$rcp45$tas$present <- store
load("../../back-end/data/statistics.cmip.tas.2021-2050.rda")
gcms$rcp45$tas$nf <- store
load("../../back-end/data/statistics.cmip.tas.2071-2100.rda")
gcms$rcp45$tas$ff <- store
load("../../back-end/data/statistics.cmip.era.pr.1981-2010.rda")
gcms$rcp45$pr$present <- store
load("../../back-end/data/statistics.cmip.pr.2021-2050.rda")
gcms$rcp45$pr$nf <- store
load("../../back-end/data/statistics.cmip.pr.2071-2100.rda")
gcms$rcp45$pr$ff <- store

## GCMs RCP85
load("../../back-end/data/statistics.cmip.era.tas.1981-2010.rcp85.rda")
gcms$rcp85$tas$present <- store
load("../../back-end/data/statistics.cmip.tas.2021-2050.rcp85.rda")
gcms$rcp85$tas$nf <- store
load("../../back-end/data/statistics.cmip.tas.2071-2100.rcp85.rda")
gcms$rcp85$tas$ff <- store
load("../../back-end/data/statistics.cmip.era.pr.1981-2010.rcp85.rda")
gcms$rcp85$pr$present <- store
load("../../back-end/data/statistics.cmip.pr.2021-2050.rcp85.rda")
gcms$rcp85$pr$nf <- store
load("../../back-end/data/statistics.cmip.pr.2071-2100.rcp85.rda")
gcms$rcp85$pr$ff <- store

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

# function to be used to combine META data with IPCC table
merge.meta.ipcc <- function(meta = meta, ipcc = IPCC.AR5.Table.9.A.1) {
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
## Load meta data for RCMs
#data(package='DECM', 'metaextract', envir=environment())

data(package = 'esd','IPCC.AR5.Table.9.A.1')
IPCC.AR5.Table.9.A.1$Horizontal.Resolution <- cmipgcmresolution()

gcm.meta <- function(variable='tas',experiment='RCP4.5') {
  load(paste('../../back-end/data/metaextract_',variable,'_',tolower(experiment),'.rda',sep=''))
  meta <- as.data.frame(meta)
  META <- meta[,c('source','experiment','institute_id','model_id','parent_experiment_rip','realization','longname','variable','units','timeunit',
                  'resolution','longitude','latitude',"experiment_id",'dim1','index','calendar','creation_date',
                  'tracking_id','physics_version','forcing','reference','contact','comment','table_id','file')]
  
  gcm.meta <- subset(META, subset = (source == 'CMIP5') & (variable == variable) & (experiment == experiment))
  gcm.meta <- merge.meta.ipcc(meta = gcm.meta,ipcc = IPCC.AR5.Table.9.A.1)
}

gcm.tas <- gcm.pr <- gcm.all <- NULL
gcm.tas$rcp45 <- gcm.meta('tas','RCP45')
gcm.tas$rcp85 <- gcm.meta('tas','RCP85')
gcm.pr$rcp45 <- gcm.meta('pr','RCP45')
gcm.pr$rcp85 <- gcm.meta('pr','RCP85')

gcm.com.var <- function(gcm.tas,gcm.pr) {
  
  # Common meta data for all variables
  modelrip.tas <- paste(gcm.tas$institute_id,gcm.tas$model_id,gcm.tas$parent_experiment_rip,gcm.tas$realization)
  modelrip.pr <- paste(gcm.pr$institute_id,gcm.pr$model_id,gcm.pr$parent_experiment_rip,gcm.pr$realization)
  id.com <- intersect(modelrip.pr,modelrip.tas)
  id <- NULL
  id$tas <- which(is.element(modelrip.tas[!duplicated(modelrip.tas)],id.com))
  # Some of the attributes are duplicated for precipitation 
  id$pr <- which(is.element(modelrip.pr[!duplicated(modelrip.pr)],id.com)) 
  invisible(id)
} 
com.gcm.var <- NULL
com.gcm.var$rcp45 <- gcm.com.var(gcm.tas=gcm.tas$rcp45,gcm.pr=gcm.pr$rcp45)
com.gcm.var$rcp85 <- gcm.com.var(gcm.tas=gcm.tas$rcp85,gcm.pr=gcm.pr$rcp85)

# Extract common simulations between variables
gcm.all$rcp45 <- gcm.tas$rcp45[com.gcm.var$rcp45$tas,-c(7,8,9,10)]
gcm.all$rcp85 <- gcm.tas$rcp85[com.gcm.var$rcp85$tas,-c(7,8,9,10)]

# ---- RCMs -------
data(package='DECM', 'metaextract', envir=environment())
meta <- as.data.frame(meta)
META <- meta[,c('project_id','experiment','gcm','gcm_rip','rcm',
                'longname','var','unit','frequency','dates',
                'resolution','lon','lon_unit','lat','lat_unit', 
                'frequency','creation_date','url')]

rcm.tas <- rcm.pr <- rcm.all <- NULL
rcm.tas$rcp45 <- subset(META, subset = (project_id == 'CORDEX') & (var == 'tas') & (experiment=="RCP4.5"))
rcm.pr$rcp45 <- subset(META, subset = (project_id == 'CORDEX') & (var == 'pr') & (experiment=="RCP4.5"))
#rcm.meta.all <- rcm.meta.pr[,-c(6:9,16)]
i.var <- which(grepl("longname|var|unit|frequency",colnames(rcm.pr$rcp45)) & 
               !grepl("lon_unit|lat_unit",colnames(rcm.pr$rcp45)))
rcm.all$rcp45 <- rcm.pr$rcp45[,-i.var]

rcm.tas$rcp85 <- subset(META, subset = (project_id == 'CORDEX') & (var == 'tas') & (experiment=="RCP8.5"))
rcm.pr$rcp85 <- subset(META, subset = (project_id == 'CORDEX') & (var == 'pr') & (experiment=="RCP8.5"))
#rcm.meta.all <- rcm.meta.pr[,-c(6:9,16)]
i.var <- which(grepl("longname|var|unit|frequency",colnames(rcm.pr$rcp85)) & 
                 !grepl("lon_unit|lat_unit",colnames(rcm.pr$rcp85)))
rcm.all$rcp85 <- rcm.pr$rcp85[,-i.var]

# RCM statistics ...
# CORDEX RCP45
rcms <- NULL
load("../../back-end/data/statistics.cordex.eobs.tas.1981-2010.rcp45.rda")
store$eobs.tas$corr <- store$eobs.tas$mean
store$eobs.tas$corr <- rep(1,13)
rcms$rcp45$tas$present <- store
load("../../back-end/data/statistics.cordex.tas.2021-2050.rcp45.rda")
rcms$rcp45$tas$nf <- store
load("../../back-end/data/statistics.cordex.tas.2071-2100.rcp45.rda")
rcms$rcp45$tas$ff <- store
load("../../back-end/data/statistics.cordex.eobs.pr.1981-2010.rcp45.rda")
store$eobs.pr$corr <- rep(1,13)
rcms$rcp45$pr$present <- store
load("../../back-end/data/statistics.cordex.pr.2021-2050.rcp45.rda")
rcms$rcp45$pr$nf <- store
load("../../back-end/data/statistics.cordex.pr.2071-2100.rcp45.rda")
rcms$rcp45$pr$ff <- store

## CORDEX RCP85
load("../../back-end/data/statistics.cordex.eobs.tas.1981-2010.rcp85.rda")
store$eobs.tas$corr <- store$eobs.tas$mean
store$eobs.tas$corr <- rep(1,13)
rcms$rcp85$tas$present <- store
load("../../back-end/data/statistics.cordex.tas.2021-2050.rcp85.rda")
rcms$rcp85$tas$nf <- store
load("../../back-end/data/statistics.cordex.tas.2071-2100.rcp85.rda")
rcms$rcp85$tas$ff <- store
load("../../back-end/data/statistics.cordex.eobs.pr.1981-2010.rcp85.rda")
store$eobs.pr$corr <- rep(1,13)
rcms$rcp85$pr$present <- store
load("../../back-end/data/statistics.cordex.pr.2021-2050.rcp85.rda")
rcms$rcp85$pr$nf <- store
load("../../back-end/data/statistics.cordex.pr.2071-2100.rcp85.rda")
rcms$rcp85$pr$ff <- store


regions.all <- list('Europe',
                    'National Regions' = c(
                      'ALB - Albania',                                
                      'BIH - Bosnia and Herzegovina',                  
                      'BGR - Bulgaria',                                
                      'DNK - Denmark',                                 
                      'IRL - Ireland',                                 
                      'EST - Estonia',                                 
                      'AUT - Austria',                                 
                      'CZE - Czech Republic',                          
                      'FIN - Finland',                                 
                      'FRA - France',                                  
                      'DEU - Germany',                                 
                      'GRC - Greece',                                  
                      'HRV - Croatia',                                 
                      'HUN - Hungary',                                 
                      'ISL - Iceland',                                 
                      'ITA - Italy',                                   
                      'LVA - Latvia',                                  
                      'BLR -  Belarus',                                 
                      'LTU -  Lithuania',                               
                      'SVK - Slovakia',                                
                      'MKD - The former Yugoslav Republic of Macedonia',
                      'NLD - Netherlands',                             
                      'NOR - Norway',                                  
                      'POL - Poland',                                  
                      'PRT - Portugal',                                
                      'ROU - Romania',                                 
                      'MDA - Republic of Moldova',                     
                      'RUS - Russia',                                  
                      'ESP - Spain',                                   
                      'SWE - Sweden',                                  
                      'CHE - Switzerland',                             
                      'GBR - United Kingdom',
                      'UKR - Ukraine'),
                      'PRUDENCE Regions' = c('BI - British Isles','IP - Iberian Peninsula','FR - FRANCE','ME - Mid Europe','SC - Scandinavia','AL - ALPS','MD - Mediterranean','EA - Eastern Europe')
)    

afg <- readOGR(dsn = '../../back-end/inst/extdata/TM_WORLD_BORDERS-0.3/', 
               layer = "TM_WORLD_BORDERS-0.3",
               verbose = FALSE, 
               stringsAsFactors = FALSE)

## Standardized Precipitation Index
rcm.name <- function(i,rcp) {
  rcmi <- rcm.pr[[rcp]][i,c('gcm','gcm_rip','rcm')]
  rcmi[is.na(rcmi)] <- ''
  return(paste(as.character(as.matrix(rcmi)),collapse = '_'))
}
rcm.names <- NULL
rcm.names$rcp45 <- sapply(1:dim(rcm.pr[['rcp45']])[1],rcm.name,'rcp45')
rcm.names$rcp85 <- sapply(1:dim(rcm.pr[['rcp85']])[1],rcm.name,'rcp85')

rcm.tas$rcp85 <- subset(rcm.tas$rcp85,subset =  is.element(rcm.names$rcp85,rcm.names$rcp45))
rcm.pr$rcp85 <- subset(rcm.pr$rcp85,subset =  is.element(rcm.names$rcp85,rcm.names$rcp45))
rcm.all$rcp85 <-  subset(rcm.all$rcp85,subset =  is.element(rcm.names$rcp85,rcm.names$rcp45))