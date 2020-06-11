## helpers.R
## Help functions for the shiny app "modelexplorer"
library(shiny)
library(DT)
library(DECM)

## Preparations
## source scripts
data("statistics.cmip.tas.2071-2100", package="DECM")
gcmnames <- paste(seq(length(store)),names(store),sep=". ")

## load metadata
data("metaextract")
M <- data.frame(list(project_id=meta$project_id, experiment_id=meta$experiment_id, gcm=meta$gcm,
                     rip=meta$gcm_rip, rcm=meta$rcm, var=meta$var, unit=meta$unit, 
                     resolution=paste(meta$resolution,"deg"),
                     domain=paste(gsub(","," - ",meta$lon),"E"," / ",paste(gsub(","," - ",meta$lat)),"N",sep=""), 
                     years=gsub(",","-",gsub("-[0-9]{2}","",meta$dates)), url=meta$url))

## load and expand commonEOFs
#ceof.all <- NULL
#data("ceof.gcm.tas.annual", package="DECM")
#ceof.all$tas$CMIP5 <- ceof
#data("ceof.gcm.pr.annual", package="DECM")
#ceof.all$pr$CMIP5 <- ceof
#data("ceof.rcm.tas.annual", package="DECM")
#ceof.all$tas$CORDEX <- ceof
#data("ceof.rcm.pr.annual", package="DECM")
#ceof.all$pr$CORDEX <- ceof
#ceof <- ceof.all
#rm("ceof.all"); gc(reset=TRUE)
#selectrowindex <- 1

#select.ceof <- function(table_rows_selected=1,varid="Temperature") {
#  if (length(table_rows_selected)>0) {
#    selectedrowindex <- table_rows_selected[length(table_rows_selected)]
#    selectedrowindex <- as.numeric(selectedrowindex)
#  } else {
#    selectedrowindex <- 1
#  }
#  selectedrow <- (M[selectedrowindex,])
#  ceof.sel <- NULL
#  if (grepl("temp",tolower(varid))) {
#    ceof.sel <- ceof$tas
#  } else {
#    ceof.sel <- ceof$pr
#  }
#  ceof.sel <- ceof.sel[[selectedrow$project_id]]
#  im <- which(attr(ceof,"model_id")$rcm==selectedrow$rcm & 
#              attr(ceof,"model_id")$gcm==selectedrow$gcm & 
#              attr(ceof,"model_id")$gcm_rip==selectedrow$rip)
#  attr(ceof.sel,"im") <- im
#  return(ceof.sel)
#}

#map.changes <- function(ceof,period="ff",FUN="mean",new=FALSE) {
#  if(new) dev.new()
#  par(fig=c(0,0.5,0,0.33), new=FALSE)  
#  map.ensemble(ceof,type="mvpd",new=FALSE,FUN=FUN)
#  par(fig=c(0.5,1,0,0.33), new=FALSE)  
#  map.ensemble(ceof,type=paste("mv",period,sep=""),new=FALSE,FUN=FUN)
#  par(fig=c(0,1,0.33,1), new=FALSE)  
#  map.ensemble(ceof,type=paste("cc",period,sep=""),new=FALSE,FUN=FUN)
#}
#
#fn.switch <- function(fn="mean") {
#  if(!is.null(fn)) {
#    switch(fn,"Mean"="mean",
#           "95th percentile"="q95",
#           "5th percentile"="q5")
#  } else {
#    "mean"
#  }
#}
#
#type.switch <- function(type="Climate change far future") {
#  if(!is.null(type)) {
#    switch(type,"Climate change near future"="ccnf",
#           "Climate change far future"="ccff",
#           "Mean value present day"="mvpd",
#           "Mean value near future"="mvnf",
#  } else {
#    "ccff"
#  }
#}