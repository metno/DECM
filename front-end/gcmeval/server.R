
library(shiny)
library(DECM)
source("helpers.R")

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

## Load metadata
data("metaextract")
im <- meta$project_id=="CMIP5" & meta$var=="tas"
gcmnames <- paste(seq(sum(im)),": ",meta$gcm[im],".",meta$gcm_rip[im],sep="")

## Function 'regions' is defined in helpers.R
srex <- regions("srex")

## Load geographical data for map
data("geoborders",envir=environment())
  
## Define a server for the Shiny app
shinyServer(function(input, output) {
  
  output$dtdpr <- renderPlot({
    season <- switch(tolower(as.character(input$season)),
                     'annual mean'='ann',
                     'winter'=c('dec','jan','feb'),
                     'spring'=c('mar','apr','may'),
                     'summer'=c('jun','jul','aug'),
                     'autumn'=c('sep','oct','nov'))
                     #'annual mean'='ann','winter'='djf','spring'='mam',
                     #'summer'='jja','autumn'='son')
    period <- switch(tolower(as.character(input$period)),
                     "far future (2071-2100)"='ff',
                     "near future (2021-2050)"='nf')
    gcms <- names(stats$tas$ff)
    if(tolower(input$region)=="global") {
      region <- "global"
    } else {
      i.srex <- which(srex$name==input$region)
      region <- srex$label[i.srex]
    }
    dtas <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
                     stats$tas[[period]][[gcm]][[region]][["mean"]][[s]])) - 
                     mean(sapply(season, function(s) 
                     stats$tas$present[[gcm]][[region]][["mean"]][[s]])))
    dpr <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
                     stats$pr[[period]][[gcm]][[region]][["mean"]][[s]])) - 
                     mean(sapply(season, function(s) 
                     stats$pr$present[[gcm]][[region]][["mean"]][[s]])))
    im <- as.numeric(gsub(":.*","",input$gcms))
    scatterplot(dtas,dpr*(60*60*24),ix=NULL,xlim=input$tlim,ylim=input$plim,
                xlab="Temperature change (deg C)",ylab="Precipitation change (mm/day)",
                main=paste("Climate change present day (1981-2010) to",tolower(input$period)),
                show.legend=FALSE,im=im,legend=seq(length(dtas)),pal=NULL,#"rainbow",
                pch=seq(length(dtas)),cex=1.4,lwd=1.5,new=FALSE)
  }, width=500, height=500)

  output$Tcycle <- renderPlot({
    var <- 'tas'
    #if(input$ref) {
      period <- "present"
    #} else {
    #  period <- switch(tolower(as.character(input$period)),
    #                   "far future (2071-2100)"='ff',
    #                   "near future (2021-2050)"='nf')
    #}
    gcms <- names(stats$tas$ff)
    ref <- NULL
    if(tolower(input$region)=="global") {
      region <- "global"
    } else {
      i.srex <- which(srex$name==input$region)
      region <- srex$label[i.srex]
    }
    x <- lapply(gcms, function(gcm) stats[[var]][[period]][[gcm]][[region]][["mean"]][2:13])
    if(period=="present") {
      ref <- names(stats$tas$present)[!grepl("gcm",names(stats$tas$present))]
      ref <- stats[[var]][[period]][[ref]][[region]][["mean"]][2:13]
    }
    ylim <- c(NULL,NULL)
    y0 <- NA # input$y0
    y1 <- NA # input$y1
    if(!is.na(y0)) {
      ylim[1] <- y0
    } else {
      ylim[1] <- min(c(unlist(x),ref)) - 0.45*diff(range(c(unlist(x),ref)))
    }
    if(!is.na(y1)) {
      ylim[2] <- y1
    } else {
      ylim[2] <- max(c(unlist(x),ref)) + 0.15*diff(range(c(unlist(x),ref)))
    }
    
    im <- as.numeric(gsub(":.*","",input$gcms))
    par(xpd = T, mar = par()$mar + c(4,0,0,0))
    plot(1:12, x[[1]], col = "white", xlim = c(0.5,12.5), ylim = ylim,
         xaxt = "n", xlab = "",ylab="temperature (deg C)",
         main="")
    axis(1, at=1:12, labels=FALSE)#names(x[[1]]))
    text(1:12-0.1, par("usr")[3] - 0.05*diff(ylim), labels = names(x[[1]]), srt = 45, pos = 1, xpd = TRUE)
    
    col <- rep("blue",length(x)) #rainbow(length(x))
    col.pale <- rep("Grey80",length(x)) #adjustcolor(col,alpha=0.1)
    col.ref <- "red" #"black"
    lapply(1:length(x),function(i) lines(1:12,x[[i]],lwd=1.5,col=col.pale[i]))
    lapply(im,function(i) lines(1:12,x[[i]],lwd=2,col=col[i]))
    if(!is.null(ref)) {
      lines(1:12,ref,lty=2,lwd=2,col=col.ref)
      legend(0,par("usr")[3] - 0.25*diff(ylim),#"bottomleft",
             legend=c("Reference data (ERA-interim)","GCMs","Selected GCMs"),
             lty=c(2,1,1),box.lwd=0.5,cex=0.85,col=c(col.ref,col.pale[1],col[1]))
    } else {
      legend(0,par("usr")[3] - 0.25*diff(ylim),#"top left",
             legend=c("All GCMs","Selected GCMs"),
             lty=c(1,1),cex=0.85,col=c(col.pale[1],col[1]))
    }
    if(period=="present") {
      text(1,min(ylim)+diff(range(ylim))*0.1,"present day (1981-2010)",pos=4)
    } else {
      text(1,min(ylim)+diff(range(ylim))*0.1,input$period,pos=4)
    }
  }, width=410, height=400)

  output$Pcycle <- renderPlot({
    var <- 'pr'
    period <- 'present'
    gcms <- names(stats$pr$ff)
    ref <- NULL
    if(tolower(input$region)=="global") {
      #x <- lapply(gcms, function(gcm) stats[[var]][[period]][[gcm]][["mean"]][2:13])
      #if(period=="present") ref <- stats[[var]][[period]][[1]][["mean"]][2:13]
      region <- "global"
    } else {
      i.srex <- which(srex$name==input$region)
      region <- srex$label[i.srex]
      #x <- lapply(gcms, function(gcm) stats[[var]][[period]][[gcm]][[region]][["mean"]][2:13])
      #if(period=="present") ref <- stats[[var]][[period]][[1]][[region]][["mean"]][2:13]
    }
    x <- lapply(gcms, function(gcm) stats[[var]][[period]][[gcm]][[region]][["mean"]][2:13])
    if(period=="present") {
      ref <- names(stats$pr$present)[!grepl("gcm",names(stats$pr$present))]
      ref <- stats[[var]][[period]][[ref]][[region]][["mean"]][2:13]
    }
    x <- lapply(x, function(y) y*60*60*24) ## mm/s to mm/day
    ref <- ref*1E3 ## m/day to mm/day
    ylim <- c(NULL,NULL)
    y0 <- NA # input$y0
    y1 <- NA # input$y1
    if(!is.na(y0)) {
      ylim[1] <- y0
    } else {
      ylim[1] <- min(c(unlist(x),ref)) - 0.45*diff(range(c(unlist(x),ref)))
    }
    if(!is.na(y1)) {
      ylim[2] <- y1
    } else {
      ylim[2] <- max(c(unlist(x),ref)) + 0.15*diff(range(c(unlist(x),ref)))
    }
    
    im <- as.numeric(gsub(":.*","",input$gcms))
    par(xpd = T, mar = par()$mar + c(4,0,0,0))
    plot(1:12, x[[1]], col = "white", xlim = c(0.5,12.5), ylim = ylim, 
         xaxt = "n", xlab = "", ylab="Precipitation (mm/day)")
    axis(1, at=1:12, labels=FALSE)#names(x[[1]]))
    text(1:12-0.1, par("usr")[3] - 0.05*diff(ylim), labels = names(x[[1]]), srt = 45, pos = 1, xpd = TRUE)
    
    col <- rep("blue",length(x)) #rainbow(length(x))
    col.pale <- rep("Grey80",length(x)) #adjustcolor(col,alpha=0.1)
    col.ref <- "red" #"black"
    lapply(1:length(x),function(i) lines(1:12,x[[i]],lwd=1.5,col=col.pale[i]))
    lapply(im,function(i) lines(1:12,x[[i]],lwd=2,col=col[i]))
    if(!is.null(ref)) {
      lines(1:12,ref,lty=2,lwd=2,col=col.ref)
      legend(0,par("usr")[3] - 0.25*diff(ylim),#"bottomleft",
             legend=c("Reference data (ERA-interim)","GCMs","Selected GCMs"),
             lty=c(2,1,1),box.lwd=0.5,cex=0.85,col=c(col.ref,col.pale[1],col[1]))
    } else {
      legend(0,par("usr")[3] - 0.25*diff(ylim),#"top left",
             legend=c("GCMs","Selected GCMs"),
             lty=c(1,1),cex=0.85,col=c(col.pale[1],col[1]))
    }
    text(1,min(ylim)+diff(range(ylim))*0.1,"present day (1981-2010)",pos=4)
  }, width=410, height=400)  

  output$table <- DT::renderDataTable({
    if(tolower(input$region)=="global") {
      region <- "global"
    } else {
      i.srex <- which(srex$name==input$region)
      region <- srex$label[i.srex]
    }
    gcms <- names(stats$tas$ff)
    it <- switch(tolower(input$season),"annual mean"=1,"winter"=c(13,2,3),
                 "spring"=c(4,5,6),"summer"=c(7,8,9),"autumn"=c(10,11,12))
    if("Temperature" %in% input$varid) {
      mean.gcm <- lapply(gcms, function(gcm) stats$tas$present[[gcm]][[region]]$mean)
      sd.gcm <- lapply(gcms, function(gcm) stats$tas$present[[gcm]][[region]]$spatial.sd)
      ref <- names(stats$tas$present)[!grepl("gcm",names(stats$tas$present))]
      mean.ref <- stats$tas$present[[ref]][[region]]$mean
      sd.ref <- stats$tas$present[[ref]][[region]]$spatial.sd
      bias <- unlist(lapply(mean.gcm, function(x) mean(x[it]-mean.ref[it])))
      sdratio <- unlist(lapply(sd.gcm, function(x) mean(x[it]/sd.ref[it])))
      rmse.cycle <- unlist(lapply(mean.gcm, function(x) sqrt( sum(((x[2:13]-x[1]) - (mean.ref[2:13]-mean.ref[1]) )^2)/12)))
      corr <- unlist(lapply(gcms, function(gcm) mean(stats$tas$present[[gcm]][[region]]$corr[it])))
      e <- unlist(lapply(gcms, function(gcm) stats$tas$present[[gcm]][[region]]$e))
    } 
    if("Precipitation" %in% input$varid) {
      mean.gcm <- lapply(gcms, function(gcm) stats$pr$present[[gcm]][[region]]$mean*60*60*24)
      sd.gcm <- lapply(gcms, function(gcm) stats$pr$present[[gcm]][[region]]$spatial.sd*60*60*24)
      ref <- names(stats$pr$present)[!grepl("gcm",names(stats$pr$present))]
      mean.ref <- stats$pr$present[[ref]][[region]]$mean*1E3
      sd.ref <- stats$pr$present[[ref]][[region]]$spatial.sd*1E3
      pr.bias <- unlist(lapply(mean.gcm, function(x) mean(x[it]-mean.ref[it])))
      pr.sdratio <- unlist(lapply(sd.gcm, function(x) mean(x[it]/sd.ref[it])))
      pr.rmse.cycle <- unlist(lapply(mean.gcm, function(x) sqrt( sum(((x[2:13]-x[1]) - 
                                                                        (mean.ref[2:13]-mean.ref[1]) )^2)/12)))
      pr.corr <- unlist(lapply(gcms, function(gcm) mean(stats$pr$present[[gcm]][[region]]$corr[it])))
      pr.e <- unlist(lapply(gcms, function(gcm) stats$pr$present[[gcm]][[region]]$e))
      if(!("Temperature" %in% input$varid)) {
        bias <- pr.bias
        sdratio <- pr.sdratio
        rmse.cycle <- pr.rmse.cycle
        corr <- pr.corr
        e <- pr.e
      } else {
        bias <- (bias + pr.bias)/2
        sdratio <- (sdratio + pr.sdratio)/2
        rmse.cycle <- (rmse.cycle + pr.rmse.cycle)/2
        corr <- (corr + pr.corr)/2
        e <- (e + pr.e)/2
      }
    }
    M <- data.frame(list(gcm=meta$gcm[im], rip=meta$gcm_rip[im], 
                         "bias"=round(bias,digits=2), "sd.ratio"=round(sdratio,digits=2), 
                         "spatial.corr"=round(corr,digits=2), "CMPI"=round(e,digits=2)))
    datatable(M,filter='top',selection='single',options=list(),style="bootstrap",
              colnames=gsub("[.|_]"," ",names(M)))
  })
  
  output$map <- renderPlot({
    if(tolower(input$region)=="global") {
      region <- list(lon=c(-180,-180,180,180,-180),lat=c(-90,90,90,-90,-90))
    } else {
      i.srex <- which(srex$name==input$region)
      region <- list(lon=srex$coord[[i.srex]][1,],
                     lat=srex$coord[[i.srex]][2,])
    }
    par(mgp=c(1,0.5,0),mar=c(0.2,0.2,0.2,0.2))
    plot(geoborders$x,geoborders$y,col="grey30",type="l",lwd=0.5,
         xlim=c(-180,180),ylim=c(-90,90),
         xlab="Longitude",ylab="Latitude",xaxt="n",yaxt="n")
    par(xaxt="s",yaxt="s",las=1,col.axis='grey',col.lab='grey20',
        cex.lab=0.7,cex.axis=0.7)
    axis(3,at=pretty(par("xaxp")[1:2],n=5),col='grey50')
    axis(2,at=pretty(par("yaxp")[1:2],n=5),col='grey50')
    grid()
    lines(region$lon,region$lat,col="blue",lwd=1.5,lty=1)
  }, width=200,height=200*0.6)#width=250, height=175)
  
})
