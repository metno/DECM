## Define a server for the Shiny app
source("global.R")

shinyServer(function(input, output) {
  
  output$seasoncycle <- renderPlot({
    var <- switch(tolower(as.character(input$variable)),
                  "temperature"='tas',
                  "precipitation"='pr')
    period <- switch(tolower(as.character(input$period)),
                     "present day (1981-2010)"='present',
                     "far future (2071-2100)"='ff',
                     "near future (2021-2050)"='nf')
    gcms <- names(stats[[var]]$ff)[switch(var,"tas"=im.tas,"pr"=im.pr)]
    
    if(tolower(input$region)=="global") {
      region <- "global"
    } else {
      i.srex <- which(srex$name==input$region)
      region <- srex$label[i.srex]
    }

    x <- lapply(gcms, function(gcm) {
      stats[[var]][[period]][[gcm]][[region]][["mean"]][2:13] })
    if(var=="pr") x <- lapply(x, function(y) y*60*60*24) ## mm/s to mm/day

    if(period=="present") {
      im.ref <- which(!grepl("gcm",names(stats[[var]]$present)))
      ref <- stats[[var]][[period]][[im.ref]][[region]][["mean"]][2:13]
      if(var=="pr") ref <- ref*1E3 ## m/day to mm/day
    } else {
      ref <- NULL
    }
    
    ylim <- c(NULL,NULL)
    if(!is.na(input$y0)) {
      ylim[1] <- input$y0
    } else {
      ylim[1] <- min(c(unlist(x),ref)) - 0.45*diff(range(c(unlist(x),ref)))
    }
    if(!is.na(input$y1)) {
      ylim[2] <- input$y1
    } else {
      ylim[2] <- max(c(unlist(x),ref)) + 0.15*diff(range(c(unlist(x),ref)))
    }
    
    im <- as.numeric(gsub(":.*","",input$gcms))
    par(xpd = T, mar = par()$mar + c(4,0,0,0))
    plot(1:12, x[[1]], col = "white", xlim = c(0.5,12.5), ylim = ylim, 
         xaxt = "n", xlab = "",
         ylab=paste(input$variable," (",switch(var,"tas"="deg C","pr"="mm/day"),")",sep=""))
    axis(1, at=1:12, labels=FALSE)
    text(1:12-0.1, par("usr")[3] - 0.05*diff(ylim), labels = names(x[[1]]), srt = 45, pos = 1, xpd = TRUE)
    
    for(i in 1:length(x)) lines(1:12,x[[i]],col="grey80")
    for(i in im) lines(1:12,x[[i]],col="blue")
    
    if(!is.null(ref)) {
      lines(1:12,ref,col="red",lty=2)
      legend(0,par("usr")[3] - 0.25*diff(ylim),
             legend=c("All GCMs","Selected GCMs","Reference data (ERA-interim)"),
             lty=c(1,1,2),col=c("grey80","blue","red"),box.lwd=0.5,cex=0.85)
    } else {
      legend(0,par("usr")[3] - 0.25*diff(ylim),#"top left",
             legend=c("GCMs","Selected GCMs"),
             lty=c(1,1),col=c("grey80","blue"),cex=0.85)
    }
  }, width=450, height=450*0.9)
  
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
  }, width=200,height=200*0.6)
  
})