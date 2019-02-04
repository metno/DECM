source("global.R")

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
    gcms.tas <- names(stats$tas$ff)[im.tas]
    gcms.pr <- names(stats$pr$ff)[im.pr]
    if(tolower(input$region)=="global") {
      region <- "global"
    } else {
      i.srex <- which(srex$name==input$region)
      region <- srex$label[i.srex]
    }
    dtas <- sapply(gcms.tas, function(gcm) mean(sapply(season, function(s)
      stats$tas[[period]][[gcm]][[region]][["mean"]][[s]])) - 
        mean(sapply(season, function(s) 
          stats$tas$present[[gcm]][[region]][["mean"]][[s]])))
    dpr <- sapply(gcms.pr, function(gcm) mean(sapply(season, function(s)
      stats$pr[[period]][[gcm]][[region]][["mean"]][[s]])) - 
        mean(sapply(season, function(s) 
          stats$pr$present[[gcm]][[region]][["mean"]][[s]])))
    im <- as.numeric(gsub(":.*","",input$gcms))
    scatterplot(dtas,dpr*(60*60*24),ix=NULL,xlim=input$tlim,ylim=input$plim,
                xlab="Temperature change (deg C)",ylab="Precipitation change (mm/day)",
                main=paste("Climate change assuming RCP4.5\npresent day (1981-2010) to",input$period),
                show.legend=FALSE,im=im,
                legend=seq(length(dtas)),pal=NULL,#pal="cat",pch=21,
                pch=".",#pch=as.character(seq(length(dtas))),
                cex=0,lwd=1.5,new=FALSE)
    text(dtas,dpr*(60*60*24),as.character(seq(length(dtas))),col=adjustcolor("blue",alpha.f=0.1),cex=1.5)
    text(dtas[im],dpr[im]*(60*60*24),as.character(im),col=adjustcolor("blue",alpha.f=0.8),cex=1.5)
  }, width=450, height=450)
  
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
    #lines(attr(geoborders,'borders')$x,attr(geoborders,'borders')$y,col="grey30")
    par(xaxt="s",yaxt="s",las=1,col.axis='grey',col.lab='grey20',
        cex.lab=0.7,cex.axis=0.7)
    axis(3,at=pretty(par("xaxp")[1:2],n=5),col='grey50')
    axis(2,at=pretty(par("yaxp")[1:2],n=5),col='grey50')
    grid()
    lines(region$lon,region$lat,col="blue",lwd=1.5,lty=1)
  }, width=200,height=200*0.6)#width=250, height=175)
  
})
