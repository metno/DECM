library(shiny)
library(DECM)
library(fields)
source("global.R")


## Define a server for the Shiny app
shinyServer(function(input, output, session) {
  
  value <- reactiveVal(0)
  
  Regionlist <- reactive({
    rl <- list(input$regionwm1,input$regionwm2)
    rl[which(rl != "---")]
  })
  
  im <- reactive({as.numeric(gsub(":.*","",input$gcms))})

  ## Weighted rank calculations
  tasRanks <- reactive({ranking.all(varid="tas",Regions=Regionlist())})
  prRanks <- reactive({ranking.all(varid="pr",Regions=Regionlist())})

  seas_varweightedranks <- reactive({as.numeric(input$wmdt)*tasRanks()+as.numeric(input$wmdp)*prRanks()})
  seasweightvec <- reactive({as.numeric(c(input$wmann,input$wmdjf,input$wmmam,input$wmjja,input$wmson))})
  regweightvec <- reactive({
    rw <- as.numeric(c(input$wmreg1,input$wmreg2))
    rw[which(list(input$regionwm1,input$regionwm2) != "---")] 
  })
  
  weightedranks_all <- reactive({
    W <- array(NA,c(length(gcmst),4))
    for (i in 1:length(gcmst)) {
      for (j in 1:4) {
        W[i,j] <- (seasweightvec() %*% seas_varweightedranks()[i,,j,] %*% regweightvec())
      }
    }
    invisible(W)
  })
  
  metweightvec <- reactive({as.numeric(c(input$wmbias,input$wmsd,input$wmsc,input$wmcmpi))})
  weightedrank_all <- reactive({rank(weightedranks_all() %*% metweightvec())})
  
  best <- reactive({order(weightedrank_all())[1:input$ngcm]})

  ## Weighted spread calculations
  dtasSpread <- reactive({spread.all(varid="tas",Regions=Regionlist(),im=NULL)})
  dtasSelSpread <- reactive({spread.all(varid="tas",Regions=Regionlist(),im=im())})
  dprSpread <- reactive({spread.all(varid="pr",Regions=Regionlist(),im=NULL)})
  dprSelSpread <- reactive({spread.all(varid="pr",Regions=Regionlist(),im=im())})
  
  dtasRelSpread <- reactive({dtasSelSpread()/dtasSpread()})
  dprRelSpread <- reactive({dprSelSpread()/dprSpread()})
  seas_varweightedspread <- reactive({(as.numeric(input$wmdt)*dtasRelSpread() + 
                                         as.numeric(input$wmdp)*dprRelSpread())/
                                      (as.numeric(input$wmdt)+as.numeric(input$wmdp))})
  
  weightedspread_nf <- reactive({(seasweightvec() %*% seas_varweightedspread()[,1,] %*% regweightvec())/
      sum(seasweightvec())/sum(regweightvec()) })
  weightedspread_ff <- reactive({(seasweightvec() %*% seas_varweightedspread()[,2,] %*% regweightvec())/
      sum(seasweightvec())/sum(regweightvec()) })

  output$MetricText  <- renderText({
    
    weightedrank <- reactive({weightedrank_all()[im()]})
    mean_weightedrank <- reactive({mean(weightedrank(),na.rm=TRUE)})
    
    #output
    legcolsrank <- two.colors(n=107,start="green",end="red",middle = "orange") #colors for ranks
    meanRelMetricsIndx <- reactive({floor(mean_weightedrank())}) #color index based on weighted rank
    
    legcols <- two.colors(n=11,start="red",end="green",middle = "orange") #colors for percentage number
    meanRelSpreadIndx_nf <- reactive({floor(weightedspread_nf()*10)+1}) #color index based on weighted mean rel. spread for near future
    meanRelSpreadIndx_ff <- reactive({floor(weightedspread_ff()*10)+1}) #color index based on weighted mean rel. spread for far future
      
    paste("The best performing ",input$ngcm," models would be: ",paste(best(),collapse="/"),". <br> <br>",
          "You have selected the following models: ",paste(im(),collapse="/"),". <br>",
          "The ranks of the individual models are: ",paste(weightedrank(),collapse="/"),". <br>",
          "The mean weighted model performance rank of the selected models is  <font size = +1, font color=\"",
                legcolsrank[meanRelMetricsIndx()],"\"><b> Rank ", round(mean_weightedrank()), "  of  ",length(gcmst),"</b></font>.",
          "<br><br> For the <b>near future</b>, the weighted mean spread of the selected models is ","<font size = +1, font color=\"",
                legcols[meanRelSpreadIndx_ff()],"\"><b>", round(weightedspread_nf()*100), "%</b>
                </font> of the spread of the whole ensemble. ",
          " For the <b>far future</b>, the weighted mean spread of the selected models is ","<font size = +1, font color=\"",
          legcols[meanRelSpreadIndx_ff()],"\"><b>", round(weightedspread_ff()*100), "%</b>
                </font> of the spread of the whole ensemble. ",
          "<br><br> Use the scatterplot tool below to investigate the projected changes within the whole ensemble for a specific region,",
          " season and time-line. How well does your selection represent the range of possible climate change outcomes?",
          " Add more models at the edges of the scatterplot to increase the relative spread among your selected climate models.",sep="")
    
  })
  
  observe({
    if(input$ngcm!=length(input$gcms)) {
      sel <- "My selection"
    } else {
      sel <- input$gcmselect
    }
    updateSelectInput(session, inputId = "gcmselect", selected=sel, choices = c("Random","My selection","Best performance")) 
  })
  
  observe({
    nm <- length(input$gcms)
    updateNumericInput(session, inputId = "ngcm", value=nm, min=1, max=length(gcmst)) 
  })
  
  observe({
    if (tolower(input$gcmselect)=="best performance") {
      im <- best()
    } else if(tolower(input$gcmselect)=="random") {
      im <- sample(1:length(gcmnames),input$ngcm,replace=FALSE)
    } else {
      im <- as.numeric(gsub(":.*","",input$gcms))
    }
    updateCheckboxGroupInput(session, inputId = "gcms", choices = gcmnames, selected = gcmnames[im]) 
  })
  
  ##map
  output$mapm1 <- renderPlot({
    if(tolower(input$regionwm1)=="global") {
      region <- list(lon=c(-180,-180,180,180,-180),lat=c(-90,90,90,-90,-90))
    } else {
      i.srex <- which(srex$name==input$regionwm1)
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
  
  ##map
  output$mapm2 <- renderPlot({
    if(input$regionwm2 != "---"){
      if(tolower(input$regionwm2)=="global") {
        region <- list(lon=c(-180,-180,180,180,-180),lat=c(-90,90,90,-90,-90))
      } else {
        i.srex <- which(srex$name==input$regionwm2)
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
    }}, width=200,height=200*0.6)#width=250, height=175)
  
  ##map
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
  
  ##Scatterplot of temperature and precip. change 
  output$dtdpr <- renderPlot({
    season <- reactive({switch(tolower(as.character(input$season)),
                     'annual mean'='ann',
                     'winter'=c('dec','jan','feb'),
                     'spring'=c('mar','apr','may'),
                     'summer'=c('jun','jul','aug'),
                     'autumn'=c('sep','oct','nov'))})
    period <- reactive({switch(tolower(as.character(input$period)),
                     "far future (2071-2100)"='ff',
                     "near future (2021-2050)"='nf')})
    
    region <- reactive({
      if(tolower(input$region)=="global") {
        region <- "global"
      } else {
        i.srex <- which(srex$name==input$region)
        region <- srex$label[i.srex]
      }
      return(region)
    })
    
    #Temperature and precip. spread
    dtas <- reactive({sapply(gcmst, function(gcm) mean(sapply(season(), function(s)
      stats$tas[[period()]][[gcm]][[region()]][["mean"]][[s]])) - 
        mean(sapply(season(), function(s) 
          stats$tas$present[[gcm]][[region()]][["mean"]][[s]]))) })
    dpr <- reactive({(60*60*24)*sapply(gcmsp, function(gcm) mean(sapply(season(), function(s)
      stats$pr[[period()]][[gcm]][[region()]][["mean"]][[s]])) - 
        mean(sapply(season(), function(s) 
          stats$pr$present[[gcm]][[region()]][["mean"]][[s]])))})
    
    xlim <- reactive({range(dtas())+c(-0.2,0.2)*diff(range(dtas()))})#input$tlim})
    ylim <- reactive({range(dpr())+c(-0.2,0.2)*diff(range(dpr()))})#input$plim})
    
    #make scatterplot
    scatterplot(dtas(),dpr(),ix=NULL,xlim=xlim(),ylim=ylim(),
                xlab="Temperature change (deg C)",ylab="Precipitation change (mm/day)",
                main=paste("Climate change present day (1981-2010) to",tolower(input$period)),
                show.legend=FALSE,im=im(),legend=seq(length(dtas())),pal=NULL,#"rainbow",
                pch=seq(length(dtas())),cex=1.4,lwd=1.5,new=FALSE)
    
    #add colored legend
    legcols <- two.colors(n=11,start="red",end="green",middle = "orange") #colors for background of legend
    peri <- reactive({switch(period(),"nf"=1,"ff"=2)})
    meanRelSpreadIndx <- reactive({floor((mean(dtasRelSpread()[,peri(),])+mean(dprRelSpread()[,peri(),]))/2*10)+1 }) 
    dpr <- reactive({(60*60*24)*mean(dprSpread()[,peri(),])})
    dprSel <- reactive({(60*60*24)*mean(dprSelSpread()[,peri(),])})
    dprRel <- reactive({mean(dprRelSpread()[,peri(),])})
    dtas <- reactive({mean(dtasSpread()[,peri(),])})
    dtasSel <- reactive({mean(dtasSelSpread()[,peri(),])})
    dtasRel <- reactive({mean(dtasRelSpread()[,peri(),])})
    legend("bottomright",bg=legcols[meanRelSpreadIndx()], 
           legend=c("Selection spread:",paste("dT: ",round(dtasRel()*100),
            "% (",round(dtasSel(),1),"°C of total ",round(dtas(),1),"°C).",sep=""),
            paste("dP: ",round(dprRel()*100),"% (",round(dprSel(),2)," mm/day of total ",
                  round(dpr(),2)," mm/day).",sep="")),cex=1)
    
  }, width=500, height=500)


})
