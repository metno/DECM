## Load libraries and define functions: 
source("global.R")

## Define a server for the Shiny app
shinyServer(function(input, output, session) {

  # Region selection for ranking
  Regionlist <- reactive({
    rl <- list(input$regionwm1,input$regionwm2)
    rl[which(rl != "---")]
  })
  
  im <- reactive({as.numeric(gsub(":.*","",input$gcms))})

  ## Region selection for scatterplot
  Region <- reactive({
    if(tolower(input$region)=="global") {
      region <- "global"
    } else {
      i.srex <- which(srex$name==input$region)
      region <- srex$label[i.srex]
    }
    return(region)
  })
  
  ## Season selection for scatterplot
  Season <- reactive({switch(tolower(as.character(input$season)),
                             'annual mean'='ann',
                             'winter'=c('dec','jan','feb'),
                             'spring'=c('mar','apr','may'),
                             'summer'=c('jun','jul','aug'),
                             'autumn'=c('sep','oct','nov'))})
  ## Period selection for scatterplot
  Period <- reactive({switch(tolower(as.character(input$period)),
                             "far future (2071-2100)"='ff',
                             "near future (2021-2050)"='nf')})

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

  ## Temperature and precip. spread for scatterplot
  dtas <- reactive({sapply(gcmst, function(gcm) mean(sapply(Season(), function(s)
    stats$tas[[Period()]][[gcm]][[Region()]][["mean"]][[s]])) - 
      mean(sapply(Season(), function(s) 
        stats$tas$present[[gcm]][[Region()]][["mean"]][[s]]))) })
  dpr <- reactive({(60*60*24)*sapply(gcmsp, function(gcm) mean(sapply(Season(), function(s)
    stats$pr[[Period()]][[gcm]][[Region()]][["mean"]][[s]])) - 
      mean(sapply(Season(), function(s) 
        stats$pr$present[[gcm]][[Region()]][["mean"]][[s]])))})
  
  ## Calculations used for text colors
  weightedrank <- reactive({weightedrank_all()[im()]})
  mean_weightedrank <- reactive({mean(weightedrank(),na.rm=TRUE)})
  
  legcolsrank <- two.colors(n=107,start="green",end="red",middle = "orange") #colors for ranks
  meanRelMetricsIndx <- reactive({floor(mean_weightedrank())}) #color index based on weighted rank
  
  legcols <- two.colors(n=11,start="red",end="green",middle = "orange") #colors for percentage number
  meanRelSpreadIndx_nf <- reactive({floor(weightedspread_nf()*10)+1}) #color index based on weighted mean rel. spread for near future
  meanRelSpreadIndx_ff <- reactive({floor(weightedspread_ff()*10)+1}) #color index based on weighted mean rel. spread for far future
  
  ## Output: text about spread
  output$IntroText  <- renderText({
    paste("This is a tool for selecting and evaluating a group of climate models from of the CMIP5 ensemble.<br><br>",
          "The chosen climate models are evaluated based on the following criteria: <br>",
          "A) The skill of individual climate models to reproduce the climate of the past. <br>",
          "B) The spread in future climate changes, which should ideally be as large as the spread in the whole ensemble of available climate model simulations.<br><br>")
  })
  
  output$DisclaimerText <- renderText({
    paste("<i>Disclaimer: This is a prototype and should not be used as a basis for decision making. The GCM names might not correspond to the real GCM data.</i>")
  })
  
  output$RankingText  <- renderText({
    paste("Start out by chosing two regions of interest. Then select weights (i.e., the importance) of different regions, seasons, variables, and skill scores.",
          "Based on your choices, we will calculate a weighted performance score and rank the models.")
  })
  
  ## Output: text about ranking
  output$MetricText  <- renderText({
    paste("The mean weighted model performance rank of the selected models is<br><font size = +1, font color=\"",
          legcolsrank[meanRelMetricsIndx()],"\"><b> Rank ", round(mean_weightedrank()), "  of  ",length(gcmst),"</b></font>.<br><br>",
          "The following models have been selected:<br>",paste(im(),collapse="/"),
          ", <br> and their individual performance ranks are: <br>",paste(weightedrank(),collapse="/"),".<br><br>",
          "The best performing ",input$ngcm," models would be:<br>",paste(best(),collapse="/"),".",sep="")
  })
  
  ## Output: text about spread
  output$SpreadText  <- renderText({
    paste("The weighted mean spread of the selected models is <br><br>","<font size = +1, font color=\"",
          legcols[meanRelSpreadIndx_ff()],"\"><b>", round(weightedspread_nf()*100), "%</b>
                </font>  for the <b>near future</b> and <br>",
          "<font size = +1, font color=\"", 
          legcols[meanRelSpreadIndx_ff()],"\"><b>", round(weightedspread_ff()*100), "%</b>
                </font> for the <b>far future</b> <br><br> compared to the spread of the whole ensemble.<br><br>",sep="")
    
  })

  ## Output: text about spread
  output$ScatterText  <- renderText({
    paste("Use the scatterplot below to study the spread in projected changes for a specific region, season and time-line.",
          " How well does your selection of models represent the range of possible climate changes?",
          " Ideally, your subset of models should have a spread similar to the spread of the whole ensemble.",
          " Try to increase the relative spread among your selected climate models by adding more models at the edges of the scatterplot.",sep="")
  })
  
    
  ## Output: map 1
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
  
  ## Output: map 2
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
  
  ## Output: map for scatterplot region selection
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
  
  ## Output: scatterplot of temperature and precip. change 
  output$dtdpr <- renderPlotly({
    
    c1 <- rgb(116,196,215,150,maxColorValue=255)
    c2 <- rgb(0,144,168,255,maxColorValue=255)
    clr <- reactive({
      x <- rep(c1,length(gcmst))
      x[im()] <- c2
      return(x)})
    
    p <- plot_ly(data.frame(x=dtas(),y=dpr()), x=~x, y=~y, type="scatter", mode="markers",
            marker=list(color=clr()), text=gcmnames, source="scatter")
    layout(p, title=paste("Present day (1981-2010) to",tolower(input$period)),
           xaxis=list(title="Temperature change (deg C)"),
           yaxis=list(title="Precipitation change (mm/day)"), dragmode="lasso")
  })

  # Output: textbox with information about spread to go with scatterplot
  output$spread <- renderText({
    #add colored legend
    legcols <- two.colors(n=11,start="red",end="green",middle = "orange") #colors for background of legend
    
    dpr <- reactive({60*60*24*spread(varid="pr",season=input$season,region=input$region,period=Period(),im=NULL)})
    dprSel <- reactive({60*60*24*spread(varid="pr",season=input$season,region=input$region,period=Period(),im=im())})
    dprRel <- reactive({dprSel()/dpr()})
    dtas <- reactive({spread(varid="tas",season=input$season,region=input$region,period=Period(),im=NULL)})
    dtasSel <- reactive({spread(varid="tas",season=input$season,region=input$region,period=Period(),im=im())})
    dtasRel <- reactive({dtasSel()/dtas()})
    meanRelSpreadIndx <- reactive({floor((mean(dtasRel())+mean(dprRel()))/2*10)+1 })
    paste("<table border='1'><tr bgcolor='",legcols[meanRelSpreadIndx()],"'> <td style='padding: 10px;'> ",
          "Selection spread:<br>",paste("dT: ",round(dtasRel()*100),"% (",round(dtasSel(),1),"°C of total ",round(dtas(),1),"°C).<br>",sep=""),
          paste("dP: ",round(dprRel()*100),"% (",round(dprSel(),2)," mm/day of total ",round(dpr(),2)," mm/day).<br>",sep=""),
          " </td> </tr> </table>",sep="")
  })

  ## Reactive input
  observe({
    if(length(input$gcms)!=input$ngcm) {
      sel <- "My selection"
    } else {
      sel <- input$gcmselect
    }
    updateSelectInput(session, inputId = "gcmselect", selected=sel, choices = c("My selection","Random","Best performance")) 
  })
  
  observe({
    if(length(input$gcms)!=input$ngcm) {
      updateNumericInput(session, inputId = "ngcm", value=length(input$gcms), min=1, max=length(gcmst))
    }
  })
  
  observe({
    if (tolower(input$gcmselect)=="best performance") {
      i <- best()
    } else if(tolower(input$gcmselect)=="random") {
      i <- sample(1:length(gcmnames),input$ngcm,replace=FALSE)
    } else if(tolower(input$gcmselect)=="my selection") {
      i <- as.numeric(gsub(":.*","",input$gcms))
    }
    updateCheckboxGroupInput(session, inputId = "gcms", choices = gcmnames, selected = gcmnames[i])
  })
  
  observe({
    d <- event_data(event="plotly_click",source="scatter")
    if(!is.null(d)) {
      i <- sort(unique(c(as.numeric(gsub(":.*","",input$gcms)),d$pointNumber+1)))
      updateCheckboxGroupInput(session, inputId = "gcms", choices = gcmnames, selected = gcmnames[i])
    }
    js$resetClick()
  })
  
})
