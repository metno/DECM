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
  
  # Statistics used in the app
  spreadPr <- reactive({60*60*24*spread(varid="pr", season=input$season, 
                                   region=input$region, period=Period(), im=NULL)})
  spreadPrSel <- reactive({60*60*24*spread(varid="pr", season=input$season, 
                                      region=input$region, period=Period(), im=im())})
  spreadTas <- reactive({spread(varid="tas", season=input$season, 
                           region=input$region, period=Period(), im=NULL)})
  spreadTasSel <- reactive({spread(varid="tas", season=input$season, region=input$region,
                              period=Period(), im=im())})
  spreadPrRel <- reactive({spreadPrSel()/spreadPr()})
  spreadPrIndx <- reactive({floor(mean(spreadPrRel())*10)+1 })
  spreadTasRel <- reactive({spreadTasSel()/spreadTas()})
  spreadTasIndx <- reactive({floor(mean(spreadTasRel())*10)+1 })
  
  # Generate table with selected GCMs and their ranking
  gcmtable <- reactive({
    Z <- cbind(as.character(im()),
               gsub(".*:","",gcmnames[im()]),
               as.character(weightedrank()))
    Z <- as.data.frame(Z)
    colnames(Z) <- c("#","Model name","Rank")
    return(Z)
  })
  
  gcmtableBest <- reactive({
    Z <- cbind(as.character(best()),
               gsub(".*:","",gcmnames[best()]))
    Z <- as.data.frame(Z)
    colnames(Z) <- c("#","Model name")
    return(Z)
  })
  
  ## Calculations used for text colors
  weightedrank <- reactive({weightedrank_all()[im()]})
  mean_weightedrank <- reactive({mean(weightedrank(),na.rm=TRUE)})
  
  legcolsrank <- two.colors(n=107,start="green",end="red",middle = "orange") #colors for ranks
  meanRelMetricsIndx <- reactive({floor(mean_weightedrank())}) #color index based on weighted rank
  
  legcols <- two.colors(n=11,start="red",end="green",middle = "orange") #colors for percentage number
  meanRelSpreadIndx_nf <- reactive({floor(weightedspread_nf()*10)+1}) #color index based on weighted mean rel. spread for near future
  meanRelSpreadIndx_ff <- reactive({floor(weightedspread_ff()*10)+1}) #color index based on weighted mean rel. spread for far future
  
  output$IntroText  <- renderText({
    paste("GCMeval is a tool for selecting and evaluating a subset of climate models from ",
          "the CMIP5 ensemble based on:<br>",
          "<ul><li>the spread in projected climate changes, and</li>",
          "<li> the skill of individual climate models to reproduce the climate of the past.</li></ul><br>")
  })
  
  output$DisclaimerText <- renderText({
    paste("<i>Disclaimer: This is a prototype and should not be used as a basis for decision making.</i>")
  })
  
  output$RankingText  <- renderText({
    paste("<i>Start out by picking <b>two focus regions</b>. Then select <b>weights</b> (i.e., the importance) of ",
          "the regions, seasons, variables, and skill scores.",
          "A <b>performance score and model ranking</b> is then calculated for the model ensemble ",
          "based on your choices.</i>")
  })
  
  ## Output: text about ranking
  output$MetricText  <- renderText({
    paste("The mean weighted model rank of the selected models is  ",
          "<font size = +1, font color=\"",
          legcolsrank[meanRelMetricsIndx()],"\"><b> rank ", round(mean_weightedrank()), 
          "  of  ",length(gcmst),"</b></font>.<br>",sep="")
  })
  
  output$ModelsTable <- DT::renderDataTable({
    datatable(gcmtable(), caption=HTML("<font size=+1 color='black'><b>Selected models</b></font>"), 
              options=list(dom='t',pageLength=input$ngcm), 
              rownames=FALSE) %>% formatStyle(
                'Rank',
                target = 'row',
                backgroundColor = styleEqual(seq(length(gcmnames)), 
                                  two.colors(n=length(gcmnames), start="green",
                                              end="red", middle = "orange"))
    )
  })

  output$ModelsTableBest <- DT::renderDataTable({
    datatable(gcmtableBest(), caption=HTML("<font size=+1><b>Best performing models</b></font>"), 
              options=list(dom='t',pageLength=input$ngcm), 
              rownames=FALSE
    )
  })
  
  textOut <- reactive({
    legcols <- two.colors(n=11, start="red", end="green", middle="orange") 
    if(input$weighted) {
      txt <- paste("The weighted mean spread of the selected models for all ",
                    "regions, seasons and variables is <br><br>",
                   "<font size = +1, font color=\"",
                   legcols[meanRelSpreadIndx_ff()],"\"><b>", round(weightedspread_nf()*100),
                   "%</b></font>  for the <b>near future</b> and <br>",
                   "<font size = +1, font color=\"",
                   legcols[meanRelSpreadIndx_ff()],"\"><b>", round(weightedspread_ff()*100), 
                   "%</b></font> for the <b>far future</b> <br><br> compared to the spread ",
                   "of the whole ensemble.<br><br>",sep="")        
    } else {
      txt <- paste("The spread of the selected models compared to the spread ",
                   "of the whole ensemble is <br><br>",
            "<font size = +1, font color=\"",legcols[spreadTasIndx()],"\"><b>",
            round(spreadTasRel()*100),
            "%</b> </font>  for <b>temperature</b> and <br>",
            "<font size = +1, font color=\"", legcols[spreadPrIndx()],"\"><b>", 
            round(spreadPrRel()*100), 
            "%</b> </font> for <b>precipitation</b> <br><br> ",
            "for the selected region, season, time horizon, and scenario.<br><br>",sep="")
    }
    return(txt)
  })

  ## Output: text about spread
  output$SpreadText  <- renderText({
    textOut()
  })
  
  ## Output: text about scatterplot
  output$ScatterText  <- renderText({
    paste("<i>Use the scatterplot below to study the spread in projected changes for a specific region, season and time-line.",
          " Ideally, the subset of models should have a spread similar to the spread of the whole ensemble.",
          " Try to increase the relative spread among your selected climate models by adding more models at the edges of the scatterplot.</i>",sep="")
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
            marker=list(color=clr()), text=gcmnames, source="A")
    layout(p, title=paste("Present day (1981-2010) to",tolower(input$period)),
           xaxis=list(title="Temperature change (deg C)"),
           yaxis=list(title="Precipitation change (mm/day)"))#, dragmode="lasso")
  })

  output$clickevent <- renderPrint({
    event_data("plotly_click", source="A")
  })

  # When selecting GCMs in plotly scatterplot, update gcms and ngcm
  observe({
    d <- event_data(event="plotly_click", source="A")
    if(!is.null(d)) {
      i <- sort(unique(c(as.numeric(gsub(":.*","",input$gcms)),d$pointNumber+1)))
      updateCheckboxGroupInput(session, inputId = "gcms", 
                               choices = gcmnames, selected = gcmnames[i])
      updateNumericInput(session, inputId = "ngcm", value=length(input$gcms), 
                         min=1, max=length(gcmst))
    }
  })

  # When selecting GCMs from the checkboxes, update ngcm
  observeEvent(input$gcms,{
    updateNumericInput(session, inputId = "ngcm", 
                       value=length(input$gcms), min=1, max=length(gcmst))
  })
  
  # When clicking 'best' button, select best performing GCMs
  observeEvent(input$best, {
    i <- best()
    updateCheckboxGroupInput(session, inputId = "gcms", choices = gcmnames, 
                             selected = gcmnames[i])
  })

  # When clicking 'random' button, select random GCMs
  observeEvent(input$randomize, {
    i <- sample(1:length(gcmnames),input$ngcm,replace=FALSE)
    updateCheckboxGroupInput(session, inputId = "gcms", choices = gcmnames, 
                             selected = gcmnames[i])
  })
  
  # Reset plotly clicks when changing GCM selection (gcms)  
  observeEvent(input$gcms,{
    js$resetClick()
  })
  
})
