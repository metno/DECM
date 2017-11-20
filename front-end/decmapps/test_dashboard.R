library(shinydashboard)
library(leaflet)
library(esd)
library(raster)
library(rgdal)

shinyApp(
  ui = dashboardPage(skin = 'red',
                     dashboardHeader(title = 'Data Evaluation for Climate Models (DECM)', titleWidth = '600px'),
                     dashboardSidebar(collapsed = FALSE,
                                      sidebarMenu(
                                        menuItem("KPIs", tabName = 'pu',startExpanded = TRUE,
                                                 menuSubItem("Biases", tabName = "score1"),
                                                 menuSubItem("Spread or Uncertainty", tabName = "score2"),
                                                 menuSubItem("Mean Annual Cycle", tabName = "score3"),
                                                 menuSubItem("Individual Model", tabName = "score4"),
                                                 menuSubItem("Climate Change", tabName = "score5"),
                                                 menuSubItem("Storm tracks", tabName = "score6")
                                        ),
                                        menuItem("Settings and filtering", tabName = "du", startExpanded = TRUE,
                                                 selectInput("param7",
                                                             label = "Element",
                                                             choices = c("Temperature", # "Precip. sum",
                                                                         "Wet-day freq.","Precip. intensity"),
                                                             selected = "Temperature",width = '100%'),
                                                 selectInput("rcp7",label = "Scenario",
                                                             choices = c("Intermediate emissions (RCP4.5)","Low emissions (RCP2.6)", "High emissions (RCP8.5)"),
                                                             selected = "Intermediate emissions (RCP4.5)",width = '100%'),
                                                 selectInput(inputId = 'im',label = "Model",choices = c('Ens. Mean','------',gcmnames.26),
                                                             selected = 'Ens. Mean',width = '100%'),
                                                 selectInput("season7",label = "Season",
                                                             choices = c("Annual (All seasons)","Winter (DJF)","Spring (MAM)", "Summer (JJA)", "Autumn (SON)"),
                                                             selected = "Winter (DJF)",width = '100%'),
                                                 sliderInput("lon7",
                                                             label = "Longitudes",
                                                             min = 0, max = 30, value = c(0, 30)),
                                                 sliderInput("lat7",
                                                             label = "Latitudes",
                                                             min = 55, max = 72, value = c(55, 72)),
                                                 sliderInput("dates7", "Years",min=1900, max=2099,
                                                             step = 5, value= c(2070,2099),
                                                             sep="",width = "100%"),
                                                 sliderInput("datesref", "Base period",min=1900, max=2099,
                                                             step = 5, value= c(1980,2010),
                                                             sep="",width = "100%"),
                                                 selectInput("legend",label = "Legend",
                                                             choices = c("Display","Hide"),
                                                             selected = "Hide",width = '100%'),
                                                 selectInput("minimap",label = "Mini Map",
                                                             choices = c("Display","Hide"),
                                                             selected = "Hide",width = '100%'),
                                                 textInput("threshold8",label = "Threshold", placeholder = "1",width = '100%')
                                        ))
                     ),
                     dashboardBody(
                       tabItems(
                         tabItem(tabName = "score1", box("score1")),
                         tabItem(tabName = "score2", box("score2")),
                         tabItem(tabName = "score3", 
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Map", p(), 
                                                      fluidPage(plotOutput("map"))),
                                             tabPanel("Plot", p(), 
                                                      infoBox(title = 'How to read the chart', color = 'red',
                                                              value = 'The chart shows the evolution as a function of years.'),
                                                      infoBox(title = 'Remember !', color = 'orange', icon = icon("list-alt"), 
                                                              value = 'some text here '),
                                                      box(fluidPage(plotOutput("plot")))),
                                             tabPanel("Summary", p(), infoBox(title = 'How to read !', color = 'orange', 
                                                                              icon = icon("list-alt"), 
                                                                              value = 'Monhtly Summary statistics'),
                                                      tableOutput("summary")
                                             ),
                                             tabPanel("Table", p(),tableOutput("table")),
                                             tabPanel("Distribution", p(), box(fluidPage(plotOutput("hist")))),
                                             tabPanel("Taylor Diagram", p(), fluidPage(plotOutput("taylor"))),
                                             tabPanel("Scatter Plots", p() , fluidPage(plotOutput("scatter")))
                                 )),
                         tabItem(tabName = "score4", 
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Plot", p(), fluidPage(plotOutput("plot3"))),
                                             tabPanel("Summary", p(),verbatimTextOutput("summary3")),
                                             tabPanel("Table", p(),tableOutput("table3")),
                                             tabPanel("Map", p(), fluidPage(plotOutput("map3"))),
                                             tabPanel("Distribution", p(), box(fluidPage(plotOutput("hist3")))),
                                             tabPanel("Taylor Diagram", p(), fluidPage(plotOutput("taylor3"))),
                                             tabPanel("Scatter Plots", p() , fluidPage(plotOutput("scatter3")))
                                 )),
                         tabItem(tabName = "score4", box("pu-e")),
                         tabItem(tabName = "score5", 
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Map", p(), leafletOutput("map.cc",width = '100%',height = '950')),
                                             tabPanel("Plot", p(), fluidPage(plotOutput("plot.cc"))),
                                             tabPanel("Summary", p(),verbatimTextOutput("summary.cc")),
                                             tabPanel("Table", p(),tableOutput("table.cc")),
                                             tabPanel("Distribution", p(), box(fluidPage(plotOutput("hist.cc")))),
                                             tabPanel("Taylor Diagram", p(), fluidPage(plotOutput("taylor.cc"))),
                                             tabPanel("Scatter Plots", p() , fluidPage(plotOutput("scatter.cc")))
                                 )),
                         tabItem(tabName = "score6", box("du-b"))
                       ),
                       title = "Example"
                     )
  ),
  server = function(input, output,session) {
    data(Oslo)
    output$plot <- renderPlot(plot(Oslo, map.show = FALSE, new = FALSE))
    
    output$hist <- renderPlot(
      hist(Oslo,n = 50)
    )
    
    output$summary <- renderTable({
      df <- data.frame(Months = month.abb, as.data.frame(summary(Oslo)))
    })
    
    # Generate an HTML table view of the data ----
    output$table <- renderTable({
      df <- data.frame(Date = as.character(index(Oslo)),Value=coredata(Oslo))
    })
    
    ## %% Climate Change %% ##
    
    getz1 <- function(season7,dates7,lon7,lat7,rcp7,param7,im,datesref)({
      print('in Y1 function')
      #browser()
      # SET PARAMETERS
      season <- switch(tolower(as.character(season7)),
                       'Annual (All seasons)'='ann','winter'='djf','spring'='mam','summer'='jja','autumn'='son')
      rcp <- switch(tolower(as.character(rcp7)),
                    'intermediate emissions (rcp4.5)'='45','low emissions (rcp2.6)'='26','high emissions (rcp8.5)'='85')
      param <- switch(tolower(as.character(param7)),
                      'temperature'='t2m','wet-day freq.'='fw','precip. intensity'='mu',
                      'precip. sum'='ptot')
      browser()
      it <- range(as.numeric(dates7))
      it.ref <- range(as.numeric(datesref))
      is <- list(lon=as.numeric(lon7),lat=as.numeric(lat7))
      
      # load the data 
      eval(parse(text = paste('z <- Z4$',paste(param,season,rcp,sep='.'),sep='')))
      
      if (input$im == 'Ens. Mean') {
        z1 <- subset.dsensemble(z,it=it,is=is)
        zz <- subset.dsensemble(z,it=it.ref,is=is)
      } else {
        gcmnames <- names(z)[grep('_',names(z))]
        im1 <- is.element(gcmnames,im)
        z1 <- esd::subset.dsensemble.multi(z,im=im1,it=it,is=is)
        zz <- esd::subset.dsensemble.multi(z,im=im1,it=it.ref,is=is)
        #zz <- subset(z,im=!im1,it=it.ref,is=is)
      }
      if (input$param7 == 'Temperature')
        coredata(z1) <- coredata(z1) - coredata(zz)
      else 
        coredata(z1) <- (coredata(z1) - coredata(zz)) / coredata(zz) * 100
      # main <- paste(gcmnames[im1],' - ensemble mean (number of runs=',sum(im),') ',
      #               season,'/',input$rcp1,': ',it[1],'-',it[2],sep='')
      invisible(z1)
    }) 
    
    gety1 <- function(season7,dates7,lon7,lat7,rcp7,param7,im,datesref)({
      print('in Y1 function')
      #browser()
      # SET PARAMETERS
      season <- switch(tolower(as.character(season7)),
                       'Annual (All seasons)'='ann','winter'='djf','spring'='mam','summer'='jja','autumn'='son')
      rcp <- switch(tolower(as.character(rcp7)),
                    'intermediate emissions (rcp4.5)'='45','low emissions (rcp2.6)'='26','high emissions (rcp8.5)'='85')
      param <- switch(tolower(as.character(param7)),
                      'temperature'='t2m','wet-day freq.'='fw','precip. intensity'='mu',
                      'precip. sum'='ptot')
      
      it <- range(as.numeric(dates7))
      it.ref <- range(as.numeric(datesref))
      is <- list(lon=as.numeric(lon7),lat=as.numeric(lat7))
      
      # load the data 
      eval(parse(text = paste('z <- Z4$',paste(param,season,rcp,sep='.'),sep='')))
      # browser()
      
      if (input$im == 'Ens. Mean') {
        z1 <- subset.dsensemble(z,it=it,is=is)
        zz <- subset.dsensemble(z,it=it.ref,is=is)
        y1 <- map(z1,FUN="mean",FUNX='mean',plot=FALSE)
        yy <- map(zz,FUN="mean",FUNX='mean',plot=FALSE)
      } else {
        gcmnames <- names(z)[grep('_',names(z))]
        im1 <- is.element(gcmnames,im)
        z1 <- esd::subset.dsensemble.multi(z,im=im1,it=it,is=is)
        zz <- esd::subset.dsensemble.multi(z,im=im1,it=it.ref,is=is)
        #zz <- subset(z,im=!im1,it=it.ref,is=is)
        y1 <- map(z1,plot=FALSE)
        yy <- map(zz,plot=FALSE)
      }
      if (input$param7 == 'Temperature')
        coredata(y1) <- coredata(y1) - coredata(yy)
      else 
        coredata(y1) <- (coredata(y1) - coredata(yy)) / coredata(yy) * 100
      # main <- paste(gcmnames[im1],' - ensemble mean (number of runs=',sum(im),') ',
      #               season,'/',input$rcp1,': ',it[1],'-',it[2],sep='')
      invisible(y1)
    }) 
    
    observe({
      if (tolower(input$rcp7) == 'intermediate emissions (rcp4.5)') 
        choices <- c('Ens. Mean','------',gcmnames.45)
      else if (tolower(input$rcp7) == 'high emissions (rcp8.5)')
        choices <- c('Ens. Mean','------',gcmnames.85)
      else if (tolower(input$rcp7) == 'low emissions (rcp2.6)')
        choices <- c('Ens. Mean','------',gcmnames.26)
      updateSelectInput(session,inputId = "im", choices = choices, selected = choices[1]) # 
    })
    
    # reactive expressions for the map
    ysm1 <- reactive({
      return(gety1('Winter',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
    })
    
    ysm2 <- reactive({
      return(gety1('Spring',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
    })
    
    ysm3 <- reactive({
      return(gety1('Summer',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
    })
    
    ysm4 <- reactive({
      return(gety1('Autumn',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
    })
    
    # reactive expressions for z
    zsm1 <- reactive({
      return(getz1('Winter',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
    })
    
    zsm2 <- reactive({
      return(getz1('Spring',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
    })
    
    zsm3 <- reactive({
      return(getz1('Summer',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
    })
    
    zsm4 <- reactive({
      return(getz1('Autumn',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
    })
    
    # Render the map
    z.reactive <- reactive({
      
      ## browser()
      season <- switch(tolower(as.character(input$season7)),
                       'annual (all seasons)'='ann','winter (djf)'='djf','spring (mam)'='mam','summer (jja)'='jja','autumn (son)'='son')
      
      if (season == 'ann')  {
        
        z1 <- zsm1()
        z2 <- zsm2()
        z3 <- zsm3()
        z4 <- zsm4()
        
        z <- z1
        coredata(z) <- (coredata(z1) + coredata(z2) + coredata(z3) + coredata(z4)) / 4
        rm('zmap1','zmap2','zmap3','zmap4')
        
      } else {
        if (season == 'djf')
          z <- zsm1()
        else if (season == 'mam')
          z <- zsm2()
        else if (season == 'jja')
          z <- zsm3()
        else if (season == 'son')
          z <- zsm4()
      }
      return(z)
    })
    
    # Render the map
    zmap.reactive <- reactive({
      
      ## browser()
      season <- switch(tolower(as.character(input$season7)),
                       'annual (all seasons)'='ann','winter (djf)'='djf','spring (mam)'='mam','summer (jja)'='jja','autumn (son)'='son')
      
      if (season == 'ann')  {
        
        zmap1 <- ysm1()
        zmap2 <- ysm2()
        zmap3 <- ysm3()
        zmap4 <- ysm4()
        
        zmap <- zmap1
        coredata(zmap) <- (coredata(zmap1) + coredata(zmap2) + coredata(zmap3) + coredata(zmap4)) / 4
        rm('zmap1','zmap2','zmap3','zmap4')
        
      } else {
        if (season == 'djf')
          zmap <- ysm1()
        else if (season == 'mam')
          zmap <- ysm2()
        else if (season == 'jja')
          zmap <- ysm3()
        else if (season == 'son')
          zmap <- ysm4()
      }
      return(zmap)
    })
    
    observe(priority = 0, {
      zmap <- zmap.reactive()
      output$map.cc <- renderLeaflet({
        ## browser()
        print('zmap contains ...')
        # str(zmap)
        cat('ObserveEvent','PARAM',input$param7,'SEASON', input$season7,'RCP',input$rcp7,'SM',input$im)
        cat(sep = '\n')
        x <- attr(zmap,'longitude')
        y <- attr(zmap,'latitude')
        z <- coredata(zmap)
        ## browser()
        #Create raster object
        dat1 <- list(x=attr(zmap,'longitude'),y = attr(zmap,'latitude'), z = coredata(zmap))
        dim(dat1$z) <- c(length(dat1$x),length(dat1$y))
        r <- raster(dat1)
        print(print(object.size(r),units = 'Mb'))
        
        if ((input$param7 == 'Temperature')) {
          rev <- FALSE
          col <- 'warm'
          rng <- round(range(r@data@values,na.rm=TRUE),digits = 1)
          breaks <- c(0,max(rng))
          #breaks <- seq(-5,5,0.5)
          leg.title <- "Change [C]"
        } else if (input$param7 == 'Precip. sum') {
          rev <- TRUE
          col <- 't2m'
          breaks <- seq(-50,50,5)
          leg.title <- 'Change [%]'
        } else if (input$param7 == 'Wet-day freq.') {
          rev <- FALSE
          col <- 't2m'
          rng <- round(range(r@data@values,na.rm=TRUE),digits = 0)
          breaks <- c(-max(abs(rng)),max(abs(rng)))
          #breaks <- seq(0,1,0.05)
          leg.title <- 'Change [%]'
        } else if (input$param7 == 'Precip. intensity') {
          rev <- TRUE
          col <- 't2m'
          rng <- round(range(r@data@values,na.rm=TRUE),digits = 1)
          breaks <- c(-max(abs(rng)),max(abs(rng))) #seq(0,20,0.05)
          leg.title <- 'Change [%]'
        }
        
        pal <- colorBin(colscal(col = col,rev=rev),breaks, bins = 10, pretty = TRUE,na.color = NA)
        
        ## custom label format function
        myLabelFormat = function(..., reverse_order = FALSE){
          if(reverse_order){
            function(type = "numeric", cuts){
              cuts <- sort(cuts, decreasing = T)
            }
          } else{
            labelFormat(...)
          }
        }
        m <- leaflet() %>%
          addProviderTiles(providers$Esri.WorldStreetMap,
                           #addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)) %>%
          setView(lat=64,lng = 16, zoom = 5) %>%
          addRasterImage(x = r,colors = pal, opacity = 0.65) 
        ## browser()  
        if (input$legend == 'Display')
          m <- m %>% addLegend("bottomleft", values=round(r@data@values, digits = 2), 
                               title=leg.title, colors = rev(colscal(col= col, rev = rev, n=length(pretty(breaks,n = 10)))),
                               labels = rev(pretty(breaks,n = 10)),#pal=pal, 
                               layerId="colorLegend")  # labFormat = myLabelFormat(reverse_order = F)
        # m <- m %>% addLegend("topleft", values=round(r@data@values, digits = 2), title=leg.title, colors = pal(round(r@data@values, digits = 2)),labels = seq(1,10,1),#pal=pal, 
        #                      labFormat = myLabelFormat(reverse_order = T),layerId="colorLegend") # labFormat = myLabelFormat(reverse_order = T),
        if (input$minimap == 'Display')
          m <- m %>% addMiniMap()
        m
      })
      
      output$summary.cc <- renderPrint({
        str(zmap)
      })
      output$table.cc <- renderPrint({
        z <- z.reactive()
      })
    })
    
  }
)

