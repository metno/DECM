# define the back-end

function(input, output,session) {
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
    print('in Z1 function')
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
    
    if (input$im == 'Ens. Mean') {
      z1 <- subset.dsensemble(z,it=it,is=is)
      zz <- subset.dsensemble(z,it=it.ref,is=is)
      ## Need to debug here ... browser()
      y1 <- plot(expandpca(z1),new = FALSE) 
      yy <- plot(expandpca(zz),new = FALSE)
    } else {
      gcmnames <- names(z)[grep('_',names(z))]
      im1 <- is.element(gcmnames,im)
      z1 <- esd::subset.dsensemble.multi(z,im=im1,it=it,is=is)
      zz <- esd::subset.dsensemble.multi(z,im=im1,it=it.ref,is=is)
      y1 <- plot(expandpca(z1),new = FALSE) ; dev.off()
      yy <- plot(expandpca(zz),new = FALSE) ; dev.off()
      }
    # browser()
    
    print(paste('input$param7 is : ', input$param7))
    
    if (input$param7 == 'Temperature')
      y1 <- y1 - mean(coredata(yy),na.rm=TRUE)
    else 
      y1 <- (y1 - mean(coredata(yy),na.rm=TRUE)) / mean(coredata(yy),na.rm=TRUE) * 100
    # main <- paste(gcmnames[im1],' - ensemble mean (number of runs=',sum(im),') ',
    #               season,'/',input$rcp1,': ',it[1],'-',it[2],sep='')
    
    invisible(y1)
  }) 
  
  getLoc <- function(season7,dates7,lon7,lat7,rcp7,param7,im,datesref,loc7,ts = FALSE)({
    print('in location function')
    
    # SET PARAMETERS
    season <- switch(tolower(as.character(season7)),
                     'Annual (All seasons)'='ann','winter'='djf','spring'='mam','summer'='jja','autumn'='son')
    rcp <- switch(tolower(as.character(rcp7)),
                  'intermediate emissions (rcp4.5)'='45','low emissions (rcp2.6)'='26','high emissions (rcp8.5)'='85')
    param <- switch(tolower(as.character(param7)),
                    'temperature'='t2m','wet-day freq.'='fw','precip. intensity'='mu',
                    'precip. sum'='ptot')
    iLoc <- switch(tolower(as.character(loc7)),
                    'temperature'='t2m','wet-day freq.'='fw','precip. intensity'='mu',
                    'precip. sum'='ptot')
    it <- range(as.numeric(dates7))
    it.ref <- range(as.numeric(datesref))
    loc.names <- names(z)
    is <- which(is.element(loc.names,loc7))
    
    # load the data 
    eval(parse(text = paste('z <- Z3$',paste(param,season,rcp,sep='.'),sep='')))
    
    z0 <- z[[is]]
    z1 <- subset(z0,it=it)
    zz <- subset(z0,it=it.ref)
    # browser()
    print(paste('input$param7 is : ', input$param7))
    y1 <- z1
    if (input$param7 == 'Temperature')
      coredata(y1) <- coredata(z1) - mean(coredata(zz),na.rm=TRUE)
    else 
      coredata(y1) <- (coredata(z1) - mean(coredata(zz),na.rm=TRUE)) / mean(coredata(zz),na.rm=TRUE) * 100
    # main <- paste(gcmnames[im1],' - ensemble mean (number of runs=',sum(im),') ',
    #               season,'/',input$rcp1,': ',it[1],'-',it[2],sep='')
    colnames(y1) <- attr(y1,'model_id') ## quick fix here ...
    # browser()
    # if (input$im == 'Ens. Mean') {
    #   yy <- zoo(rowMeans(y1,na.rm = TRUE),order.by = index(y1))
    #   yy <- attrcp(y1,yy)
    #   class(yy) <- class(y1)
    # } else {
    #   gcmnames <- attr(y1,'model_id')
    #   yy <- subset(as.station(y1), is = which(is.element(gcmnames,strsplit(im,fixed = TRUE, split= '_')[[1]][2])))
    # }
    # attr(yy,'ci') <- apply(y1,1,FUN='sd',na.rm = TRUE)
    # invisible(yy)
    invisible(y1)
  })
  
  gety1 <- function(season7,dates7,lon7,lat7,rcp7,param7,im,datesref,ts = FALSE)({
    print('in Y1 function')
    
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
    
    if (input$im == 'Ens. Mean') {
      z1 <- subset.dsensemble(z,it=it,is=is)
      zz <- subset.dsensemble(z,it=it.ref,is=is)
      y1 <- map.dsensemble(z1,FUN="mean",FUNX='mean',plot=FALSE)
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
    if (ts & (input$im == 'Ens. Mean')) 
      y1 <- expandpca(y1) 
    else
      
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
  
  # reactive expressions for Locations
  
  loc1 <- reactive({
    return(getLoc('Winter',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref,input$loc7))
  })
  loc2 <- reactive({
    return(getLoc('Spring',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref,input$loc7))
  })
  loc3 <- reactive({
    return(getLoc('Summer',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref,input$loc7))
  })
  loc4 <- reactive({
    return(getLoc('Autumn',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref,input$loc7))
  })
  
  # Reactive expressions for map
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

    season <- switch(tolower(as.character(input$season7)),
                     'annual (all seasons)'='ann','winter (djf)'='djf','spring (mam)'='mam','summer (jja)'='jja','autumn (son)'='son')
    
    if (season == 'ann')  {
      
      z1 <- zsm1()
      z2 <- zsm2()
      z3 <- zsm3()
      z4 <- zsm4()
      
      z <- z1
      coredata(z) <- (coredata(z1) + coredata(z2) + coredata(z3) + coredata(z4)) / 4
      rm('z1','z2','z3','z4')
      
    } else {
      if (season == 'djf')
        zall <- zsm1()
      else if (season == 'mam')
        zall <- zsm2()
      else if (season == 'jja')
        zall <- zsm3()
      else if (season == 'son')
        zall <- zsm4()
    }
    return(zall)
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
  
  # Render the Locations
  loc.reactive <- reactive({
    
    ## browser()
    season <- switch(tolower(as.character(input$season7)),
                     'annual (all seasons)'='ann','winter (djf)'='djf','spring (mam)'='mam','summer (jja)'='jja','autumn (son)'='son')
    
    if (season == 'ann')  {
      
      z1 <- loc1()
      z2 <- loc2()
      z3 <- loc3()
      z4 <- loc4()
      
      z <- z1
      coredata(z) <- (coredata(z1) + coredata(z2) + coredata(z3) + coredata(z4)) / 4
      rm('z1','z2','z3','z4')
      
    } else {
      if (season == 'djf')
        zall <- loc1()
      else if (season == 'mam')
        zall <- loc2()
      else if (season == 'jja')
        zall <- loc3()
      else if (season == 'son')
        zall <- loc4()
    }
    return(zall)
  })
  
  observe(priority = 0, {
    
    zmap <- zmap.reactive()
    zval <- z.reactive()
    sta <- loc.reactive()
    attr(sta,'eof') <- NULL
    
    # allGCM <- attr(y1,'model_id')
    # if (input$selim == 'All') {
    #   updateCheckboxGroupInput(session,input$selim,selected = allGCM)  
    # } 
    
    content <- as.character(loc(sta))
    #browser()
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
        addRasterImage(x = r,colors = pal, opacity = 0.65) %>%
        addCircleMarkers(lng = as.numeric(lon(sta)), lat = as.numeric(lat(sta)), 
                         radius = 8,stroke = TRUE, fill = 'red', color = 'black',weight = 0.8,label = content) #%>%
        # addPopups(lng = as.numeric(unlist(lapply(1:length(sta), function(i) lon(sta[[i]])))), 
        #           lat = as.numeric(unlist(lapply(1:length(sta), function(i) lat(sta[[i]])))),
        #           popup = content)
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
      summary(zval)
    })
    
    output$plot.cc <- renderPlotly({
      
      ## Compute the ens. mean
      em <- rowMeans(sta,na.rm = TRUE)
      es <- apply(coredata(sta),1,FUN='sd')
      
      df <- data.frame(Date = as.character(year(sta)), Value = round(em,digits = 1),
                       low  = round(em - 1.96 * es,digits = 1),
                       high = round(em + 1.96 * es,digits = 1))
      
      df.obs <- data.frame(Date = as.character(year(attr(sta,'station'))), Value = round(as.anomaly(attr(sta,'station')),digits = 2))
      p <- plot_ly(df, x= ~as.character(year(sta)))
      
      p <- p %>% add_markers(data = df.obs, x = ~Date, y = ~ Value , name = 'Observations', 
                             marker = list(size = 10, color = 'rgba(204,204, 204, .8)',
                                           line = list(color = 'rgba(51, 51, 51, .9)',width = 1))) 
      
      if (input$loess) {
        change <- fitted(loess(df$Value~as.numeric(df$Date)))
        p <- p %>% 
          add_trace(y = ~ change , type = 'scatter', name = 'Ens. Mean',mode = "lines", line = list(color = c('darkorange'), width = 4))
        p <- p %>% add_trace(p, y = fitted(loess(df$low~as.numeric(df$Date))) , type = 'scatter', 
                             name = 'Lower limit', mode = "lines", line = list(dash = 'dash',color = c('darkorange'), width = 4, shape ="spline")) %>%
          add_trace(p, x=~df$Date, y=~fitted(loess(df$high~as.numeric(df$Date))) , type = 'scatter', 
                    mode = "lines", name = 'Upper limit',line = list(dash = 'dash',color = c('darkorange'), width = 4,shape = "spline")) 
      } else {
        p <- p %>% 
          add_trace(y = ~ df$Value , type = 'scatter', name = 'Simulation',mode = "lines", line = list(color = c('darkorange'), width = 4))
        
        #df.err2 <- data.frame(Date = as.character(year(sta)), Value =  0.9 * round(attr(sta,'ci'),digits = 1))
        p <- p %>% add_trace(p, y = ~ df$low , type = 'scatter', 
                             name = 'Lower limit', mode = "lines", line = list(dash = 'dash',color = c('darkorange'), width = 4, shape ="spline")) %>%
          add_trace(p, x=~df$Date, y=~ df$high , type = 'scatter', 
                    mode = "lines", name = 'Upper limit',line = list(dash = 'dash',color = c('darkorange'), width = 4,shape = "spline")) 
      }
      if (length(input$selim) >0) {
        for (i in 1:length(input$selim)) {
          allGCM <- attr(sta,'model_id')
          gcm <- strsplit(input$selim[i],fixed = TRUE, split= '_')[[1]][2]
          eval(parse(text = paste()))
          eval(parse(text = paste('m',i,' <- as.numeric(sta[,which(is.element(allGCM,gcm))])',sep='')))
          eval(parse(text = paste("p <- p %>% add_trace(p, y = ~ m",i,
                                  ", type = 'scatter', name = gcm, mode = 'lines', line = list(width = 1, shape ='spline'))",sep='')))
        }
      }
      p <- p %>% 
        layout(title = paste("Climate Change signal realtive to the base period",paste(input$datesref,collapse='/'), " at ",toupper(loc(sta)),sep =' '),
               xaxis = list(title = "Dates",gridcolor = "#bfbfbf"),
               yaxis = list(title = "Climate Change",gridcolor = "#bfbfbf"))
      
      p$elementId <- NULL
      p
    })
    
    # Unfinished
    output$prob2.cc <- renderPlotly({
      z <- c(as.numeric(zval))
      thresh <- as.numeric(input$threshold8)
      direction <- 'colder'
      zx <- ceiling(max(c(abs(z),abs(thresh)),na.rm=TRUE))+1
      breaks <- seq(-zx,zx,by=0.5)
      prob <- 1 - pnorm(thresh,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE))
      
      X <- seq(-zx,zx,by=0.05)
      lines(X,dnorm(X,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE)),
            lwd=5,col=rgb(0.5,0,0,0.3))
      if (as.character(direction) == "Colder") {
        Xless <- X[X <= thresh]
        polygon(c(Xless,max(Xless),min(Xless)),
                c(dnorm(Xless,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE)),0,0),
                col=rgb(1,0,0,0.2))
      } else {
        Xmore <- X[X >= thresh]
        polygon(c(max(Xmore),min(Xmore),Xmore),
                c(0,0,dnorm(X[X >= thresh],mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE))),
                col=rgb(1,0,0,0.2))
      }
      # if (as.character(input$direction4) == "Colder") 
      #   prob <- pnorm(input$threshold8,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE)) else
      
      df <- data.frame(breaks = breaks, Value = z)
      p <- plot_ly(source = "source") %>%
        add_histogram(p, data = df, x = ~breaks, y = ~Value, orientation = 'v')
      p <- p %>%
        layout(title = paste(toupper(input$im), "Climate Change signal realtive to the base period modeled by ", sep =' '),
               xaxis = list(title = "Dates", gridcolor = "#bfbfbf"),
               yaxis = list(title = "Climate Change", gridcolor = "#bfbfbf"))
      add_lines(p,x = X, y = dnorm(X,mean(mean(z,na.rm=TRUE))))
      p
      })
    
    output$gcm.table <- renderDataTable({
      df <- data.frame(Names = gcmnames.26)
    })
    
    output$prob.cc <- renderPlot({
      z <- c(as.numeric(zval))
      thresh <- as.numeric(input$threshold8)
      direction <- 'colder'
      zx <- ceiling(max(c(abs(z),abs(thresh)),na.rm=TRUE))+1
      breaks <- seq(-zx,zx,by=0.5)
      # if (as.character(input$direction4) == "Colder") 
      #   prob <- pnorm(input$threshold8,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE)) else
      prob <- 1 - pnorm(thresh,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE))
      main <- paste('Location test','using ',input$im,' model runs: the probability of',
                    tolower('higher'),'than',round(thresh,2),
                    'is',round(100*prob,2))
      hist(z,breaks=breaks,main=main,new=FALSE,freq=FALSE,col='grey')
      X <- seq(-zx,zx,by=0.05)
      lines(X,dnorm(X,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE)),
            lwd=5,col=rgb(0.5,0,0,0.3))
      if (as.character(direction) == "Colder") {
        Xless <- X[X <= thresh]
        polygon(c(Xless,max(Xless),min(Xless)),
                c(dnorm(Xless,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE)),0,0),
                col=rgb(1,0,0,0.2))
      } else {
        Xmore <- X[X >= thresh]
        polygon(c(max(Xmore),min(Xmore),Xmore),
                c(0,0,dnorm(X[X >= thresh],mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE))),
                col=rgb(1,0,0,0.2))
      }
    })
    
    output$table.cc <- renderTable({
      data.frame(Date = as.character(index(zval)), Value = coredata(zval))
    })
  
    # output$scatter.cc <- renderTable({
    #   data.frame(Date = as.character(index(zval)), Value = coredata(zval))
    # })
  
##### Scatter plot server
    output$scatter.cc <- renderPlotly({
      season <- switch(input$season7,
                       'Annual (All seasons)'='ann',
                       'Winter (DJF)'=c('dec','jan','feb'),
                       'Spring (MAM)'=c('mar','apr','may'),
                       'Summer (JJA)'=c('jun','jul','aug'),
                       'Autumn (SON)'=c('sep','oct','nov'))
      #'annual mean'='ann','winter'='djf','spring'='mam',
      #'summer'='jja','autumn'='son')
      period <- switch(tolower(as.character(input$period)),
                       "2071-2100"='ff',
                       "2021-2050"='nf')
      
      gcms <- names(stats$tas$ff)
      if(tolower(input$region)=="global") {
        coord <- list(lon=c(-180,180),lat=c(-90,90))
        dtas <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
          stats$tas[[period]][[gcm]][["mean"]][[s]])) - 
            mean(sapply(season, function(s)
              stats$tas$present[[gcm]][["mean"]][[s]])))
        dpr <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
          stats$pr[[period]][[gcm]][["mean"]][[s]])) - 
            mean(sapply(season, function(s)
              stats$pr$present[[gcm]][["mean"]][[s]])))
        #dtas <- sapply(gcms, function(gcm) stats$tas[[period]][[gcm]][["mean"]][[season]] - 
        #                 stats$tas$present[[gcm]][["mean"]][[season]]) 
        #dpr <- sapply(gcms, function(gcm) stats$pr[[period]][[gcm]][["mean"]][[season]] - 
        #                stats$pr$present[[gcm]][["mean"]][[season]])
      } else {
        #i.srex <- which(srex$name==input$region)
        region <- 'ALA' #srex$label[i.srex]
        dtas <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
          stats$tas[[period]][[gcm]][[region]][["mean"]][[s]])) - 
            mean(sapply(season, function(s) 
              stats$tas$present[[gcm]][[region]][["mean"]][[s]])))
        dpr <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
          stats$pr[[period]][[gcm]][[region]][["mean"]][[s]])) - 
            mean(sapply(season, function(s) 
              stats$pr$present[[gcm]][[region]][["mean"]][[s]])))
        #dtas <- sapply(gcms, function(gcm) stats$tas[[period]][[gcm]][[region]][["mean"]][[season]] - 
        #                stats$tas$present[[gcm]][[region]][["mean"]][[season]])
        #dpr <- sapply(gcms, function(gcm) stats$pr[[period]][[gcm]][[region]][["mean"]][[season]] - 
        #                                  stats$pr$present[[gcm]][[region]][["mean"]][[season]])
      }
      im <- as.numeric(gsub(":.*","",input$gcms))
      
      d <- data.frame(var1 = dtas, var2 = dpr*(60*60*24))
      # colnames(d) <- c('Temperature Change','Precipitation Change')
      p <- plot_ly(d, x = ~var1, y = ~var2, type = 'scatter', color = paste('gcms',1:9), 
                   # Hover text:
                   text = ~paste("Model N : ", im, '$<br> Score:', 2),
                   marker = list(size = 15, symbol = 'cross-open-dot',
                                 color = colscal(n=9),
                                 line = list(color = col2rgb('grey40'))),
                   mode = 'markers') 
      
      p <- p %>%
        layout(title = paste(toupper(input$im), 
                             "Climate Change signal realtive to the base period modeled by ", sep =' '),
               xaxis = list(title = "Temperature Change [deg. C]"),
               yaxis = list(title = "Precipitation Change [%]"))  
    p  
      
      # scatterplot(dtas,dpr*(60*60*24),ix=NULL,xlim=input$tlim,ylim=input$plim,
      #             xlab="Temperature change (deg C)",ylab="Precipitation change (mm/day)",
      #             main=paste("Climate change assuming RCP4.5\npresent day (1981-2010) to",input$period),
      #             show.legend=FALSE,im=im,
      #             legend=seq(length(dtas)),pal=NULL,#pal="cat",pch=21,
      #             pch=as.character(seq(length(dtas))),cex=1.5,lwd=1.5,new=FALSE)
    })
    
    # output$map <- renderPlot({
    #   if(tolower(input$region)=="global") {
    #     region <- list(lon=c(-180,-180,180,180,-180),lat=c(-90,90,90,-90,-90))
    #   } else {
    #     i.srex <- which(srex$name==input$region)
    #     region <- list(lon=srex$coord[[i.srex]][1,],
    #                    lat=srex$coord[[i.srex]][2,])
    #   }
    #   par(mgp=c(1,0.5,0),mar=c(0.2,0.2,0.2,0.2))
    #   plot(geoborders$x,geoborders$y,col="grey30",type="l",lwd=0.5,
    #        xlim=c(-180,180),ylim=c(-90,90),
    #        xlab="Longitude",ylab="Latitude",xaxt="n",yaxt="n")
    #   #lines(attr(geoborders,'borders')$x,attr(geoborders,'borders')$y,col="grey30")
    #   par(xaxt="s",yaxt="s",las=1,col.axis='grey',col.lab='grey20',
    #       cex.lab=0.7,cex.axis=0.7)
    #   axis(3,at=pretty(par("xaxp")[1:2],n=5),col='grey50')
    #   axis(2,at=pretty(par("yaxp")[1:2],n=5),col='grey50')
    #   grid()
    #   lines(region$lon,region$lat,col="blue",lwd=1.5,lty=1)
    # }, width=200,height=200*0.6)#width=250, height=175)
    
    
  })
  
}

