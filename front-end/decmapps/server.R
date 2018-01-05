# define the back-end

function(input, output,session) {
  
  ## Messages / Notifications / Tasks
  
  notData <- reactive({
    if (!is.null(input$rows)) {
      newmodel <- input$rows
    } else 
      newmodel <- 'No_model_selected'
    return(rbind(ntfs, data.frame(message= unlist(strsplit(newmodel,split = ' ')),status = 'success')))
  })
  
  msgData <- reactive({
    return(msgs)
  })
  
  
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
  
  # Climate Change
  
  getz1 <- function(season7,dates7,lon7,lat7,rcp7,param7,im,datesref)({
    print('in Z1 function')
    #
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
    # 
    if (input$im == 'Ens. Mean') {
      z1 <- esd::subset.dsensemble(z,it=it,is=is)
      zz <- esd::subset.dsensemble(z,it=it.ref,is=is)
      ## Need to debug here ... 
      y1 <- plot(expandpca(z1),new = FALSE) ; dev.off()
      yy <- plot(expandpca(zz),new = FALSE) ; dev.off()
    } else {
      gcmnames <- names(z)[grep('_',names(z))]
      im1 <- is.element(gcmnames,im)
      z1 <- esd::subset.dsensemble.multi(z,im=im1,it=it,is=is)
      zz <- esd::subset.dsensemble.multi(z,im=im1,it=it.ref,is=is)
      y1 <- plot(expandpca(z1),new = FALSE) ; dev.off()
      yy <- plot(expandpca(zz),new = FALSE) ; dev.off()
    }
    # 
    
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
    
    # load the data 
    eval(parse(text = paste('z <- Z3$',paste(param,season,rcp,sep='.'),sep='')))
    loc.names <- names(z)
    is <- which(is.element(loc.names,loc7))    
    
    z0 <- z[[is]]
    z1 <- subset(z0,it=it)
    zz <- subset(z0,it=it.ref)
    # 
    print(paste('input$param7 is : ', input$param7))
    y1 <- z1
    if (input$param7 == 'Temperature')
      coredata(y1) <- coredata(z1) - mean(coredata(zz),na.rm=TRUE)
    else 
      coredata(y1) <- (coredata(z1) - mean(coredata(zz),na.rm=TRUE)) / mean(coredata(zz),na.rm=TRUE)
    # main <- paste(gcmnames[im1],' - ensemble mean (number of runs=',sum(im),') ',
    #               season,'/',input$rcp1,': ',it[1],'-',it[2],sep='')
    colnames(y1) <- attr(y1,'model_id') ## quick fix here ...
    # 
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
      z1 <- esd::subset.dsensemble(z,it=it,is=is)
      zz <- esd::subset.dsensemble(z,it=it.ref,is=is)
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
  
  ## projected change
  t2m.djf.45 <- reactive({
    return(getLoc('winter',input$dates7,input$lon7,input$lat7,'intermediate emissions (rcp4.5)','Temperature',input$im,input$datesref,input$loc7))
  })
  t2m.mam.45 <- reactive({
    return(getLoc('spring',input$dates7,input$lon7,input$lat7,'intermediate emissions (rcp4.5)','Temperature',input$im,input$datesref,input$loc7))
  })
  t2m.jja.45 <- reactive({
    return(getLoc('summer',input$dates7,input$lon7,input$lat7,'intermediate emissions (rcp4.5)','Temperature',input$im,input$datesref,input$loc7))
  })
  t2m.son.45 <- reactive({
    return(getLoc('autumn',input$dates7,input$lon7,input$lat7,'intermediate emissions (rcp4.5)','Temperature',input$im,input$datesref,input$loc7))
  })
  # for mu
  mu.djf.45 <- reactive({
    return(getLoc('winter',input$dates7,input$lon7,input$lat7,'intermediate emissions (rcp4.5)','precip. intensity',input$im,input$datesref,input$loc7))
  })
  mu.mam.45 <- reactive({
    return(getLoc('spring',input$dates7,input$lon7,input$lat7,'intermediate emissions (rcp4.5)','precip. intensity',input$im,input$datesref,input$loc7))
  })
  mu.jja.45 <- reactive({
    return(getLoc('summer',input$dates7,input$lon7,input$lat7,'intermediate emissions (rcp4.5)','precip. intensity',input$im,input$datesref,input$loc7))
  })
  mu.son.45 <- reactive({
    return(getLoc('autumn',input$dates7,input$lon7,input$lat7,'intermediate emissions (rcp4.5)','precip. intensity',input$im,input$datesref,input$loc7))
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
    
    ## 
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
    
    ## 
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
  
  dtdp.reactive <- reactive({
    dc <- NULL
    # cc in temp 2021-2050
    dc$t2m$djf$rcp45$nf <- colMeans(coredata(subset(t2m.djf.45(),it = c(2021,2050)))) - colMeans(coredata(subset(t2m.djf.45(),it = input$datesref)))
    dc$t2m$mam$rcp45$nf <- colMeans(coredata(subset(t2m.mam.45(),it = c(2021,2050)))) - colMeans(coredata(subset(t2m.mam.45(),it = input$datesref)))
    dc$t2m$jja$rcp45$nf <- colMeans(coredata(subset(t2m.jja.45(),it = c(2021,2050)))) - colMeans(coredata(subset(t2m.jja.45(),it = input$datesref)))
    dc$t2m$son$rcp45$nf <- colMeans(coredata(subset(t2m.son.45(),it = c(2021,2050)))) - colMeans(coredata(subset(t2m.son.45(),it = input$datesref)))
    
    # cc in mu by 2021-2050
    dc$mu$djf$rcp45$nf <- colMeans(coredata(subset(mu.djf.45(),it = c(2021,2050)))) - colMeans(coredata(subset(mu.djf.45(),it = input$datesref)))
    dc$mu$mam$rcp45$nf <- colMeans(coredata(subset(mu.mam.45(),it = c(2021,2050)))) - colMeans(coredata(subset(mu.mam.45(),it = input$datesref)))
    dc$mu$jja$rcp45$nf <- colMeans(coredata(subset(mu.jja.45(),it = c(2021,2050)))) - colMeans(coredata(subset(mu.jja.45(),it = input$datesref)))
    dc$mu$son$rcp45$nf <- colMeans(coredata(subset(mu.son.45(),it = c(2021,2050)))) - colMeans(coredata(subset(mu.son.45(),it = input$datesref)))
    
    # cc in temp 2071-2100
    dc$t2m$djf$rcp45$ff <- colMeans(coredata(subset(t2m.djf.45(),it = c(2071,2100)))) - colMeans(coredata(subset(t2m.djf.45(),it = input$datesref)))
    dc$t2m$mam$rcp45$ff <- colMeans(coredata(subset(t2m.mam.45(),it = c(2071,2100)))) - colMeans(coredata(subset(t2m.mam.45(),it = input$datesref)))
    dc$t2m$jja$rcp45$ff <- colMeans(coredata(subset(t2m.jja.45(),it = c(2071,2100)))) - colMeans(coredata(subset(t2m.jja.45(),it = input$datesref)))
    dc$t2m$son$rcp45$ff <- colMeans(coredata(subset(t2m.son.45(),it = c(2071,2100)))) - colMeans(coredata(subset(t2m.son.45(),it = input$datesref)))
    
    # cc in mu by 2071-2100
    dc$mu$djf$rcp45$ff <- colMeans(coredata(subset(mu.djf.45(),it = c(2071,2100)))) - colMeans(coredata(subset(mu.djf.45(),it = input$datesref))) 
    dc$mu$mam$rcp45$ff <- colMeans(coredata(subset(mu.mam.45(),it = c(2071,2100)))) - colMeans(coredata(subset(mu.mam.45(),it = input$datesref)))
    dc$mu$jja$rcp45$ff <- colMeans(coredata(subset(mu.jja.45(),it = c(2071,2100)))) - colMeans(coredata(subset(mu.jja.45(),it = input$datesref)))
    dc$mu$son$rcp45$ff <- colMeans(coredata(subset(mu.son.45(),it = c(2071,2100)))) - colMeans(coredata(subset(mu.son.45(),it = input$datesref)))
    
    # reformat mu.cc table
    cm.names <- names(dc$mu$djf$rcp45$nf)
    mu.cc <- data.frame(val=dc$mu$djf$rcp45$nf,rcp='rcp45',season='djf',period='nf',param='mu',cm = cm.names, stringsAsFactors = FALSE) 
    mu.cc <- rbind(mu.cc, data.frame(val=dc$mu$djf$rcp45$ff,rcp='rcp45',season='djf',period='ff',param='mu',cm = cm.names,stringsAsFactors = FALSE))
    mu.cc <- rbind(mu.cc, data.frame(val=dc$mu$mam$rcp45$nf,rcp='rcp45',season='mam',period='nf',param='mu',cm = cm.names,stringsAsFactors = FALSE))
    mu.cc <- rbind(mu.cc, data.frame(val=dc$mu$mam$rcp45$ff,rcp='rcp45',season='mam',period='ff',param='mu',cm = cm.names,stringsAsFactors = FALSE)) 
    mu.cc <- rbind(mu.cc, data.frame(val=dc$mu$jja$rcp45$nf,rcp='rcp45',season='jja',period='nf',param='mu',cm = cm.names,stringsAsFactors = FALSE))
    mu.cc <- rbind(mu.cc, data.frame(val=dc$mu$jja$rcp45$ff,rcp='rcp45',season='jja',period='ff',param='mu',cm = cm.names,stringsAsFactors = FALSE))
    mu.cc <- rbind(mu.cc, data.frame(val=dc$mu$son$rcp45$nf,rcp='rcp45',season='son',period='nf',param='mu',cm = cm.names,stringsAsFactors = FALSE))
    mu.cc <- rbind(mu.cc, data.frame(val=dc$mu$son$rcp45$ff,rcp='rcp45',season='son',period='ff',param='mu',cm = cm.names,stringsAsFactors = FALSE))
    
    # reformat t2m.cc table
    t2m.cc <- data.frame(val=dc$t2m$djf$rcp45$nf,rcp='rcp45',season='djf',period='nf',param='t2m',cm = cm.names, stringsAsFactors = FALSE) 
    t2m.cc <- rbind(t2m.cc, data.frame(val=dc$t2m$djf$rcp45$ff,rcp='rcp45',season='djf',period='ff',param='t2m',cm = cm.names,stringsAsFactors = FALSE))
    t2m.cc <- rbind(t2m.cc, data.frame(val=dc$t2m$mam$rcp45$nf,rcp='rcp45',season='mam',period='nf',param='t2m',cm = cm.names,stringsAsFactors = FALSE))
    t2m.cc <- rbind(t2m.cc, data.frame(val=dc$t2m$mam$rcp45$ff,rcp='rcp45',season='mam',period='ff',param='t2m',cm = cm.names,stringsAsFactors = FALSE)) 
    t2m.cc <- rbind(t2m.cc, data.frame(val=dc$t2m$jja$rcp45$nf,rcp='rcp45',season='jja',period='nf',param='t2m',cm = cm.names,stringsAsFactors = FALSE))
    t2m.cc <- rbind(t2m.cc, data.frame(val=dc$t2m$jja$rcp45$ff,rcp='rcp45',season='jja',period='ff',param='t2m',cm = cm.names,stringsAsFactors = FALSE))
    t2m.cc <- rbind(t2m.cc, data.frame(val=dc$t2m$son$rcp45$nf,rcp='rcp45',season='son',period='nf',param='t2m',cm = cm.names,stringsAsFactors = FALSE))
    t2m.cc <- rbind(t2m.cc, data.frame(val=dc$t2m$son$rcp45$ff,rcp='rcp45',season='son',period='ff',param='t2m',cm = cm.names,stringsAsFactors = FALSE))
    
    cc <- cbind(t2m.cc[,1],mu.cc[,-5])
    colnames(cc) <- c('t2m','mu','rcp','season','period','model')
    
    return(cc)
  })
  
  output$tgcm <- DT::renderDataTable({
    DT::datatable(model.45, rownames = FALSE,  
                  options = list(selection = 'multiple',pageLength=20), 
                  callback = JS("table.on('search.dt', function() {
                            $(this).toggleClass('selected');
                            Shiny.onInputChange('rows',table.rows({page:'all',selected:true}).data().toArray());
                    });"))
  }) #options = list(pageLength=20)})
  
  # selModel <- reactive({
  #   ed <- event_data("plotly_click")
  #   if (!is.null(ed))
  #     selMod <- c(selMod,models.45[ed$pointNumber + 1])
  #   selMod <- c(selMod,input$rows)
  #   invisible(selMod)
  #   
  # })
  # 
  # selModel <- reactive({
  #   if (is.null(selModelTable()) & is.null(selModelPlot()))
  #     Y <- NULL
  #   else
  #     Y <- c(selModelTable(),selModelPlot())
  #   return(isolate(Y))  
  # })
  # 
  gcm.sc.vals <- function(param,region,period) {
    if (param == 'tas')
       	gcms <- names(stats$tas$ff)
    else if (param == 'pr')
	gcms <- names(stats$pr$ff)

    period <- switch(tolower(as.character(period)),
                     "present (1981-2010)"='present',
                     "far future (2071-2100)"='ff',
                     "near future (2021-2050)"='nf')
    ref <- NULL
    if(tolower(region)=="global") {
      region <- "global"
    } else {
      i.srex <- which(srex$name==region)
      region <- srex$label[i.srex]
    }
    x <- lapply(gcms, function(gcm) stats[[param]][[period]][[gcm]][[region]][["mean"]][2:13])
    
    if(period=="present") {
      id.ref <- grep('era',names(stats[[param]][[period]])) 
      ref <- stats[[param]][[period]][[id.ref]][[region]][["mean"]][2:13]
    }#
    
    if(param=="pr") {      
     #browser()      
     x <- lapply(x, function(y) y*60*60*24) ## convert to mm/day
      ref <- ref*1E3 ## convert to mm/day
    }
  
    gcm.vals <- as.data.frame(lapply(1:length(x),function(i) {if (length(x[[i]]) > 0) x[[i]] else x[[1]]})) ## AM Need to fix this later on     
    colnames(gcm.vals) <- paste('gcm.',1:length(gcm.vals),sep='')

    df <- data.frame(gcm.vals,ref,stringsAsFactors = FALSE)
  }
  
  gcm.sc.tas.reactive <- reactive({
    return(gcm.sc.vals(param = 'tas',region = input$region, period = input$period))
  })
  
  gcm.sc.pr.reactive <- reactive({
    return(gcm.sc.vals(param = 'pr',region = input$region,  period = input$period))
  })
  
  # observe(priority = -1, {
  #   updateSelectInput(session = session, inputId = input$region.sc,choices = region.names,selected = input$region)
  #   isloate(input$region.sc)
  # })
  # 
  # observe(priority = -2, {
  #   updateSelectInput(session = session, inputId = input$region,choices = region.names,selected = input$region.sc)
  #   isolate(input$region)
  # })
  # 
  
  sta.meta <- reactive({
    
    data(package= 'esd', station.meta)
    
    if (input$param7 == 'Temperature')
      param <- 't2m'
    else
      param = 'precip'
    
    sta.meta <- select.station(param= param, src = 'ECAD')
    
  })
  
  
  observe( priority = 0, { # 
    
    ## test <- t2m.djf.45()
    
    output$map.cc <- renderLeaflet({
      sta <- loc.reactive()
      # quick fix in as.station.dsensemble
      attr(sta,'eof') <- NULL
      content <- as.character(loc(sta))
      
      zmap <- zmap.reactive()
      print('zmap contains ...')
      # str(zmap)
      cat('ObserveEvent','PARAM',input$param7,'SEASON', input$season7,'RCP',input$rcp7,'SM',input$im)
      cat(sep = '\n')
      x <- attr(zmap,'longitude')
      y <- attr(zmap,'latitude')
      z <- coredata(zmap)
      ## 
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
      ##   
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
      summary(z.reactive())
    })
    
    output$plot.cc <- renderPlotly({
      sta <- loc.reactive()
      # quick fix in as.station.dsensemble
      attr(sta,'eof') <- NULL
      content <- as.character(loc(sta))
      
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
      if (length(input$rows) >0) {
        for (i in 1:length(input$rows)) {
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
      z <- c(as.numeric(z.reactive()))
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
      data.frame(Names = gcmnames.45)
    })
    
    output$prob.cc <- renderPlot({
      z <- c(as.numeric(z.reactive()))
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
    
    output$table.cc <- DT::renderDataTable({
      DT::datatable(data.frame(Date = as.character(index(z.reactive())), Value = coredata(z.reactive())),
                    selection = 'multiple', 
                    callback = JS("table.on('click.dt', function() {
                            $(this).toggleClass('selected');
                            Shiny.onInputChange('rows',table.rows('.selected').data().toArray());
                    });"),
                    extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller','Select'),
                    rownames=FALSE,
                    options=list(dom = 'Bfrtip',
                                 searching = T,
                                 pageLength = 25,
                                 searchHighlight = FALSE,
                                 colReorder = TRUE,
                                 fixedHeader = TRUE,
                                 filter = 'top',
                                 buttons = c('copy', 'csv','excel', 'print'),
                                 paging    = TRUE,
                                 deferRender = TRUE,
                                 scroller = TRUE,
                                 scrollX = TRUE,
                                 scrollY = 800,
                                 select.style = 'os'
                    ))
    })
    
    ## Global Climate Models Menu item ---      
    ## Metadata table
    data(package= 'DECM', metaextract)
    library(dplyr)
    meta <- meta %>% mutate_if(is.factor, as.character)
    META <- meta[,c('project_id','gcm','gcm_rip','rcm','longname','var','unit','frequency',
                    'resolution','lon','lon_unit','lat','lat_unit',"experiment_id",'dim','dates','url','creation_date')]
    
    cm.meta <- subset(META, subset = (project_id == 'CMIP5') & (var == 'tas'))
    cm.meta <- cm.meta[, c('project_id','gcm','gcm_rip','rcm','frequency', 
                           'resolution','lon','lon_unit','lat','lat_unit',
                           "experiment_id",'dim','dates','url','creation_date')]
    
    output$gcm.meta.tas <- DT::renderDataTable({
      ## Metadata table
    data(package= 'DECM', metaextract)
    library(dplyr)
    meta <- meta %>% mutate_if(is.factor, as.character)
    META <- meta[,c('project_id','gcm','gcm_rip','rcm','longname','var','unit','frequency',
                    'resolution','lon','lon_unit','lat','lat_unit',"experiment_id",'dim','dates','url','creation_date')]
    
    cm.meta <- subset(META, subset = (project_id == 'CMIP5') & (var == 'tas'))
    cm.meta <- cm.meta[, c('project_id','gcm','gcm_rip','rcm','var','unit','frequency', 
                           'resolution','lon','lon_unit','lat','lat_unit',
                           "experiment_id",'dim','dates','url','creation_date')]

      DT::datatable(cm.meta,
                    selection = 'multiple', 
                    callback = JS("table.on('click.dt', function() {
                            $(this).toggleClass('selected');
                            Shiny.onInputChange('rows2',table.rows('.selected').indexes().toArray());
                    });"),
                    extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller','Select'),
                    rownames=FALSE,
                    options=list(dom = 'Bfrtip',
                                 buttons = c('colvis',
                                             'selectAll','selectNone',
                                             'copy', 'csv','excel', 'print'),
                                 searching = T,
                                 pageLength = 30,
                                 searchHighlight = FALSE,
                                 colReorder = TRUE,
                                 fixedHeader = FALSE,
                                 filter = 'top',
                                 paging    = TRUE,
                                 deferRender = TRUE,
                                 scroller = TRUE,
                                 scrollX = TRUE,
                                 responsive = TRUE,
                                 select.style = 'os',
                                 scrollY = 800
                    ))
    })
    
    output$gcm.meta.pr <- DT::renderDataTable({
      ## Metadata table
    data(package= 'DECM', metaextract)
    library(dplyr)
    meta <- meta %>% mutate_if(is.factor, as.character)
    META <- meta[,c('project_id','gcm','gcm_rip','longname','var','unit','frequency',
                    'resolution','lon','lon_unit','lat','lat_unit',"experiment_id",'dim','dates','url','creation_date')]
    
    cm.meta <- subset(META, subset = (project_id == 'CMIP5') & (var == 'pr'))
    cm.meta <- cm.meta[, c('project_id','gcm','gcm_rip','frequency', 
                           'resolution','lon','lon_unit','lat','lat_unit',
                           "experiment_id",'dim','dates','url','creation_date')]

      DT::datatable(cm.meta,
                    selection = 'multiple', 
                    callback = JS("table.on('click.dt', function() {
                            $(this).toggleClass('selected');
                            Shiny.onInputChange('rows2',table.rows('.selected').indexes().toArray());
                    });"),
                    extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller','Select'),
                    rownames=FALSE,
                    options=list(dom = 'Bfrtip',
                                 buttons = c('colvis',
                                             'selectAll','selectNone',
                                             'copy', 'csv','excel', 'print'),
                                 searching = T,
                                 pageLength = 30,
                                 searchHighlight = FALSE,
                                 colReorder = TRUE,
                                 fixedHeader = FALSE,
                                 filter = 'top',
                                 paging    = TRUE,
                                 deferRender = TRUE,
                                 scroller = TRUE,
                                 scrollX = TRUE,
                                 responsive = TRUE,
                                 select.style = 'os',
                                 scrollY = 800
                    ))
    })

    output$region <- renderLeaflet({
      id <- which(is.element(region.names,input$region))
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap,
                         #addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) 
      if (input$region == 'Global') {
        m <- m %>% addPolygons(lng = c(-180,180,180,-180,-180), lat = c(-90,-90,90,90,-90), 
                               fill = TRUE, color = 'black',weight = 0.8,label = 'Global') 
      } else {
        m <- m %>% addPolygons(lng = srex$coords[[id-1]][1,], lat = srex$coords[[id-1]][2,], 
                               fill = TRUE, color = 'black',weight = 0.8,label = srex$name[id-1]) 
      }
      #%>%
      # addPopups(lng = as.numeric(unlist(lapply(1:length(sta), function(i) lon(sta[[i]])))), 
      #           lat = as.numeric(unlist(lapply(1:length(sta), function(i) lat(sta[[i]])))),
      #           popup = content)
      ##   
      m
    })
    
    ## Seasonal scatter plot DT vs DP 
    output$gcm.scatter <- renderLeaflet({
      
      season <- switch(as.character(input$season7),
                       'Annual'='ann',
                       'Winter (DJF)'=c('dec','jan','feb'),
                       'Spring (MAM)'=c('mar','apr','may'),
                       'Summer (JJA)'=c('jun','jul','aug'),
                       'Autumn (SON)'=c('sep','oct','nov'))
      
      gcms <- names(stats$tas$ff)
      
      df <- function(season,period){
        
        #'annual mean'='ann','winter'='djf','spring'='mam',
        #'summer'='jja','autumn'='son')
        
        if(tolower(input$region)=="global") {
          region <- "global"
          dtas <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
            stats$tas[[period]][[gcm]][["mean"]][[s]])) - 
              mean(sapply(season, function(s) 
                stats$tas$present[[gcm]][["mean"]][[s]])))
          dpr <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
            stats$pr[[period]][[gcm]][["mean"]][[s]])) - 
              mean(sapply(season, function(s) 
                stats$pr$present[[gcm]][["mean"]][[s]])))
        } else {
          i.srex <- which(srex$name==input$region)
          region <- srex$label[i.srex]
          dtas <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
            stats$tas[[period]][[gcm]][[region]][["mean"]][[s]])) - 
              mean(sapply(season, function(s) 
                stats$tas$present[[gcm]][[region]][["mean"]][[s]])))
          dpr <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
            stats$pr[[period]][[gcm]][[region]][["mean"]][[s]])) - 
              mean(sapply(season, function(s) 
                stats$pr$present[[gcm]][[region]][["mean"]][[s]])))
        }
        dtas <- round(dtas,digits = 1)
        dpr <- round(dpr*(60*60*24)*100,digits = 1)
        invisible(data.frame(dtas,dpr))
      }
      
      df.nf <- df(season,'nf')
      df.ff <- df(season,'ff')
      
      df <- data.frame(dtas.nf= df.nf$dtas,dpr.nf= df.nf$dpr,dtas.ff= df.ff$dtas,dpr.ff = df.ff$dpr)
      
      p.scatter <- plot_ly(df) %>% 
        add_trace(x = ~dtas.nf, y = ~dpr.nf, mode = 'markers',name ='Near Future (2021-2050)',
                  marker = list(size = 10, symbol = 'circle', color = 'rgba(255, 182, 193, .6)',
                                line = list(color = 'rgba(152, 0, 0, .7)', width = 2))) %>% 
        add_trace(df.ff, x = ~dtas.ff, y = ~dpr.ff,mode = 'markers', name = 'Far Future (2071-2100)',
                  marker = list(size = 10, symbol = 'x', color = 'rgba(255, 182, 193, .6)',
                                line = list(color = 'rgba(152, 0, 0, .7)', width = 2)))
      
      if (!is.null(input$rows2)) {
        im <- input$rows2+1
        for (i in 1:length(im)) {
          eval(parse(text = paste("p.scatter <- p.scatter %>% add_trace(x = ~dtas.nf[",i,"], y = ~dpr.nf[",i,"], mode = 'markers',name =gcms[",i,"],
                                             marker = list(size = 14, symbol = 'circle', color = 'rgba(255, 182, 193, .6)',
                                                           line = list(color = I('black'), width = 2))) %>% 
                                                              add_trace(x = ~dtas.ff[",i,"], y = ~dpr.ff[",i,"],mode = 'markers', name = gcms[",i,"],
                                             marker = list(size = 14, symbol = 'x',color = 'rgba(255, 182, 193, .6)',
                                                           line = list(color = I('black'), width = 2)))",
                                  sep='')))
        }
      }
      
      p.scatter <- p.scatter %>% layout(title = "Scatter Plot",
                                        xaxis = list(title = 'Temperature Change [Degrees C]',autotick = FALSE,
                                                     tick0 = 0,dtick = 0.5,zeroline = TRUE,showline = TRUE,range(-0.5,10)),
                                        yaxis = list (title = "Precipitation Change [%]",autotick = FALSE,
                                                      tick0 = 0,dtick = 5,zeroline = TRUE,showline = TRUE,range = c(-5,20)))
      p.scatter$elementId <- NULL
      p.scatter
    })
    
    ## Seasonal cycle 
    output$gcm.sc.tas <- renderPlotly({
      browser()
      df <- gcm.sc.tas.reactive()
      df <- df[,-36] # AM Quick fix but has to be removed ... once meta is updated.
      # GCM seasonal cycle
      df.env <- NULL
      low <- apply(subset(df,select = grep('gcm',colnames(df))),1,min,na.rm=TRUE)
      high <- apply(subset(df,select = grep('gcm',colnames(df))),1,max,na.rm=TRUE)
      avg <- apply(subset(df,select = grep('gcm',colnames(df))),1,mean,na.rm=TRUE)
      df.env <- data.frame(low,avg,high,ref = df$ref,month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      
      # define layout output
      #df$ref <- df$ref
      if (input$type == 'Individual Models') {
        ## Make the plot
        #p.sc <- plot_ly(df, x = ~month, y = ~gcm.1,type = 'scatter',mode = 'markers+lines', line = list(width = 2, color = "grey",shape = 'spline'))
        # create plot_ly
        p.sc <- plot_ly(df, x = ~month)
        
        gcms <- colnames(df)[grep('gcm',colnames(df))]
        gcm.meta <- subset(meta, subset = (var == 'tas') & (project_id == 'CMIP5'))
        id <- as.integer(gcm.meta$gcm)
        rgbcolsa <- c('rgba(45,51,38)', 'rgba(87,77,102)', 'rgba(255,191,200)', 'rgba(140,129,105)', 'rgba(234,191,255)', 'rgba(172,230,195)', 'rgba(86,105,115)', 'rgba(115,86,94)', 'rgba(230,195,172)', 'rgba(255,234,191)', 'rgba(124,140,105)', 'rgba(51,26,43)', 'rgba(191,96,172)', 'rgba(184,204,102)', 'rgba(153,87,77)', 'rgba(96,134,191)', 'rgba(230,115,145)', 'rgba(255,145,128)', 'rgba(229,161,115)', 'rgba(22,58,89)', 'rgba(85,89,22)', 'rgba(127,83,32)', 'rgba(80,179,45)', 'rgba(18,51,13)', 'rgba(64,16,22)', 'rgba(22,16,64)', 'rgba(86,29,115)', 'rgba(54,98,217)', 'rgba(255,191,64)', 'rgba(61,182,242)', 'rgba(126,57,230)', 'rgba(51,38,13)', 'rgba(178,0,95)', 'rgba(0,128,85)', 'rgba(26,0,191)', 'rgba(255,0,238)', 'rgba(178,0,0)', 'rgba(0,202,217)', 'rgba(0,230,153)', 'rgba(0,255,34)', 'rgba(204,0,54)', 'rgba(102,0,14)', 'rgba(229,92,0)', 'rgba(0,107,115)', 'rgba(77,0,51)', 'rgba(204,255,0)', 'rgba(140,112,0)', 'rgba(12,89,0)')
        
         rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)', 'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        
        ## group by gcms
        colsa <- rgbcolsa[id]
        cols <- rgbcols[id]
        
        ## Add all models
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          leg.name <- paste(as.character(as.matrix(gcm.meta[i,c('gcm','gcm_rip')])),collapse = '  ')
          grp.name <- paste('Group',id[i],sep='')
          if (input$legend.sc == 'Display All')
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                  name = leg.name, mode = 'lines', 
                                  showlegend = TRUE, legendgroup = grp.name, colors = colsa[",i,"],
                                  line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          else if (input$legend.sc == 'Hide All')
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                  name = leg.name, mode = 'lines', 
                                  showlegend = TRUE, legendgroup = grp.name, colors = colsa[",i,"],
                                  line = list(color = colsa[",i,"], width = 2, shape ='spline',showlegend = FALSE))",sep='')))
        }
        
        ## Highlight selected models in tab:models
        if (!is.null(input$rows2)) {
          im <- input$rows2+1
          for (i in im) {
            leg.name <- paste(as.character(as.matrix(gcm.meta[i,c('gcm','gcm_rip')])),collapse = '  ')
            grp.name <- paste('Group',id[i],sep='')
            gcm <- gcms[i]
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                  name = leg.name, mode = 'lines', 
                                  showlegend = TRUE, colors = cols[",i,"],
                                  line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          }
        }
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'Reference', mode = 'lines', line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$type))) { # Make an enveloppe instead of lines
        
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = FALSE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',
                    fill = 'tonexty', fillcolor='rgba(255,127,80,0.2)', line = list(color = 'transparent'),
                    showlegend = FALSE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',
                    line = list(color='rgb(255,127,80)'),
                    name = 'Average') 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'Reference', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        
        
      } else if (grepl('box',tolower(input$type))) {
        p.sc <- plot_ly(df, type = 'box')
        
        for (i in 1:12) {
          leg.name <- month.abb[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  line = list(color='rgb(255,127,80)'),
                                  name = leg.name,showlegend =FALSE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name, showlegend = FALSE, 
                                         line = list(color = 'black', dash = 'dash', width = 2))
        } 
      }
      # Add these lines to modify colors in box plot
      # marker = list(color = 'rgb(135,206,250'),
      # line = list(color = 'rgb(135,206,250'),
      
      p.sc <- p.sc %>% layout(title = "Temperature [Degrees C]",
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = "Regional average in degrees C",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      if (input$legend.sc == 'Hide All')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      p.sc$elementId <- NULL
      p.sc
    })
    
    output$gcm.sc.pr <- renderPlotly({
      
df <- gcm.sc.pr.reactive()
      df.env <- NULL
      low <- apply(df[1:30],1,min,na.rm=TRUE)
      high <- apply(df[1:30],1,max,na.rm=TRUE)
      avg <- apply(df[1:30],1,mean,na.rm=TRUE)
      df.env <- data.frame(low,avg,high,ref = df$ref,month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      if (input$type == 'Individual Models') {
        ## Make the plot
        p.sc <- plot_ly(df,x = ~month)
        
        gcms <- colnames(df)[grep('gcm',colnames(df))]
        gcm.meta <- subset(meta, subset = (var == 'tas') & (project_id == 'CMIP5'))
        id <- as.integer(gcm.meta$gcm)
       rgbcolsa <- c('rgba(45,51,38)', 'rgba(87,77,102)', 'rgba(255,191,200)', 'rgba(140,129,105)', 'rgba(234,191,255)', 'rgba(172,230,195)', 'rgba(86,105,115)', 'rgba(115,86,94)', 'rgba(230,195,172)', 'rgba(255,234,191)', 'rgba(124,140,105)', 'rgba(51,26,43)', 'rgba(191,96,172)', 'rgba(184,204,102)', 'rgba(153,87,77)', 'rgba(96,134,191)', 'rgba(230,115,145)', 'rgba(255,145,128)', 'rgba(229,161,115)', 'rgba(22,58,89)', 'rgba(85,89,22)', 'rgba(127,83,32)', 'rgba(80,179,45)', 'rgba(18,51,13)', 'rgba(64,16,22)', 'rgba(22,16,64)', 'rgba(86,29,115)', 'rgba(54,98,217)', 'rgba(255,191,64)', 'rgba(61,182,242)', 'rgba(126,57,230)', 'rgba(51,38,13)', 'rgba(178,0,95)', 'rgba(0,128,85)', 'rgba(26,0,191)', 'rgba(255,0,238)', 'rgba(178,0,0)', 'rgba(0,202,217)', 'rgba(0,230,153)', 'rgba(0,255,34)', 'rgba(204,0,54)', 'rgba(102,0,14)', 'rgba(229,92,0)', 'rgba(0,107,115)', 'rgba(77,0,51)', 'rgba(204,255,0)', 'rgba(140,112,0)', 'rgba(12,89,0)')
        
         rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)', 'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        ## group by gcms
        colsa <- rgbcolsa[id]
        cols <- rgbcols[id]
        ## Add all models
        
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          leg.name <- paste(as.character(as.matrix(gcm.meta[i,c('gcm','gcm_rip')])),collapse = '  ')
          grp.name <- paste('Group',id[i],sep='')
          if (input$legend.sc == 'Display All')
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                  name = leg.name, mode = 'lines', 
                                  showlegend = TRUE, legendgroup = grp.name, colors = colsa[",i,"],
                                  line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          else if (input$legend.sc == 'Hide All')
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                  name = leg.name, mode = 'lines', 
                                  showlegend = TRUE, legendgroup = grp.name, colors = colsa[",i,"],
                                  line = list(color = colsa[",i,"], width = 2, shape ='spline',showlegend = FALSE))",sep='')))
        }
        
        ## Highlight selected models in tab:models
        if (!is.null(input$rows2)) {
          im <- input$rows2+1
          for (i in im) {
            leg.name <- paste(as.character(as.matrix(gcm.meta[i,c('gcm','gcm_rip')])),collapse = ' ')
            grp.name <- paste('Group',id[i],sep='')
            gcm <- gcms[i]
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                  name = leg.name, mode = 'lines', 
                                  showlegend = TRUE, colors = cols[",i,"],
                                  line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          }
        }
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'Reference', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        ## Format the layout
        p.sc <- p.sc %>% layout(title = "Precipitation",
                                paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                                xaxis = list(title = "Months",
                                             gridcolor = 'rgb(255,255,255)',
                                             showgrid = TRUE,
                                             showline = FALSE,
                                             showticklabels = TRUE,
                                             tickcolor = 'rgb(127,127,127)',
                                             ticks = 'outside',
                                             zeroline = FALSE),
                                yaxis = list(title = "Regional average in mm/day",
                                             gridcolor = 'rgb(255,255,255)',
                                             showgrid = TRUE,
                                             showline = FALSE,
                                             showticklabels = TRUE,
                                             tickcolor = 'rgb(127,127,127)',
                                             ticks = 'outside',
                                             zeroline = FALSE))
        
      } else if (grepl('ensemble', tolower(input$type))) { # Make an enveloppe instead of lines
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = FALSE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',
                    fill = 'tonexty', fillcolor='rgba(135,206,250,0.2)', line = list(color = 'transparent'),
                    showlegend = FALSE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',
                    line = list(color='rgb(135,206,250)'),
                    name = 'Average') 
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'Reference', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('box',tolower(input$type))) {
        p.sc <- plot_ly(df, type = 'box')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  marker = list(color = 'rgb(135,206,250'),
                                  line = list(color = 'rgb(135,206,250'),
                                  name = leg.name,showlegend =FALSE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name, showlegend = FALSE, 
                                       line = list(color = 'black', dash = 'dash', width = 2))
        }
      }
      
      # Format layout 
      p.sc <- p.sc %>% layout(title = "Regional average in mm/day",
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = "Precipitation (mm/day)",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      
      if (input$legend.sc == 'Hide All')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      
      p.sc$elementId <- NULL
      p.sc
    })
    
    output$gcm.sc.tas.data <- DT::renderDataTable({
      browser()
      df.format <- t(round(gcm.sc.tas.reactive(),digits = 1))
      colnames(df.format) <- month.abb
      df.format <- data.frame(N = c(1:(dim(df.format)[1]-1),0), 
                              Model = c(as.character(as.vector(cm.meta$gcm)),'ERA'), 
                              Run = c(as.character(as.vector(cm.meta$gcm_rip)),'ERA'),
                              df.format,stringsAsFactors = FALSE)
      DT::datatable(df.format,
                    caption = paste('Monthly estimates of regional temperature assuming an 
                              intermediate emission scenarios for the',tolower(input$period),'averaged over',input$region,'region.
                                    The climate models and their corresponding runs are listed in the second column and third columns, respectively. 
                                    The last row in the table shows the estimated values from the referance data set (Observation).',
                                    sep= ' '), 
                    selection = list(mode = 'multiple',target = 'row'), 
                    callback = JS("table.on('click.dt', function() {
                                   table.select.style( 'os' );                                  
                                  $(this).toggleClass('selected');                               
                                  var rowData = table.rows('.selected',0).indexes();
                                  var rowidx = table.cells( rowData, 0 ).data().toArray();                               
                                  Shiny.onInputChange('rows4',rowidx);
                    });"),
                    extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller','Select'), #'Responsive',
                    rownames=FALSE,
                    options=list(dom = 'Bfrtip',
                                 buttons = c('colvis',
                                             'selectAll','selectNone',
                                             'copy', 'csv','excel', 'print'),
                                 searching = T,
                                 pageLength = 30,
                                 searchHighlight = FALSE,
                                 colReorder = TRUE,
                                 fixedHeader = FALSE,
                                 filter = 'top',
                                 paging    = TRUE,
                                 deferRender = TRUE,
                                 scroller = TRUE,
                                 scrollX = TRUE, 
                                 scrollY = 800,
                                 select.style = 'os'
                    )
      )
    })
    
    output$gcm.sc.pr.data <- DT::renderDataTable({
      df.format <- t(round(gcm.sc.pr.reactive(),digits = 1))
      colnames(df.format) <- month.abb
      df.format <- data.frame(N = c(1:(dim(df.format)[1]-1),0), 
                              Model = c(as.character(as.vector(cm.meta$gcm)),'ERA'), 
                              Run = c(as.character(as.vector(cm.meta$gcm_rip)),'ERA'),
                              df.format,stringsAsFactors = FALSE)
      DT::datatable(caption = paste('Monthly estimates of regional temperature assuming an 
                              intermediate emission scenarios for the',tolower(input$period),'averaged over',input$region,'region.
                                    The climate models and their corresponding runs are listed in the second column and third columns, respectively. 
                                    The last row in the table shows the estimated values from the referance data set (Observation).',
                                    sep= ' '), 
                    df.format,
                    selection = list(mode = 'multiple', target = 'row'), 
                    callback = JS("table.on('click.dt', function() {
                                   table.select.style( 'os' );                                  
                                  $(this).toggleClass('selected');                               
                                  var rowData = table.rows('.selected',0).indexes();
                                  var rowidx = table.cells( rowData, 0 ).data().toArray();                               
                                  Shiny.onInputChange('rows4',rowidx);
                    });"),
                    extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller','Select'), #'Responsive',
                    rownames=FALSE,
                    options=list(dom = 'Bfrtip',
                                 buttons = c('colvis',
                                             'selectAll','selectNone',
                                             'copy', 'csv','excel', 'print'),
                                 searching = T,
                                 pageLength = 30,
                                 searchHighlight = FALSE,
                                 colReorder = TRUE,
                                 fixedHeader = FALSE,
                                 filter = 'top',
                                 paging    = TRUE,
                                 deferRender = TRUE,
                                 scroller = TRUE,
                                 scrollX = TRUE, 
                                 scrollY = 800,
                                 select.style = 'os'
                    )
      )
    })
    
    ## RCMs tablets 
    output$rcm.meta <- DT::renderDataTable({
      data(package= 'DECM', metaextract)
      META <- meta[,c('project_id','gcm','gcm_rip','rcm','longname','var','unit','frequency',
                      'resolution','lon','lon_unit','lat','lat_unit',"experiment_id",'dim','dates','url','filename',"creation_date")]
      cm.meta <- subset(META,subset= project_id == 'CORDEX')
      
      DT::datatable(caption = "Meta data of regional climate models' ensemble.",
                    cm.meta,
                    selection = 'multiple', 
                    callback = JS("table.on('click.dt', function() {
                            $(this).toggleClass('selected');
                            Shiny.onInputChange('rows2',table.rows('.selected').indexes().toArray());
                    });"),
                    extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller','Select'),
                    rownames=FALSE,
                    options=list(dom = 'Bfrtip',
                                 buttons = c('colvis',
                                             'selectAll','selectNone',
                                             'copy', 'csv','excel', 'print'),
                                 searching = T,
                                 pageLength = 30,
                                 searchHighlight = FALSE,
                                 colReorder = TRUE,
                                 fixedHeader = FALSE,
                                 filter = 'top',
                                 paging    = TRUE,
                                 deferRender = TRUE,
                                 scroller = TRUE,
                                 scrollX = TRUE,
                                 select.style = 'os',
                                 scrollY = 800
                    ))
    })
    
    ## --- Weather Stations menuitem output ---
    ### Metadata tab
    
    
    output$station.meta <- DT::renderDataTable(server = TRUE, {
      
      DT::datatable(as.data.frame(sta.meta(),stringsAsFactors = FALSE),
                    selection = list(mode = 'multiple',target = 'row'), 
                    callback = JS("table.on('click.dt', function() {
                                   table.select.style( 'os' );                                  
                                  $(this).toggleClass('selected');                               
                                  var rowData = table.rows('.selected',0).indexes();
                                  var rowidx = table.cells( rowData, 0 ).data().toArray();                               
                                  Shiny.onInputChange('rows4',rowidx);
                    });"),
                    extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller','Select'), #'Responsive',
                    rownames=FALSE,
                    options=list(dom = 'Bfrtip',
                                 buttons = c('colvis',
                                             'selectAll','selectNone',
                                             'copy', 'csv','excel', 'print'),
                                 searching = T,
                                 pageLength = 30,
                                 searchHighlight = FALSE,
                                 colReorder = TRUE,
                                 fixedHeader = FALSE,
                                 filter = 'top',
                                 paging    = TRUE,
                                 deferRender = TRUE,
                                 scroller = TRUE,
                                 scrollX = TRUE, #responsive = TRUE,
                                 scrollY = 800,
                                 select.style = 'os'
                    )
      )
    })
    
    ### Map the stations
    output$station.map <- renderLeaflet({
      sta.meta <- sta.meta()
      # Lon <- as.numeric(levels(factor(round(sta.meta$longitude,digits = 2))))
      # Lat <- as.numeric(levels(factor(round(sta.meta$latitude,digits = 2))))
      # Alt <- as.numeric(levels(factor(sta.meta$altitude)))
      # content <- paste(sep = "<br/>",
      #                  tags$strong(HTML(toupper(sta.meta$location))),
      #                  tags$strong(HTML(paste('LON ',Lon,'W',sep=''), paste(' LAT ',Lat,'N',sep=''), paste(' ALT ',Alt,'m',sep=''))), tags$br(),
      #                sprintf("Station ID: %s", as.numeric(sta.meta$station_id)),
      #                  sprintf("Parameter: %s", paste(toupper(sta.meta$variable),collapse = ',')),
      #                  sprintf("Start year: %s", paste(sta.meta$start),collapse = ','),
      #                  sprintf("End year: %s", paste(sta.meta$end),collapse = ','),
      #                  sprintf("Data provider: %s", paste(sta.meta$source,collapse = ',')))
      # 
      p <- leaflet() %>% 
        addCircleMarkers(lng = sta.meta$longitude,
                         lat = sta.meta$latitude,fill = TRUE,
                         labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                         popup = toupper(sta.meta$location),popupOptions(keepInView = TRUE),
                         radius =3,stroke=TRUE,weight = 1, color='black',
                         layerId = sta.meta$station_id,
                         fillOpacity = 0.4) %>% 
        addProviderTiles(providers$Esri.WorldStreetMap,
                         #addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        #setView(lat=60,lng = 10, zoom = 5)
        setView(lat=55,lng = 20, zoom = 3)
      
      if (!is.null(input$rows4)) {
        selSta <- sta.meta[is.element(sta.meta$station_id,input$rows4),]
        print(input$rows4)
        p <- p %>% addCircleMarkers(lng = selSta$longitude,
                                    lat = selSta$latitude,fill = TRUE,
                                    labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                                    popup = toupper(selSta$location),popupOptions(keepInView = TRUE),
                                    radius =4,stroke=TRUE,weight = 1, color='red',
                                    layerId = selSta$station_id,
                                    fillOpacity = 0.4)
      }
      p 
    })
    
    # station time series plot output
    output$station.ts <- renderPlotly({
      sta.obs <- attr(loc.reactive(),'station')
      df.obs <- data.frame(Date = as.character(year(sta.obs)), Value = round(sta.obs,digits = 2))
      ps <- plot_ly(df.obs, x= ~Date, y = ~ Value , name = 'Observations', mode= 'lines+markers',type= 'scatter')
      ps$elementId <- NULL
      ps
    })
    # station time series table output
    output$station.data <- DT::renderDataTable(server = TRUE, {
      sta.obs <- attr(loc.reactive(),'station')
      df.obs <- data.frame(Date = year(sta.obs), Value = round(coredata(sta.obs),digits = 2),stringsAsFactors = FALSE)
      DT::datatable(df.obs,
                    selection = 'multiple', 
                    callback = JS("table.on('click.dt', function() {
                            $(this).toggleClass('selected');
                            Shiny.onInputChange('rows2',table.rows('.selected').indexes().toArray());
                    });"),
                    extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller','Select'),
                    rownames=FALSE,
                    options=list(dom = 'Bfrtip',
                                 buttons = c('colvis',
                                             'selectAll','selectNone',
                                             'copy', 'csv','excel', 'print'),
                                 searching = T,
                                 pageLength = 30,
                                 searchHighlight = FALSE,
                                 colReorder = TRUE,
                                 fixedHeader = FALSE,
                                 filter = 'top',
                                 paging    = TRUE,
                                 deferRender = TRUE,
                                 scroller = TRUE,
                                 scrollX = TRUE,
                                 select.style = 'os',
                                 scrollY = 800
                    ))
    })
    
    output$rea.table <- DT::renderDataTable({
      data(package= 'DECM', metaextract)
      META <- meta[,c('project_id','gcm','gcm_rip','rcm','longname','var','unit','frequency',
                      'resolution','lon','lon_unit','lat','lat_unit',"experiment_id",'dim','dates','url','filename',"creation_date")]
      cm.meta <- subset(META,subset= project_id == 'CORDEX')
      DT::datatable(cm.meta,
                    selection = 'multiple', 
                    callback = JS("table.on('click.dt', function() {
                            $(this).toggleClass('selected');
                            Shiny.onInputChange('rows5',table.rows('.selected').indexes().toArray());
                    });"),
                    extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller','Responsive','Select'),
                    rownames=FALSE,
                    options=list(dom = 'Bfrtip',
                                 buttons = c('colvis',
                                             'selectAll','selectNone',
                                             'copy', 'csv','excel', 'print'),
                                 searching = T,
                                 pageLength = 30,
                                 searchHighlight = FALSE,
                                 colReorder = TRUE,
                                 fixedHeader = FALSE,
                                 filter = 'top',
                                 paging    = TRUE,
                                 deferRender = TRUE,
                                 scroller = TRUE,
                                 scrollX = TRUE,
                                 responsive = TRUE#,
                                 #scrollY = 700
                    ))
    })
    
    # output$scatter.cc <- renderTable({
    #   data.frame(Date = as.character(index(z.reactive())), Value = coredata(z.reactive()))
    # })
    # print(input$rows2)
    ##### Scatter plot server
    
    output$tgcm <- DT::renderDataTable({
      DT::datatable(data.frame(Models=models.45),  
                    selection = 'multiple', 
                    callback = JS("table.on('click.dt', function() {
                            $(this).toggleClass('selected');
                            Shiny.onInputChange('rows',table.rows('.selected').data().toArray());
                    });"),
                    extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller'),
                    rownames=FALSE,
                    options=list(dom = 'Bfrtip',
                                 searching = T,
                                 pageLength = 25,
                                 searchHighlight = FALSE,
                                 colReorder = TRUE,
                                 fixedHeader = TRUE,
                                 filter = 'top',
                                 buttons = c('copy', 'csv','excel', 'print'),
                                 paging    = TRUE,
                                 deferRender = TRUE,
                                 scroller = TRUE,
                                 scrollX = TRUE,
                                 scrollY = 800
                    ))
    }) #options = list(pageLength=20)})
    
    output$shopcart <- renderInfoBox({
      infoBox(
        "Shopping box",
        input$rows,color = 'aqua',
        icon = icon("credit-card")
      )
    })  
    
    output$messageMenu <- renderMenu({
      msgs <- apply(msgData(), 1, function(row) {
        messageItem(from = row[["from"]], message = row[["message"]])
      })
      
      ntfs <- apply(notData(), 1, function(row) {
        notificationItem(text = row[["message"]], status = row[["status"]],
                         icon = icon("shopping-cart", lib = "glyphicon"))
      })
      
      dropdownMenu(type = "messages", .list = msgs)
      dropdownMenu(type = "notifications", .list = ntfs, icon = icon("shopping-cart", lib = "glyphicon"))
      
    })
    
    output$scatter.cc <- renderPlotly({
      im <- as.numeric(gsub(":.*","",input$gcms))
      dcc <- dtdp.reactive() 
      #d <- data.frame(var1 = dcc$t2m$djf$rcp45$nf, var2 = dcc$mu$djf$rcp45$nf)
      # colnames(d) <- c('Temperature Change','Precipitation Change')
      #DF.cc$season <- as.factor(DF.cc[,3])
      #DF.cc$period <- as.integer(factor(DF.cc$period))
      p <- plot_ly()
      
      # filter by 
      selR <- switch(tolower(input$rcp.cc),'intermediate' = 'rcp45','low' = 'rcp26', 'high' = 'rcp85')
      selS <- switch(tolower(input$season.cc),'winter' = 'djf','spring' = 'mam', 'autumn' = 'son','summer' = 'jja')
      selP <- switch(input$period.cc,'Near Future (2021-2050)' = 'nf','Far future (2071-2100)' = 'ff')
      # 
      selData <- DF.cc %>% filter(season == selS, rcp == selR , period == selP)
      
      # plot
      p <- p %>% add_trace(data = selData, name = 'All',x = ~t2m, y = ~mu, type = "scatter",
                           mode = "markers", colors = I('blue'), 
                           marker = list(size = 14, symbol = 'circles', color = 'blue', line = list(width = 1,color = I('blue')),opacity = 0.2), 
                           text = ~paste("<br> Model: ", selData[,6], "</br> Score:", 2)) 
      
      # if (length(input$rows)>0) {
      #   
      #   p <- p %>% add_trace(data = selData %>% filter(model == input$rows), name = 'Changes',x = ~t2m, y = ~mu, type = "scatter",
      #                        mode = "markers", colors = I('blue'), 
      #                        marker = list(size = 14, symbol = 'circles', color = 'blue', line = list(width = 1,color = I('blue')),opacity = 0.2), 
      #                        text = ~paste("<br> Model: ", selData[,6], "</br> Score:", 2))
      # }
      # 
      de <- dataEllipse(x = as.matrix(selData[,1:2]),levels = 0.90) # 0.05,0.1,0.25,0.5,0.75,0.9,
      dev.off()
      
      if (input$ci == 'Display') {
        p <- p %>% add_text(x = c(-2,2,-2,2) , y = c(0.5,0.5,-0.5,-0.5), 
                            text = c('Wet & Cold','Wet & Warm','Dry & Cold', 'Dry & Warm'))
        p <- p %>% add_polygons(x=de[,1],y=de[,2],opacity = 0.2,name = "Confidence Interval", color = I('blue'))
      }
      # add ens. means
      # p <- p %>% add_trace(x = mean(subset(DF.cc,subset = (rcp=='rcp45') & (period == '1') & (season=='djf'))[,1],na.rm = TRUE), 
      #              y = mean(subset(DF.cc,subset = (rcp=='rcp45') & (period == '1') & (season=='djf'))[,2],na.rm = TRUE), mode = 'markers', 
      #              colors = 'blue',symbols = 'circles',sizes = c(20),
      #              # Hover text:
      #              text = ~paste("<br> Ens. Mean '</br> Score:', 2")) 
      # 
      # p <- p %>% add_trace(x = mean(subset(DF.cc,subset = (rcp=='rcp45') & (period == '2') & (season=='djf'))[,1],na.rm = TRUE), 
      #                      y = mean(subset(DF.cc,subset = (rcp=='rcp45') & (period == '2') & (season=='djf'))[,2],na.rm = TRUE), type = 'scatter', 
      #                      colors = c('red'),
      #                      symbols = c('x'),
      #                      sizes = c(30),
      #                      # Hover text:
      #                      text = ~paste("<br> Ens. Mean '</br> Score:', 2"), mode = 'markers') 
      
      
      if (length(input$rows) > 0) { # replaced selModel
        ## 
        selData2 <- subset(selData,subset = is.element(models.45,input$rows))
        p <- p %>% add_trace(data = selData2, name = 'Selected',x = ~t2m, y = ~mu, type = "scatter",
                             mode = "markers", colors = I('black'),
                             marker = list(size = 12, symbol = '+', line = list(width = 2,color = I('red')),opacity = 0.6), 
                             text = ~paste("<br> Model: ", selData2[,6], "</br> Score:", 2)) %>% 
          add_text(data = selData2, x = ~t2m, y = ~mu, text = ~model,name = 'Model name', 
                   textfont = list(family = "sans serif", size = 14,color = toRGB("black")), textposition = "top")
        if (length(input$rows)>3) {
          de2 <- dataEllipse(x = as.matrix(selData2[,1:2]),levels = 0.90) # 0.05,0.1,0.25,0.5,0.75,0.9,
          dev.off()
          
          if (input$ci == 'Display') {
            p <- p %>% add_text(x = c(-2,2,-2,2) , y = c(0.5,0.5,-0.5,-0.5), 
                                text = c('Wet & Cold','Wet & Warm','Dry & Cold', 'Dry & Warm'))
            p <- p %>% add_polygons(x=de2[,1],y=de2[,2],opacity = 0.2,name = "Confidence Interval", color = I('orange'))
          }
          
        }
      }
      p <- p %>%
        layout(title = "Climate Change signal realtive to the base period",
               xaxis = list(dtick = 0.5, title = "Temperature Change [deg. C]",zerolinewidth = 1),
               yaxis = list(dtick = 0.05, title = "Precipitation Change [%]",zerolinewidth = 1),
               paper_bgcolor = 'rgb(243, 243, 243)',
               plot_bgcolor = 'rgb(243, 243, 243)')  
      p$elementId <- NULL
      
      p  
      
      # scatterplot(dtas,dpr*(60*60*24),ix=NULL,xlim=input$tlim,ylim=input$plim,
      #             xlab="Temperature change (deg C)",ylab="Precipitation change (mm/day)",
      #             main=paste("Climate change assuming RCP4.5\npresent day (1981-2010) to",input$period),
      #             show.legend=FALSE,im=im,
      #             legend=seq(length(dtas)),pal=NULL,#pal="cat",pch=21,
      #             pch=as.character(seq(length(dtas))),cex=1.5,lwd=1.5,new=FALSE)
    })
    
    output$scatter.cc2 <- renderPlotly({
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
  
  observe(priority = -1,{
    showNotification(paste("Selected models are : ",paste(input$rows,collapse = '/')),type = 'message')
    updateTabsetPanel(session, "tabs", 'score5.tabs')
  }) 
}




