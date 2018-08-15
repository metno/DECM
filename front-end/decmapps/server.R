# define the back-end

function(input, output,session) {
  
  ## Messages / Notifications / Tasks
  notData <- reactive({
    gcm.meta <- gcm.meta.tas.reactive()
    if (!is.null(input$rowsGcm)) {
      newmodel <- apply(gcm.meta[input$rowsGcm,c('model_id','parent_experiment_rip','realization')],1,FUN='paste',collapse = '-')
    } else 
      newmodel <- 'No_model_selected'
    return(rbind(ntfs, data.frame(message= unlist(strsplit(paste('GCM:',newmodel,sep=''),split = ' ')),status = 'danger')))
  })
  
  msgData <- reactive({
    return(msgs)
  })
  
  data(Oslo)
  
  # Model explorer
  data("metaextract")
  M <- data.frame(list(Project=meta$project_id,
                       Experiment=meta$experiment_id,
                       GCM=meta$gcm,
                       RIP=meta$gcm_rip, 
                       RCM=meta$rcm, 
                       VAR=meta$var,
                       Unit=meta$unit, 
                       Resolution=paste(meta$resolution,"deg / monthly"),
                       Domain=paste(gsub(","," - ",meta$lon),"E"," / ",paste(gsub(","," - ",meta$lat)),"N",sep=""), 
                       Years=gsub(",","-",gsub("-[0-9]{2}","",meta$dates)), 
                       URL=meta$url))
  
  filterSim <- function(project,exp,gcm,run,rcm,var,dates,url) {
    #
    if (project != "none")
      M <- subset(M,subset = M$Project==project)
    if (input$exp != "none") 
      M <- subset(M,subset = M$Experiment == exp)
    if (input$gcm != "none")
      M <- subset(M,subset = M$GCM == gcm)
    if (input$run != "none")
      M <- subset(M,subset = M$RIP == run)
    if (input$rcm != "none") 
      M <- subset(M, M$RCM == rcm)
    if (input$var != "none") 
      M <- subset(M,subset = M$VAR == var)
    if (input$dates != "none")
      M <- subset(M,subset = M$Years == dates)
    if (input$url != "none")
      M <- subset(M,subset = M$URL==url)
    return(M)
  }
  
  filter.sim <- reactive({
    return(filterSim(input$project,input$exp,input$gcm,input$run,input$rcm,input$var,input$dates,input$url))
  })
  
  observe({
    output$browser <- DT::renderDataTable({
      #
      x <- filter.sim()
      dat <- x %>% mutate(URL = sprintf(paste0("<a href='", URL,"' target='_blank'>Get data</a>")))
      DT::datatable(dat,escape = FALSE)#,selection = 'single',options = list(), style="bootstrap")
    })
  })
  
  output$glossary <- DT::renderDataTable({
    DT::datatable(
      rbind(c('Project','Name of the project that run the simulations'),
            c('GCM','Global Climate Model or General Circulation Model'),
            c('RIP','Realisation, initialisation method, and physics version of a GCM'),
            c('RCM','Regional Climate Model'),
            c('tas','Near-surface air temperature'),
            c('pr','Precipitation'),
            c('Resolution','A rough estimate of the horizontal resolution'),
            c('CMIP5','Climate Model Intercomparison Program - Phase 5'),
            c('CORDEX','Coordinated Regional Climate Downscaling Experiment')),
      colnames = c('Abbreviation','Description'),options = list(dom = 't'), 
      caption='Glossary and variable names',style="bootstrap")
  })
  
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
  gcm.sc.vals <- function(param,region,period,stat){
    ## 
    if (param == 'tas')
      gcms <- names(stats$tas$ff)
    else if (param == 'pr')
      gcms <- names(stats$pr$ff)
    
    period <- switch(tolower(as.character(period)),
                     "present (1981-2010)"='present',
                     "far future (2071-2100)"='ff',
                     "near future (2021-2050)"='nf')
    
    stat <- switch(tolower(as.character(stat)),
                   "root mean square" = 'rms',
                   "error" = 'e',
                   "standard deviation"='spatial.sd',
                   "mean"="mean",
                   "spatial correlation"='corr')
    
    ref <- NULL
    if(tolower(region)=="global") {
      region <- "global"
    } else {
      i.srex <- which(srex$name==region)
      region <- srex$label[i.srex]
    }
    x <- lapply(gcms, function(gcm) stats[[param]][[period]][[gcm]][[region]][[stat]][2:13])
    
    #if(period=="present") {
    id.ref <- grep('era',names(stats[[param]][['present']])) 
    if (is.element(stat,c('mean','spatial.sd')))
      ref <- stats[[param]][['present']][[id.ref]][[region]][[stat]][2:13]
    else if (stat == 'corr') {
      ref <- rep(1,12)
    } else if (stat == 'rms' | stat == 'e') {
      ref <- rep(0,12)
    } 
    #} else 
    #  ref <- NULL #rep(NA,12)
    
    if((param=="pr") & (is.element(stat,c('mean','e','spatial.sd','rms')))) {      
      #      
      x <- lapply(x, function(y) y*60*60*24 * 30) ## convert to mm/day
      ref <- ref*1E3 * 30## convert to mm/day
    }
    
    gcm.vals <- as.data.frame(lapply(1:length(x),function(i) {if (length(x[[i]]) > 0) x[[i]] else x[[1]]})) ## AM Need to fix this later on     
    colnames(gcm.vals) <- paste('gcm.',1:length(gcm.vals),sep='')
    
    # common models to all climate variables
    if (input$gcm.var != 'Individual') {
      if (param=="pr") 
        gcm.vals <- base::subset(gcm.vals,select = id.pr)
      else if (param=="tas")
        gcm.vals <- base::subset(gcm.vals,select = id.tas)
    }
    
    if (input$gcm.sim.sc == 'Selected Simulations') {
      if (!is.null(input$rowsGcm))
        gcm.vals <- gcm.vals[,input$rowsGcm]
      else
        showNotification('Please select a model from the meta data table!')
    }
    
    if (!is.null(ref))
      df <- data.frame(gcm.vals,ref,stringsAsFactors = FALSE)
    else
      df <- data.frame(gcm.vals,stringsAsFactors = FALSE)
  }
  
  gcm.sc.tas.reactive <- reactive({
    return(gcm.sc.vals(param = 'tas',region = input$gcm.region, period = input$gcm.period,stat = input$gcm.stat))
  })
  
  gcm.sc.tas.reactive.pu <- reactive({
    return(gcm.sc.vals(param = 'tas',region = input$gcm.region.pu, period = input$gcm.period.pu,stat = input$gcm.stat.pu))
  })
  
  gcm.sc.tas.reactive.sc.pu <- reactive({
    return(gcm.sc.vals(param = 'tas',region = input$gcm.sc.region.pu, period = input$gcm.sc.period.pu,stat = input$gcm.sc.stat.pu))
  })
  
  gcm.sc.tas.reactive.cc.pu <- reactive({
    return(gcm.sc.vals(param = 'tas',region = input$gcm.cc.region, period = input$gcm.cc.period,stat = input$gcm.cc.stat))
  })
  
  gcm.sc.tas.present <- reactive({
    return(gcm.sc.vals(param = 'tas',region = input$gcm.region, period = 'Present (1981-2010)',stat = input$gcm.stat))
  })
  
  gcm.sc.tas.present.pu <- reactive({
    return(gcm.sc.vals(param = 'tas',region = input$gcm.region.pu, period = 'Present (1981-2010)',stat = input$gcm.stat.pu))
  })
  
  gcm.sc.tas.present.sc.pu <- reactive({
    return(gcm.sc.vals(param = 'tas',region = input$gcm.sc.region.pu, period = 'Present (1981-2010)',stat = input$gcm.sc.stat.pu))
  })
  
  gcm.sc.tas.present.cc.pu <- reactive({
    return(gcm.sc.vals(param = 'tas',region = input$gcm.cc.region, period = 'Present (1981-2010)',stat = input$gcm.cc.stat))
  })
  
  gcm.sc.pr.reactive <- reactive({
    return(gcm.sc.vals(param = 'pr',region = input$gcm.region,  period = input$gcm.period,stat = input$gcm.stat))
  })
  
  gcm.sc.pr.reactive.pu <- reactive({
    return(gcm.sc.vals(param = 'pr',region = input$gcm.region.pu,  period = input$gcm.period.pu,stat = input$gcm.stat.pu))
  })
  
  gcm.sc.pr.reactive.sc.pu <- reactive({
    return(gcm.sc.vals(param = 'pr',region = input$gcm.sc.region.pu,  period = input$gcm.sc.period.pu,stat = input$gcm.sc.stat.pu))
  })
  
  gcm.sc.pr.reactive.cc.pu <- reactive({
    return(gcm.sc.vals(param = 'pr',region = input$gcm.cc.region,  period = input$gcm.cc.period,stat = input$gcm.cc.stat))
  })
  
  gcm.sc.pr.present <- reactive({
    return(gcm.sc.vals(param = 'pr',region = input$gcm.region,  period = 'Present (1981-2010)',stat = input$gcm.stat))
  })
  
  gcm.sc.pr.present.pu <- reactive({
    return(gcm.sc.vals(param = 'pr',region = input$gcm.region.pu,  period = 'Present (1981-2010)',stat = input$gcm.stat.pu))
  })
  
  gcm.sc.pr.present.sc.pu <- reactive({
    return(gcm.sc.vals(param = 'pr',region = input$gcm.sc.region.pu,  period = 'Present (1981-2010)',stat = input$gcm.sc.stat.pu))
  })
  
  gcm.sc.pr.present.cc.pu <- reactive({
    return(gcm.sc.vals(param = 'pr',region = input$gcm.cc.region,  period = 'Present (1981-2010)',stat = input$gcm.cc.stat))
  })
  
  ## RCMs 
  rcm.sc.vals <- function(param,region,period,stat) {
    stats <- rcms
    if (param == 'tas')
      rcms <- names(stats$tas$ff)
    else if (param == 'pr')
      rcms <- names(stats$pr$ff)
    
    period <- switch(tolower(as.character(period)),
                     "present (1981-2010)"='present',
                     "far future (2071-2100)"='ff',
                     "near future (2021-2050)"='nf')
    
    stat <- switch(tolower(as.character(stat)),
                   "root mean square" = 'rms',
                   "error" = 'e',
                   "standard deviation"='spatial.sd',
                   "mean"="mean",
                   "spatial correlation"='corr')
    
    # prudence.region <- c('Europe',
    #                      'BI - Brithish Isles',
    #                      'IP - Iberian Peninsula',
    #                      'FR - FRANCE',
    #                      'ME - Mid Europe',
    #                      'SC - Scandinavia',
    #                      'AL - ALPS',
    #                      'MD - Mediterranean',
    #                      'EA - Eastern Europe')
    
    ref <- NULL
    
    if(tolower(region)=="europe") {
      region <- "europe"
    } else if (!is.element('--',region)) {
      #i.prud <- which(prudence.region==region)
      region <- substr(region,1,3)
      region <- gsub(' ','',region)
    }
    x <- lapply(rcms, function(rcm) stats[[param]][[period]][[rcm]][[region]][[stat]][2:13])
    #browser()
    #if(period=="present") {
    id.ref <- grep('eobs',names(stats[[param]][['present']])) 
    if (is.element(stat,c('mean','spatial.sd')))
      ref <- stats[[param]][['present']][[id.ref]][[region]][[stat]][2:13]
    else if (stat == 'corr') {
      ref <- rep(1,12)
    } else if (stat == 'rms' | stat == 'e') {
      ref <- rep(0,12)
    } 
    #} else 
    #  ref <- rep(NA,12)
    
    if((param=="pr") & (is.element(stat,c('mean','e','spatial.sd','rms')))) {      
      #      
      x <- lapply(x, function(y) y*60*60*24 * 30) ## convert to mm/day
      #ref <- ref*1E3 * 30## convert to mm/day
    }
    
    rcm.vals <- as.data.frame(lapply(1:length(x),function(i) {if (length(x[[i]]) > 0) x[[i]] else x[[1]]})) ## AM Need to fix this later on     
    colnames(rcm.vals) <- paste('rcm.',1:length(rcm.vals),sep='')
    
    # # common models to all climate variables
    # if (input$rcm.var != 'Individual') {
    #   if (param=="pr") 
    #     rcm.vals <- base::subset(rcm.vals,select = id.pr)
    #   else if (param=="tas")
    #     rcm.vals <- base::subset(rcm.vals,select = id.tas)
    # }
    
    if (input$rcm.sim.sc == "Selected Simulations") {
      if (!is.null(input$rowsRcm))
        rcm.vals <- rcm.vals[,input$rowsRcm]
      else
        showNotification('Please select a model from the meta data table!')
    }
    #browser()
    if (!is.null(ref))
      df <- data.frame(rcm.vals,ref,stringsAsFactors = FALSE)
    else
      df <- data.frame(rcm.vals,stringsAsFactors = FALSE)
  }
  
  rcm.sc.tas.reactive <- reactive({
    return(rcm.sc.vals(param = 'tas',region = input$rcm.region, period = input$rcm.period,stat = input$rcm.stat))
  })
  
  rcm.sc.tas.reactive.sc.pu <- reactive({
    return(rcm.sc.vals(param = 'tas',region = input$rcm.sc.region.pu, period = input$rcm.sc.period.pu,stat = input$rcm.sc.stat.pu))
  })
  
  rcm.sc.tas.reactive.cc.pu <- reactive({
    return(rcm.sc.vals(param = 'tas',region = input$rcm.cc.region, period = input$rcm.cc.period,stat = input$rcm.cc.stat))
  })
  
  rcm.sc.tas.reactive.pu <- reactive({
    return(rcm.sc.vals(param = 'tas',region = input$rcm.region.pu, period = input$rcm.period.pu,stat = input$rcm.stat.pu))
  })
  
  rcm.sc.pr.reactive <- reactive({
    return(rcm.sc.vals(param = 'pr',region = input$rcm.region,  period = input$rcm.period,stat = input$rcm.stat))
  })
  
  rcm.sc.pr.reactive.pu <- reactive({
    return(rcm.sc.vals(param = 'pr',region = input$rcm.region.pu,  period = input$rcm.period.pu,stat = input$rcm.stat.pu))
  })
  
  rcm.sc.pr.reactive.sc.pu <- reactive({
    return(rcm.sc.vals(param = 'pr',region = input$rcm.sc.region.pu,  period = input$rcm.sc.period.pu,stat = input$rcm.sc.stat.pu))
  })
  
  rcm.sc.pr.reactive.cc.pu <- reactive({
    return(rcm.sc.vals(param = 'pr',region = input$rcm.cc.region,  period = input$rcm.cc.period,stat = input$rcm.cc.stat))
  })
  
  rcm.sc.tas.present <- reactive({
    return(rcm.sc.vals(param = 'tas',region = input$rcm.region,  period = 'Present (1981-2010)',stat = input$rcm.stat))
  })
  
  rcm.sc.tas.present.sc.pu <- reactive({
    return(rcm.sc.vals(param = 'tas',region = input$rcm.sc.region.pu,  period = 'Present (1981-2010)',stat = input$rcm.sc.stat.pu))
  })
  
  rcm.sc.tas.present.cc.pu <- reactive({
    return(rcm.sc.vals(param = 'tas',region = input$rcm.cc.region,  period = 'Present (1981-2010)',stat = input$rcm.cc.stat))
  })
  
  rcm.sc.tas.present.pu <- reactive({
    return(rcm.sc.vals(param = 'tas',region = input$rcm.region.pu,  period = 'Present (1981-2010)',stat = input$rcm.stat.pu))
  })
  
  rcm.sc.pr.present <- reactive({
    return(rcm.sc.vals(param = 'pr',region = input$rcm.region,  period = 'Present (1981-2010)',stat = input$rcm.stat))
  })
  
  rcm.sc.pr.present.pu <- reactive({
    return(rcm.sc.vals(param = 'pr',region = input$rcm.region.pu,  period = 'Present (1981-2010)',stat = input$rcm.stat.pu))
  })
  
  rcm.sc.pr.present.sc.pu <- reactive({
    return(rcm.sc.vals(param = 'pr',region = input$rcm.sc.region.pu,  period = 'Present (1981-2010)',stat = input$rcm.sc.stat.pu))
  })
  
  rcm.sc.pr.present.cc.pu <- reactive({
    return(rcm.sc.vals(param = 'pr',region = input$rcm.cc.region,  period = 'Present (1981-2010)',stat = input$rcm.cc.stat))
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
  
  gcm.meta.tas.reactive <- reactive({
    if (input$gcm.var == 'Individual'){
      return(gcm.meta.tas)
    } else 
      return(gcm.meta.all)
  })
  
  gcm.meta.tas.reactive.pu <- reactive({
    if (input$gcm.var.pu == 'Individual'){
      return(gcm.meta.tas)
    } else 
      return(gcm.meta.all)
  })
  
  gcm.meta.tas.reactive.sc.pu <- reactive({
    if (input$gcm.sc.var.pu == 'Individual'){
      return(gcm.meta.tas)
    } else 
      return(gcm.meta.all)
  })
  
  gcm.meta.tas.reactive.cc.pu <- reactive({
    if (input$gcm.cc.var == 'Individual'){
      return(gcm.meta.tas)
    } else 
      return(gcm.meta.all)
  })
  
  gcm.meta.pr.reactive <- reactive({
    if (input$gcm.var == 'Individual'){
      return(gcm.meta.pr)
    } else 
      return(gcm.meta.all)
  })
  
  gcm.meta.pr.reactive.pu <- reactive({
    if (input$gcm.var.pu == 'Individual'){
      return(gcm.meta.pr)
    } else 
      return(gcm.meta.all)
  })
  
  rcm.meta.tas.reactive <- reactive({
    if (input$rcm.var == 'Individual'){
      return(rcm.meta.tas)
    } else 
      return(rcm.meta.all)
  })
  
  rcm.meta.tas.reactive.sc.pu <- reactive({
    if (input$rcm.sc.var.pu == 'Individual'){
      return(rcm.meta.tas)
    } else 
      return(rcm.meta.all)
  })
  
  rcm.meta.tas.reactive.cc.pu <- reactive({
    if (input$rcm.cc.var == 'Individual'){
      return(rcm.meta.tas)
    } else 
      return(rcm.meta.all)
  })
  
  rcm.meta.tas.reactive.pu <- reactive({
    if (input$rcm.var.pu == 'Individual'){
      return(rcm.meta.tas)
    } else 
      return(rcm.meta.all)
  })
  
  rcm.meta.pr.reactive <- reactive({
    
    if (input$rcm.var == 'Individual'){
      return(rcm.meta.pr)
    } else 
      return(rcm.meta.all)
  })
  
  rcm.meta.pr.reactive.sc.pu <- reactive({
    
    if (input$rcm.sc.var.pu == 'Individual'){
      return(rcm.meta.pr)
    } else 
      return(rcm.meta.all)
  })
  
  rcm.meta.pr.reactive.pu <- reactive({
    
    if (input$rcm.var.pu == 'Individual'){
      return(rcm.meta.pr)
    } else 
      return(rcm.meta.all)
  })
  
  txttab <- reactive({
    txt <- paste('Monthly estimates of regional temperature assuming an intermediate emission scenarios for the',tolower(input$gcm.period),'averaged over',input$gcm.region,'region. The climate models and their corresponding runs are listed in the second and third columns, respectively. The last row in the table shows the estimated values from the referance data set (Observation).', sep= ' ')
    return(txt)
  })
  
  prud.region <- function (m,region) {
    if (region == 'Europe') 
      m <- m %>% addPolygons(lng = c(-24.75,44.75,44.75,-24.75,-24.75), 
                             lat = c(27.75,27.75,72.25,72.25,27.75),
                             fill = TRUE, color = 'black',weight = 0.8,label = 'Europe') 
    else if (region == 'BI - British Isles')
      m <- m %>% addPolygons(lng = c(-10,2,2,-10,-10),
                             lat = c(50,50,59,59,50),  
                             fill = TRUE, color = 'black',weight = 0.8,label = 'Europe') 
    else if (region == 'IP - Iberian Peninsula')
      m <- m %>% addPolygons(lng = c(-10,3,3,-10,-10), 
                             lat = c(36,36,44,44,36), 
                             fill = TRUE, color = 'black',weight = 0.8,label = 'Europe') 
    else if (region == 'FR - FRANCE')
      m <- m %>% addPolygons(lng = c(-5,5,5,-5,-5), 
                             lat = c(44,44,50,50,44), 
                             fill = TRUE, color = 'black',weight = 0.8,label = 'Europe') 
    else if (region == 'ME - Mid Europe')
      m <- m %>% addPolygons(lng = c(2,16,16,2,2), 
                             lat = c(48,48,55,55,48), 
                             fill = TRUE, color = 'black',weight = 0.8,label = 'Europe') 
    else if (region == 'SC - Scandinavia')
      m <- m %>% addPolygons(lng = c(5,30,30,5,5), 
                             lat = c(55,55,70,70,55), 
                             fill = TRUE, color = 'black',weight = 0.8,label = 'Europe') 
    else if (region == 'AL - ALPS')
      m <- m %>% addPolygons(lng = c(5,15,15,5,5), 
                             lat = c(44,44,48,48,44), 
                             fill = TRUE, color = 'black',weight = 0.8,label = 'Europe') 
    else if (region == 'MD - Mediterranean')
      m <- m %>% addPolygons(lng = c(3,25,25,3,3), 
                             lat = c(36,36,44,44,36), 
                             fill = TRUE, color = 'black',weight = 0.8,label = 'Europe') 
    else if (region == 'EA - Eastern Europe')
      m <- m %>% addPolygons(lng = c(16,30,30,16,16), 
                             lat = c(44,44,54,54,44), 
                             fill = TRUE, color = 'black',weight = 0.8,label = 'Europe')
    else {
      selected_polygon <- subset(afg,afg$ISO3==substr(region,1,3))
      
      # polygon_labelPt <- selected_polygon@polygons[[1]]@labpt
      # #center the view on the polygon 
      # m <- m %>% setView(lng=polygon_labelPt[1],lat=polygon_labelPt[2],zoom=6)
      # 
      #remove any previously highlighted polygon
      m <- m %>% removeShape("highlighted_polygon")
      
      #add a slightly thicker red polygon on top of the selected one
      Sr1 <- Polygon(cbind(c(-24.75,44.75,44.75,-24.75,-24.75), 
                           c(27.75,27.75,72.25,72.25,27.75)))
      Srs1 = Polygons(list(Sr1), "s1")
      Srs2 = Polygons(list(Sr1), "s2")
      SpP = SpatialPolygons(list(Srs1,Srs2),1:2)
      projection(SpP) <- projection(selected_polygon)
      
      selected_polygon <- intersect(selected_polygon,SpP)
      polygon_labelPt <- selected_polygon@polygons[[1]]@labpt
      #center the view on the polygon 
      m <- m %>% setView(lng=polygon_labelPt[1],lat=polygon_labelPt[2],zoom=6)
     
      m <- m %>% addPolylines(fill = TRUE, stroke = TRUE, color = "red",weight = 1.5, opacity = 0.5,
                              data=selected_polygon,noClip = TRUE,
                              group="highlighted_polygon") 
      
    } 
    
    invisible(m)
  }
  
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
      
      low <- round(em - 1.96 * es,digits = 1)
      high <- round(em + 1.96 * es,digits = 1)
      avg <- round(em ,digits = 1)
      df.env <- data.frame(Date = as.character(year(sta)),low=low,avg = round(em,digits = 1),high=high)
      
      df.obs <- data.frame(Date = as.character(year(attr(sta,'station'))), Value = round(as.anomaly(attr(sta,'station')),digits = 2))
      
      # p <- plot_ly(df, x= ~as.character(year(sta)))
      
      p <- plot_ly(df.env, x = ~Date, y = ~high, type = 'scatter', mode = 'lines',
                   line = list(color = 'transparent',shape = 'spline'),
                   showlegend = FALSE, name = 'High') %>%
        add_trace(y = ~low, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255,127,80,0.2)', line = list(color = 'transparent',shape = 'spline'),
                  showlegend = FALSE, name = 'Low') %>%
        add_trace(x = ~Date, y = ~avg, type = 'scatter', mode = 'lines',
                  line = list(color='rgb(255,127,80)',shape = 'spline'),
                  name = 'Average') 
      p <- p %>% add_markers(data = df.obs, x = ~Date, y = ~ Value , name = 'Observations', 
                             marker = list(size = 10, color = 'rgba(204,204, 204, .8)',
                                           line = list(color = 'rgba(51, 51, 51, .9)',width = 1))) 
      
      
      
      # if (input$loess) {
      #   change <- fitted(loess(df$Value~as.numeric(df$Date)))
      #   p <- p %>% 
      #     add_trace(y = ~ change , type = 'scatter', name = 'Ens. Mean',mode = "lines", line = list(color = c('darkorange'), width = 4))
      #   p <- p %>% add_trace(p, y = fitted(loess(df$low~as.numeric(df$Date))) , type = 'scatter', 
      #                        name = 'Lower limit', mode = "lines", line = list(dash = 'dash',color = c('darkorange'), width = 4, shape ="spline")) %>%
      #     add_trace(p, x=~df$Date, y=~fitted(loess(df$high~as.numeric(df$Date))) , type = 'scatter', 
      #               mode = "lines", name = 'Upper limit',line = list(dash = 'dash',color = c('darkorange'), width = 4,shape = "spline")) 
      # } else {
      #   p <- p %>% 
      #     add_trace(y = ~ df$Value , type = 'scatter', name = 'Simulation',mode = "lines", line = list(color = c('darkorange'), width = 4))
      #   
      #   #df.err2 <- data.frame(Date = as.character(year(sta)), Value =  0.9 * round(attr(sta,'ci'),digits = 1))
      #   p <- p %>% add_trace(p, y = ~ df$low , type = 'scatter', 
      #                        name = 'Lower limit', mode = "lines", line = list(dash = 'dash',color = c('darkorange'), width = 4, shape ="spline")) %>%
      #     add_trace(p, x=~df$Date, y=~ df$high , type = 'scatter', 
      #               mode = "lines", name = 'Upper limit',line = list(dash = 'dash',color = c('darkorange'), width = 4,shape = "spline")) 
      # }
      if (length(input$rows.cc) >0) {
        for (i in 1:length(input$rows.cc)) {
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
      DT::datatable(data.frame(Names = gcmnames.45),
                    selection = 'multiple', 
                    callback = JS("table.on('click.dt', function() {
                                  $(this).toggleClass('selected');
                                  Shiny.onInputChange('rows.cc',table.rows('.selected').data().toArray());
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
                                  Shiny.onInputChange('rows.cc',table.rows('.selected').data().toArray());
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
    
    ## Global Climate Models Menu item ---      
    ## Metadata table
    ## data(package= 'DECM', metaextract)
    
    #
    ## library(dplyr)
    
    output$gcm.meta.tas <- DT::renderDataTable({
      gcm.meta.tas <- gcm.meta.tas.reactive()
      ## Metadata table
      #data(package= 'DECM', metaextract)
      DF.tas <- data.frame(N = cbind(1:dim(gcm.meta.tas)[1],gcm.meta.tas))
      colnames(DF.tas) <- c('N',colnames(gcm.meta.tas))
      DT::datatable(DF.tas, 
                    caption = 'Meta data for all simulations that combines attributes from IPCC.TABLE and from the data itself including institue names, experiment, etc.
                    The culumns visibily allow displaying or hiding by the user unecessary columns.',
                    selection = list(mode = 'multiple',target = 'row'),
                    callback = JS("table.on('click.dt', function() {
                                  table.select.style( 'os' );                                  
                                  $(this).toggleClass('selected');                               
                                  var rowData = table.rows('.selected',0).indexes();
                                  var rowidx = table.cells( rowData, 0 ).data().toArray();                               
                                  Shiny.onInputChange('rowsGcm',rowidx);
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
    
    output$gcm.meta.pr <- DT::renderDataTable({
      gcm.meta.pr <- gcm.meta.pr.reactive()
      ## Metadata table
      DF.pr <- data.frame(N = cbind(1:dim(gcm.meta.pr)[1],gcm.meta.pr))
      colnames(DF.pr) <- c('N',colnames(gcm.meta.pr))
      DT::datatable(DF.pr,
                    caption = 'Meta data for all simulations that combines attributes from IPCC.TABLE and from the data itself including institue names, experiment, etc.
                    The culumns visibily allow displaying or hiding by the user unecessary columns.',
                    selection = list(mode = 'multiple',target = 'row'),
                    callback = JS("table.on('click.dt', function() {
                                  table.select.style( 'os' );                                  
                                  $(this).toggleClass('selected');                               
                                  var rowData = table.rows('.selected',0).indexes();
                                  var rowidx = table.cells( rowData, 0 ).data().toArray();                               
                                  Shiny.onInputChange('rowsGcm',rowidx);
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
    
    output$gcm.meta.all <- DT::renderDataTable({
      ## Metadata table
      DF.pr <- data.frame(N = cbind(1:dim(gcm.meta.all)[1],gcm.meta.all))
      colnames(DF.pr) <- c('N',colnames(gcm.meta.all))
      DT::datatable(DF.pr,
                    selection = list(mode = 'multiple',target = 'row'),
                    callback = JS("table.on('click.dt', function() {
                                  $(this).toggleClass('selected');
                                  Shiny.onInputChange('rowsGcm',table.rows('.selected').indexes().toArray());
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
    
    ## RCMs tablets 
    
    output$rcm.meta.tas <- DT::renderDataTable({
      #data(package= 'DECM', metaextract)
      DF.tas <- data.frame(N = cbind(1:dim(rcm.meta.tas)[1],rcm.meta.tas))
      colnames(DF.tas) <- c('N',colnames(rcm.meta.tas))
      DT::datatable(DF.tas,
                    selection = list(mode = 'multiple',target = 'row'),
                    callback = JS("table.on('click.dt', function() {
                                  table.select.style( 'os' );                                  
                                  $(this).toggleClass('selected');                               
                                  var rowData = table.rows('.selected',0).indexes();
                                  var rowidx = table.cells( rowData, 0 ).data().toArray();                               
                                  Shiny.onInputChange('rowsRcm',rowidx);
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
                                 autoWidth = TRUE,
                                 #columnDefs = list(list(width = '50px', targets = "_all")),
                                 filter = 'top',
                                 paging    = TRUE,
                                 deferRender = TRUE,
                                 scroller = TRUE,
                                 scrollX = TRUE,
                                 select.style = 'os',
                                 scrollY = 800
                    ))
      })
    
    output$rcm.meta.pr <- DT::renderDataTable({
      ## Metadata table
      
      DF.pr <- data.frame(N = cbind(1:dim(rcm.meta.pr)[1],rcm.meta.pr))
      colnames(DF.pr) <- c('N',colnames(rcm.meta.pr))
      DT::datatable(DF.pr,
                    selection = 'multiple', 
                    callback = JS("table.on('click.dt', function() {
                                  table.select.style( 'os' );                                  
                                  $(this).toggleClass('selected');                               
                                  var rowData = table.rows('.selected',0).indexes();
                                  var rowidx = table.cells( rowData, 0 ).data().toArray();                               
                                  Shiny.onInputChange('rowsRcm',rowidx);
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
    
    output$rcm.meta.all <- DT::renderDataTable({
      ## Metadata table
      
      #data(package= 'DECM', metaextract)
      DF.com <- data.frame(N = cbind(1:dim(rcm.meta.all)[1],rcm.meta.all))
      colnames(DF.com) <- c('N',colnames(rcm.meta.all))
      DT::datatable(DF.com,
                    selection = 'multiple', 
                    callback = JS("table.on('click.dt', function() {
                                  table.select.style( 'os' );                                  
                                  $(this).toggleClass('selected');                               
                                  var rowData = table.rows('.selected',0).indexes();
                                  var rowidx = table.cells( rowData, 0 ).data().toArray();                               
                                  Shiny.onInputChange('rowsRcm',rowidx);
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
    
    ## Define reions
    output$gcm.region <- renderLeaflet({
      id <- which(is.element(region.names,input$gcm.region))
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap,
                         #addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = FALSE)) 
      if (input$gcm.region == 'Global') {
        m <- m %>% addPolygons(lng = c(-180,180,180,-180,-180), lat = c(-90,-90,90,90,-90), 
                               fill = TRUE, stroke = TRUE, color = 'red',weight = 3,label = 'Global') 
      } else {
        m <- m %>% addPolygons(lng = srex$coords[[id-1]][1,], lat = srex$coords[[id-1]][2,], 
                               fill = TRUE, stroke = TRUE, color = 'red',weight = 3,label = srex$name[id-1]) 
      }
      m
    })
    
    output$gcm.region.pu <- renderLeaflet({
      id <- which(is.element(region.names,input$gcm.region.pu))
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap,
                         #addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = FALSE)) 
      if (input$gcm.region.pu == 'Global') {
        m <- m %>% addPolygons(lng = c(-180,180,180,-180,-180), lat = c(-90,-90,90,90,-90), 
                               fill = TRUE, stroke = TRUE, color = 'red',weight = 3,label = 'Global') 
      } else {
        m <- m %>% addPolygons(lng = srex$coords[[id-1]][1,], lat = srex$coords[[id-1]][2,], 
                               fill = TRUE, stroke = TRUE, color = 'red',weight = 3,label = srex$name[id-1]) 
      }
      m
    })
    
    output$gcm.sc.region.pu <- renderLeaflet({
      id <- which(is.element(region.names,input$gcm.sc.region.pu))
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap,
                         #addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = FALSE)) 
      if (input$gcm.sc.region.pu == 'Global') {
        m <- m %>% addPolygons(lng = c(-180,180,180,-180,-180), lat = c(-90,-90,90,90,-90), 
                               fill = TRUE, stroke = TRUE, color = 'red',weight = 3,label = 'Global') 
      } else {
        m <- m %>% addPolygons(lng = srex$coords[[id-1]][1,], lat = srex$coords[[id-1]][2,], 
                               fill = TRUE, stroke = TRUE, color = 'red',weight = 3,label = srex$name[id-1]) 
      }
      m
    })
    
    output$gcm.bias.region.pu <- renderLeaflet({
      id <- which(is.element(region.names,input$gcm.bias.region.pu))
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap,
                         #addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = FALSE)) 
      if (input$gcm.bias.region.pu == 'Global') {
        m <- m %>% addPolygons(lng = c(-180,180,180,-180,-180), lat = c(-90,-90,90,90,-90), 
                               fill = TRUE, stroke = TRUE, color = 'red',weight = 3,label = 'Global') 
      } else {
        m <- m %>% addPolygons(lng = srex$coords[[id-1]][1,], lat = srex$coords[[id-1]][2,], 
                               fill = TRUE, stroke = TRUE, color = 'red',weight = 3,label = srex$name[id-1]) 
      }
      m
    })
    
    output$rcm.region <- renderLeaflet({
      # id <- which(is.element(region.names,input$rcm.region))
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap,
                         #addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) 
      m <- prud.region(m,region = input$rcm.region)
      m
    })
    
    output$rcm.region.pu <- renderLeaflet({
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap,
                         #addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) 
      m <- prud.region(m,region = input$rcm.region.pu)
      m
    })
    
    output$rcm.sc.region.pu <- renderLeaflet({
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap,
                         #addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) 
      m <- prud.region(m,region = input$rcm.sc.region.pu)
      m
    })
    
    output$rcm.bias.region.pu <- renderLeaflet({
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap,
                         #addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) 
      m <- prud.region(m,region = input$rcm.region.pu)
      m
    })
    
    ## Seasonal cycle 
    output$gcm.sc.tas <- renderPlotly({
      
      gcm.meta.tas <- gcm.meta.tas.reactive()
      df <- gcm.sc.tas.reactive()
      
      if (!is.null(input$rowsGcm))
        if (input$gcm.sim.sc == 'Selected Simulations')
          gcm.meta.tas <- gcm.meta.tas[input$rowsGcm,]
      
      if (input$gcm.outputValues == 'Bias')
        df <- df - df[,dim(df)[2]]      
      else if (input$gcm.outputValues == 'Anomaly') {
        DF <- t(df)
        df <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$gcm.outputValues == 'Change') {
        df <- gcm.sc.tas.reactive() - gcm.sc.tas.present()
      }
      
      #df <- df[,-36] # AM Quick fix but has to be removed ... once meta is updated.
      # GCM seasonal cycle
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      
      # define layout output
      #df$ref <- df$ref
      if (input$gcm.chart.type == 'Individual Simulations') {
        ## Make the plot
        #p.sc <- plot_ly(df, x = ~month, y = ~gcm.1,type = 'scatter',mode = 'markers+lines', line = list(width = 2, color = "grey",shape = 'spline'))
        # create plot_ly
        p.sc <- plot_ly(df, x = ~month)
        
        gcms <- colnames(df)[grep('gcm',colnames(df))]
        if (is.element(input$gcm.groupBy,c('None','---'))) {
          id <- 1 : (length(df) - 1)
          lev <- levels(factor(id))
        }
        else { 
          id <- as.integer(factor(base::subset(gcm.meta.tas, select = input$gcm.groupBy)[[1]]))
          lev <- levels(factor(base::subset(gcm.meta.tas, select = input$gcm.groupBy)[[1]]))
        }
        
        if (length(lev) > 50) {
          # 108 distinct colors
          rgbcols <- c('rgb(255,128,128)', 'rgb(178,89,89)', 'rgb(51,26,26)', 'rgb(217,123,108)', 'rgb(140,79,70)', 'rgb(89,51,45)', 'rgb(64,36,32)', 'rgb(242,153,121)', 'rgb(178,113,89)', 
                       'rgb(102,65,51)', 'rgb(76,48,38)', 'rgb(51,32,26)', 'rgb(217,152,108)', 'rgb(153,107,77)', 'rgb(102,71,51)', 'rgb(76,54,38)', 'rgb(229,176,115)', 'rgb(178,137,89)',
                       'rgb(140,108,70)', 'rgb(102,78,51)', 'rgb(51,39,26)', 'rgb(217,181,108)', 'rgb(140,117,70)', 'rgb(76,64,38)', 'rgb(178,161,89)', 'rgb(102,92,51)', 'rgb(51,46,26)', 
                       'rgb(229,222,115)', 'rgb(173,179,89)', 'rgb(74,77,38)', 'rgb(229,255,128)', 'rgb(126,140,70)', 'rgb(46,51,26)', 'rgb(170,204,102)', 'rgb(85,102,51)', 'rgb(186,242,121)', 
                       'rgb(127,166,83)', 'rgb(81,128,64)', 'rgb(116,204,102)', 'rgb(127,255,128)', 'rgb(83,166,94)', 'rgb(51,102,58)', 'rgb(38,77,43)', 'rgb(26,51,29)', 'rgb(115,230,161)', 
                       'rgb(77,153,107)', 'rgb(64,128,98)', 'rgb(45,89,68)', 'rgb(102,204,170)', 'rgb(26,51,43)', 'rgb(121,242,218)', 'rgb(83,166,149)', 'rgb(51,102,92)', 'rgb(38,77,69)',
                       'rgb(102,204,197)', 'rgb(121,234,242)', 'rgb(83,160,166)', 'rgb(57,111,115)', 'rgb(38,74,77)', 'rgb(26,49,51)', 'rgb(108,195,217)', 'rgb(77,138,153)', 'rgb(121,202,242)', 
                       'rgb(89,149,179)', 'rgb(70,117,140)', 'rgb(51,85,102)', 'rgb(32,53,64)', 'rgb(128,196,255)', 'rgb(102,156,204)', 'rgb(64,98,128)', 'rgb(128,179,255)', 'rgb(102,143,204)', 
                       'rgb(45,62,89)', 'rgb(26,36,51)', 'rgb(121,153,242)', 'rgb(77,97,153)', 'rgb(128,145,255)', 'rgb(89,101,179)', 'rgb(64,72,128)', 'rgb(45,51,89)', 'rgb(32,36,64)', 'rgb(108,108,217)', 			      'rgb(58,51,102)', 'rgb(162,128,255)', 'rgb(113,89,179)', 'rgb(89,70,140)', 'rgb(48,38,77)', 'rgb(39,26,51)', 'rgb(128,77,153)', 'rgb(74,45,89)', 'rgb(172,96,191)', 
                       'rgb(247,128,255)', 'rgb(111,57,115)', 'rgb(62,32,64)', 'rgb(242,121,218)', 'rgb(191,96,172)', 'rgb(89,45,80)', 'rgb(51,26,46)', 'rgb(128,64,106)', 'rgb(166,83,127)', 
                       'rgb(242,121,170)', 'rgb(102,51,71)', 'rgb(51,26,36)', 'rgb(191,96,121)', 'rgb(128,64,81)', 'rgb(77,38,48)', 'rgb(229,115,130)', 'rgb(153,77,87)', 'rgb(102,51,58)')
          
          # 108 distinct colors with transparency
          rgbcolsa <- c('rgba(255,128,128,0.5)', 'rgba(178,89,89,0.5)', 'rgba(51,26,26,0.5)', 'rgba(217,123,108,0.5)', 'rgba(140,79,70,0.5)', 'rgba(89,51,45,0.5)', 'rgba(64,36,32,0.5)', 
                        'rgba(242,153,121,0.5)', 'rgba(178,113,89,0.5)', 'rgba(102,65,51,0.5)', 'rgba(76,48,38,0.5)', 'rgba(51,32,26,0.5)', 'rgba(217,152,108,0.5)', 'rgba(153,107,77,0.5)', 'rgba(102,71,51,0.5)', 'rgba(76,54,38,0.5)', 'rgba(229,176,115,0.5)', 'rgba(178,137,89,0.5)', 'rgba(140,108,70,0.5)', 'rgba(102,78,51,0.5)', 'rgba(51,39,26,0.5)', 'rgba(217,181,108,0.5)', 'rgba(140,117,70,0.5)', 'rgba(76,64,38,0.5)', 'rgba(178,161,89,0.5)', 'rgba(102,92,51,0.5)', 'rgba(51,46,26,0.5)', 
                        'rgba(229,222,115,0.5)', 'rgba(173,179,89,0.5)', 'rgba(74,77,38,0.5)', 'rgba(229,255,128,0.5)', 'rgba(126,140,70,0.5)', 'rgba(46,51,26,0.5)', 'rgba(170,204,102,0.5)', 'rgba(85,102,51,0.5)', 'rgba(186,242,121,0.5)', 'rgba(127,166,83,0.5)', 'rgba(81,128,64,0.5)', 'rgba(116,204,102,0.5)', 'rgba(127,255,128,0.5)', 'rgba(83,166,94,0.5)', 'rgba(51,102,58,0.5)', 'rgba(38,77,43,0.5)', 'rgba(26,51,29,0.5)', 'rgba(115,230,161,0.5)', 'rgba(77,153,107,0.5)', 'rgba(64,128,98,0.5)', 'rgba(45,89,68,0.5)', 'rgba(102,204,170,0.5)', 'rgba(26,51,43,0.5)', 'rgba(121,242,218,0.5)', 'rgba(83,166,149,0.5)', 'rgba(51,102,92,0.5)', 'rgba(38,77,69,0.5)', 'rgba(102,204,197,0.5)', 'rgba(121,234,242,0.5)', 'rgba(83,160,166,0.5)', 'rgba(57,111,115,0.5)', 'rgba(38,74,77,0.5)', 'rgba(26,49,51,0.5)', 'rgba(108,195,217,0.5)', 'rgba(77,138,153,0.5)', 'rgba(121,202,242,0.5)', 'rgba(89,149,179,0.5)', 'rgba(70,117,140),0.5', 'rgba(51,85,102,0.5)', 'rgba(32,53,64,0.5)', 'rgba(128,196,255,0.5)', 'rgba(102,156,204,0.5)', 'rgba(64,98,128,0.5)', 'rgba(128,179,255,0.5)', 'rgba(102,143,204,0.5)', 'rgba(45,62,89,0.5)', 'rgba(26,36,51,0.5)', 'rgba(121,153,242,0.5)', 'rgba(77,97,153,0.5)', 'rgba(128,145,255,0.5)', 'rgba(89,101,179,0.5)', 'rgba(64,72,128,0.5)', 'rgba(45,51,89,0.5)', 'rgba(32,36,64,0.5)', 'rgba(108,108,217,0.5)', 'rgba(58,51,102,0.5)', 'rgba(162,128,255,0.5)', 'rgba(113,89,179,0.5)', 'rgba(89,70,140)', 'rgba(48,38,77,0.5)', 'rgba(39,26,51,0.5)', 'rgba(128,77,153,0.5)', 'rgba(74,45,89)', 'rgba(172,96,191,0.5)', 
                        'rgba(247,128,255,0.5)', 'rgba(111,57,115,0.5)', 'rgba(62,32,64,0.5)', 'rgba(242,121,218,0.5)', 'rgba(191,96,172,0.5)', 'rgba(89,45,80,0.5)', 'rgba(51,26,46,0.5)', 'rgba(128,64,106,0.5)', 'rgba(166,83,127,0.5)', 
                        'rgba(242,121,170,0.5)', 'rgba(102,51,71,0.5)', 'rgba(51,26,36,0.5)', 'rgba(191,96,121,0.5)', 'rgba(128,64,81,0.5)', 'rgba(77,38,48,0.5)', 'rgba(229,115,130,0.5)', 'rgba(153,77,87,0.5)', 'rgba(102,51,58,0.5)')
        } else {
          rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                        'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                        'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                        'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                        'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                        'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                        'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                        'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
          
          rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                       'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                       'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                       'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                       'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                       'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                       'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        }
        ## Same as precip ...
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        ## color by groups
        if (input$gcm.colorBy == 'Group') {
          colsa <- rgbcolsa[id]
          cols <- rgbcols[id]
        } else {
          colsa <- rgbcolsa
          cols <- rgbcols
        }
        
        # Add all models
        if (is.null(input$rowsGcm)) {
          for (gcm in gcms) {
            i <- which(is.element(gcms,gcm))
            leg.name <- paste(paste(as.character(as.matrix(gcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '),
                              paste(substr(lev[id[i]],1,5),'...',sep=''),
                              sep = ' ')
            grp.name <- paste('Group',id[i],sep='')
            
            
            #if (is.null(input$rowsGcm)) {
            if (is.element(input$gcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name, 
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter',
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
            #}
            ## Highlight selected models in tab:models
          } 
        } else {
          im <- input$rowsGcm
          for (i in 1:length(im)) {
            leg.name <- paste(as.character(as.matrix(gcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
            grp.name <- paste('Group',id[im[i]],sep='')
            gcm <- gcms[i] #gcms[im[i]]
            if (is.element(input$gcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name, mode = 'lines', legendgroup = grp.name, 
                                      colors = ",i,", hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name, mode = 'lines', legendgroup = grp.name,
                                      colors = colsa[im[",i,"]], hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = colsa[im[",i,"]], width = 2, shape ='spline'))",sep='')))
          }
          }
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
          } else if (grepl('ensemble', tolower(input$gcm.chart.type))) { # Make an enveloppe instead of lines
            
            p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                            line = list(color = 'transparent'),
                            showlegend = TRUE, name = 'High') %>%
              add_trace(y = ~low, type = 'scatter', mode = 'lines', 
                        fill = 'tonexty', fillcolor='rgba(255,127,80,0.2)', line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'Low') %>%
              add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',name = 'Ens. Mean',
                        line = list(color='rgb(255,127,80)'), showlegend = TRUE,
                        name = 'Average') 
            
            
            if (!is.null(df$ref))
              p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', showlegend = TRUE,
                                         line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
            
            p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
            
            if (input$gcm.legend.sc == 'Hide')
              p.sc <- p.sc %>% layout(showlegend = FALSE)
            
          } else if (grepl('box',tolower(input$gcm.chart.type))) {
            p.sc <- plot_ly(df, type = 'box')
            
            month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
            col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                         'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                         'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                         'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                         'rgb(166,206,227)')
            for (i in 1:12) {
              leg.name <- month.abb[i]
              leg.grp <- month.grp[i]
              eval(parse(text = paste("p.sc <- p.sc %>% 
                                      add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                      type = 'box', boxpoints = 'all',
                                      legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color=col.grp[",i,"],opacity=0.6),
                                      name = leg.name,showlegend =TRUE)",sep='')))
              if (!is.null(df$ref))
                p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                           line = list(color = 'black', dash = 'dash', width = 2),
                                           legendgroup = leg.grp,
                                           showlegend = TRUE)
            } 
          }
      # Add these lines to modify colors in box plot
      # marker = list(color = 'rgb(135,206,250'),
      # line = list(color = 'rgb(135,206,250'),
      
      if (input$gcm.outputValues == 'Bias')  
        ylab <- "Bias in simulated regional temperature [deg. C]"
      else if (input$gcm.outputValues == 'Anomaly')
        ylab <- "Simulated regional temperature anomalies [deg. C]"
      else if (input$gcm.outputValues == 'Change')
        ylab <- "Absolute change in simulted regional temperature with regards to present [deg. C]"
      else 
        ylab <- "Simulated regional temperature [deg. C]"
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$gcm.region),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      if (input$gcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      else
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      
      p.sc$elementId <- NULL
      p.sc
        })
    
    output$gcm.sc.bias.tas.pu <- renderPlotly({
      
      gcm.meta.tas <- gcm.meta.tas.reactive.pu()
      df <- gcm.sc.tas.reactive.pu()
      
      # if (!is.null(input$rowsGcm))
      #   if (input$gcm.sim.sc == 'Selected Simulations')
      #     gcm.meta.tas <- gcm.meta.tas[input$rowsGcm,]
      
      df <- df - df[,dim(df)[2]]      
      
      #df <- df[,-36] # AM Quick fix but has to be removed ... once meta is updated.
      # GCM seasonal cycle
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      
      # define layout output
      #df$ref <- df$ref
      if (input$gcm.chart.type.pu == 'Individual Simulations') {
        ## Make the plot
        #p.sc <- plot_ly(df, x = ~month, y = ~gcm.1,type = 'scatter',mode = 'markers+lines', line = list(width = 2, color = "grey",shape = 'spline'))
        # create plot_ly
        p.sc <- plot_ly(df, x = ~month)
        
        gcms <- colnames(df)[grep('gcm',colnames(df))]
        id <- 1 : (length(df) - 1)
        lev <- levels(factor(id))
        
        if (length(lev) > 50) {
          # 108 distinct colors
          rgbcols <- c('rgb(255,128,128)', 'rgb(178,89,89)', 'rgb(51,26,26)', 'rgb(217,123,108)', 'rgb(140,79,70)', 'rgb(89,51,45)', 'rgb(64,36,32)', 'rgb(242,153,121)', 'rgb(178,113,89)', 
                       'rgb(102,65,51)', 'rgb(76,48,38)', 'rgb(51,32,26)', 'rgb(217,152,108)', 'rgb(153,107,77)', 'rgb(102,71,51)', 'rgb(76,54,38)', 'rgb(229,176,115)', 'rgb(178,137,89)',
                       'rgb(140,108,70)', 'rgb(102,78,51)', 'rgb(51,39,26)', 'rgb(217,181,108)', 'rgb(140,117,70)', 'rgb(76,64,38)', 'rgb(178,161,89)', 'rgb(102,92,51)', 'rgb(51,46,26)', 
                       'rgb(229,222,115)', 'rgb(173,179,89)', 'rgb(74,77,38)', 'rgb(229,255,128)', 'rgb(126,140,70)', 'rgb(46,51,26)', 'rgb(170,204,102)', 'rgb(85,102,51)', 'rgb(186,242,121)', 
                       'rgb(127,166,83)', 'rgb(81,128,64)', 'rgb(116,204,102)', 'rgb(127,255,128)', 'rgb(83,166,94)', 'rgb(51,102,58)', 'rgb(38,77,43)', 'rgb(26,51,29)', 'rgb(115,230,161)', 
                       'rgb(77,153,107)', 'rgb(64,128,98)', 'rgb(45,89,68)', 'rgb(102,204,170)', 'rgb(26,51,43)', 'rgb(121,242,218)', 'rgb(83,166,149)', 'rgb(51,102,92)', 'rgb(38,77,69)',
                       'rgb(102,204,197)', 'rgb(121,234,242)', 'rgb(83,160,166)', 'rgb(57,111,115)', 'rgb(38,74,77)', 'rgb(26,49,51)', 'rgb(108,195,217)', 'rgb(77,138,153)', 'rgb(121,202,242)', 
                       'rgb(89,149,179)', 'rgb(70,117,140)', 'rgb(51,85,102)', 'rgb(32,53,64)', 'rgb(128,196,255)', 'rgb(102,156,204)', 'rgb(64,98,128)', 'rgb(128,179,255)', 'rgb(102,143,204)', 
                       'rgb(45,62,89)', 'rgb(26,36,51)', 'rgb(121,153,242)', 'rgb(77,97,153)', 'rgb(128,145,255)', 'rgb(89,101,179)', 'rgb(64,72,128)', 'rgb(45,51,89)', 'rgb(32,36,64)', 'rgb(108,108,217)', 			      'rgb(58,51,102)', 'rgb(162,128,255)', 'rgb(113,89,179)', 'rgb(89,70,140)', 'rgb(48,38,77)', 'rgb(39,26,51)', 'rgb(128,77,153)', 'rgb(74,45,89)', 'rgb(172,96,191)', 
                       'rgb(247,128,255)', 'rgb(111,57,115)', 'rgb(62,32,64)', 'rgb(242,121,218)', 'rgb(191,96,172)', 'rgb(89,45,80)', 'rgb(51,26,46)', 'rgb(128,64,106)', 'rgb(166,83,127)', 
                       'rgb(242,121,170)', 'rgb(102,51,71)', 'rgb(51,26,36)', 'rgb(191,96,121)', 'rgb(128,64,81)', 'rgb(77,38,48)', 'rgb(229,115,130)', 'rgb(153,77,87)', 'rgb(102,51,58)')
          
          # 108 distinct colors with transparency
          rgbcolsa <- c('rgba(255,128,128,0.5)', 'rgba(178,89,89,0.5)', 'rgba(51,26,26,0.5)', 'rgba(217,123,108,0.5)', 'rgba(140,79,70,0.5)', 'rgba(89,51,45,0.5)', 'rgba(64,36,32,0.5)', 
                        'rgba(242,153,121,0.5)', 'rgba(178,113,89,0.5)', 'rgba(102,65,51,0.5)', 'rgba(76,48,38,0.5)', 'rgba(51,32,26,0.5)', 'rgba(217,152,108,0.5)', 'rgba(153,107,77,0.5)', 'rgba(102,71,51,0.5)', 'rgba(76,54,38,0.5)', 'rgba(229,176,115,0.5)', 'rgba(178,137,89,0.5)', 'rgba(140,108,70,0.5)', 'rgba(102,78,51,0.5)', 'rgba(51,39,26,0.5)', 'rgba(217,181,108,0.5)', 'rgba(140,117,70,0.5)', 'rgba(76,64,38,0.5)', 'rgba(178,161,89,0.5)', 'rgba(102,92,51,0.5)', 'rgba(51,46,26,0.5)', 
                        'rgba(229,222,115,0.5)', 'rgba(173,179,89,0.5)', 'rgba(74,77,38,0.5)', 'rgba(229,255,128,0.5)', 'rgba(126,140,70,0.5)', 'rgba(46,51,26,0.5)', 'rgba(170,204,102,0.5)', 'rgba(85,102,51,0.5)', 'rgba(186,242,121,0.5)', 'rgba(127,166,83,0.5)', 'rgba(81,128,64,0.5)', 'rgba(116,204,102,0.5)', 'rgba(127,255,128,0.5)', 'rgba(83,166,94,0.5)', 'rgba(51,102,58,0.5)', 'rgba(38,77,43,0.5)', 'rgba(26,51,29,0.5)', 'rgba(115,230,161,0.5)', 'rgba(77,153,107,0.5)', 'rgba(64,128,98,0.5)', 'rgba(45,89,68,0.5)', 'rgba(102,204,170,0.5)', 'rgba(26,51,43,0.5)', 'rgba(121,242,218,0.5)', 'rgba(83,166,149,0.5)', 'rgba(51,102,92,0.5)', 'rgba(38,77,69,0.5)', 'rgba(102,204,197,0.5)', 'rgba(121,234,242,0.5)', 'rgba(83,160,166,0.5)', 'rgba(57,111,115,0.5)', 'rgba(38,74,77,0.5)', 'rgba(26,49,51,0.5)', 'rgba(108,195,217,0.5)', 'rgba(77,138,153,0.5)', 'rgba(121,202,242,0.5)', 'rgba(89,149,179,0.5)', 'rgba(70,117,140),0.5', 'rgba(51,85,102,0.5)', 'rgba(32,53,64,0.5)', 'rgba(128,196,255,0.5)', 'rgba(102,156,204,0.5)', 'rgba(64,98,128,0.5)', 'rgba(128,179,255,0.5)', 'rgba(102,143,204,0.5)', 'rgba(45,62,89,0.5)', 'rgba(26,36,51,0.5)', 'rgba(121,153,242,0.5)', 'rgba(77,97,153,0.5)', 'rgba(128,145,255,0.5)', 'rgba(89,101,179,0.5)', 'rgba(64,72,128,0.5)', 'rgba(45,51,89,0.5)', 'rgba(32,36,64,0.5)', 'rgba(108,108,217,0.5)', 'rgba(58,51,102,0.5)', 'rgba(162,128,255,0.5)', 'rgba(113,89,179,0.5)', 'rgba(89,70,140)', 'rgba(48,38,77,0.5)', 'rgba(39,26,51,0.5)', 'rgba(128,77,153,0.5)', 'rgba(74,45,89)', 'rgba(172,96,191,0.5)', 
                        'rgba(247,128,255,0.5)', 'rgba(111,57,115,0.5)', 'rgba(62,32,64,0.5)', 'rgba(242,121,218,0.5)', 'rgba(191,96,172,0.5)', 'rgba(89,45,80,0.5)', 'rgba(51,26,46,0.5)', 'rgba(128,64,106,0.5)', 'rgba(166,83,127,0.5)', 
                        'rgba(242,121,170,0.5)', 'rgba(102,51,71,0.5)', 'rgba(51,26,36,0.5)', 'rgba(191,96,121,0.5)', 'rgba(128,64,81,0.5)', 'rgba(77,38,48,0.5)', 'rgba(229,115,130,0.5)', 'rgba(153,77,87,0.5)', 'rgba(102,51,58,0.5)')
        } else {
          rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                        'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                        'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                        'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                        'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                        'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                        'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                        'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
          
          rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                       'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                       'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                       'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                       'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                       'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                       'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        }
        ## Same as precip ...
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        ## color by groups
        
        colsa <- rgbcolsa
        cols <- rgbcols
        
        # Add all models
        
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          leg.name <- paste(paste(as.character(as.matrix(gcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='')
          
          
          #if (is.null(input$rowsGcm)) {
          if (is.element(input$gcm.colorBy, c('None','---')))
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                    name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                    showlegend = TRUE, legendgroup = grp.name, 
                                    line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
          else
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter',
                                    name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                    showlegend = TRUE, legendgroup = grp.name,
                                    line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          #}
          ## Highlight selected models in tab:models
        } 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.chart.type.pu))) { # Make an enveloppe instead of lines
        
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines', 
                    fill = 'tonexty', fillcolor='rgba(255,127,80,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',name = 'Ens. Mean',
                    line = list(color='rgb(255,127,80)'), showlegend = TRUE,
                    name = 'Average') 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', showlegend = TRUE,
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
        
        if (input$gcm.legend.sc == 'Hide')
          p.sc <- p.sc %>% layout(showlegend = FALSE)
        
      } else if (grepl('box',tolower(input$gcm.chart.type.pu))) {
        p.sc <- plot_ly(df, type = 'box')
        
        month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
        col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                     'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                     'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                     'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                     'rgb(166,206,227)')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          leg.grp <- month.grp[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color=col.grp[",i,"],opacity=0.6),
                                  name = leg.name,showlegend =TRUE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                       line = list(color = 'black', dash = 'dash', width = 2),
                                       legendgroup = leg.grp,
                                       showlegend = TRUE)
        } 
      }
      # Add these lines to modify colors in box plot
      # marker = list(color = 'rgb(135,206,250'),
      # line = list(color = 'rgb(135,206,250'),
      
      ylab <- "Bias in simulated regional temperature [deg. C]"
      
      p.sc <- p.sc %>% layout(title = paste("Region:",input$gcm.region.pu),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      if (input$gcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      else
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      p.sc$elementId <- NULL
      p.sc
    })
    
    output$gcm.sc.tas.pu <- renderPlotly({
      
      gcm.meta.tas <- gcm.meta.tas.reactive.sc.pu()
      df <- gcm.sc.tas.reactive.sc.pu()
      
      #df <- df[,-36] # AM Quick fix but has to be removed ... once meta is updated.
      # GCM seasonal cycle
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      
      # define layout output
      #df$ref <- df$ref
      if (input$gcm.sc.chart.type.pu == 'Individual Simulations') {
        ## Make the plot
        #p.sc <- plot_ly(df, x = ~month, y = ~gcm.1,type = 'scatter',mode = 'markers+lines', line = list(width = 2, color = "grey",shape = 'spline'))
        # create plot_ly
        p.sc <- plot_ly(df, x = ~month)
        
        gcms <- colnames(df)[grep('gcm',colnames(df))]
        id <- 1 : (length(df) - 1)
        lev <- levels(factor(id))
        
        if (length(lev) > 50) {
          # 108 distinct colors
          rgbcols <- c('rgb(255,128,128)', 'rgb(178,89,89)', 'rgb(51,26,26)', 'rgb(217,123,108)', 'rgb(140,79,70)', 'rgb(89,51,45)', 'rgb(64,36,32)', 'rgb(242,153,121)', 'rgb(178,113,89)', 
                       'rgb(102,65,51)', 'rgb(76,48,38)', 'rgb(51,32,26)', 'rgb(217,152,108)', 'rgb(153,107,77)', 'rgb(102,71,51)', 'rgb(76,54,38)', 'rgb(229,176,115)', 'rgb(178,137,89)',
                       'rgb(140,108,70)', 'rgb(102,78,51)', 'rgb(51,39,26)', 'rgb(217,181,108)', 'rgb(140,117,70)', 'rgb(76,64,38)', 'rgb(178,161,89)', 'rgb(102,92,51)', 'rgb(51,46,26)', 
                       'rgb(229,222,115)', 'rgb(173,179,89)', 'rgb(74,77,38)', 'rgb(229,255,128)', 'rgb(126,140,70)', 'rgb(46,51,26)', 'rgb(170,204,102)', 'rgb(85,102,51)', 'rgb(186,242,121)', 
                       'rgb(127,166,83)', 'rgb(81,128,64)', 'rgb(116,204,102)', 'rgb(127,255,128)', 'rgb(83,166,94)', 'rgb(51,102,58)', 'rgb(38,77,43)', 'rgb(26,51,29)', 'rgb(115,230,161)', 
                       'rgb(77,153,107)', 'rgb(64,128,98)', 'rgb(45,89,68)', 'rgb(102,204,170)', 'rgb(26,51,43)', 'rgb(121,242,218)', 'rgb(83,166,149)', 'rgb(51,102,92)', 'rgb(38,77,69)',
                       'rgb(102,204,197)', 'rgb(121,234,242)', 'rgb(83,160,166)', 'rgb(57,111,115)', 'rgb(38,74,77)', 'rgb(26,49,51)', 'rgb(108,195,217)', 'rgb(77,138,153)', 'rgb(121,202,242)', 
                       'rgb(89,149,179)', 'rgb(70,117,140)', 'rgb(51,85,102)', 'rgb(32,53,64)', 'rgb(128,196,255)', 'rgb(102,156,204)', 'rgb(64,98,128)', 'rgb(128,179,255)', 'rgb(102,143,204)', 
                       'rgb(45,62,89)', 'rgb(26,36,51)', 'rgb(121,153,242)', 'rgb(77,97,153)', 'rgb(128,145,255)', 'rgb(89,101,179)', 'rgb(64,72,128)', 'rgb(45,51,89)', 'rgb(32,36,64)', 'rgb(108,108,217)', 			      'rgb(58,51,102)', 'rgb(162,128,255)', 'rgb(113,89,179)', 'rgb(89,70,140)', 'rgb(48,38,77)', 'rgb(39,26,51)', 'rgb(128,77,153)', 'rgb(74,45,89)', 'rgb(172,96,191)', 
                       'rgb(247,128,255)', 'rgb(111,57,115)', 'rgb(62,32,64)', 'rgb(242,121,218)', 'rgb(191,96,172)', 'rgb(89,45,80)', 'rgb(51,26,46)', 'rgb(128,64,106)', 'rgb(166,83,127)', 
                       'rgb(242,121,170)', 'rgb(102,51,71)', 'rgb(51,26,36)', 'rgb(191,96,121)', 'rgb(128,64,81)', 'rgb(77,38,48)', 'rgb(229,115,130)', 'rgb(153,77,87)', 'rgb(102,51,58)')
          
          # 108 distinct colors with transparency
          rgbcolsa <- c('rgba(255,128,128,0.5)', 'rgba(178,89,89,0.5)', 'rgba(51,26,26,0.5)', 'rgba(217,123,108,0.5)', 'rgba(140,79,70,0.5)', 'rgba(89,51,45,0.5)', 'rgba(64,36,32,0.5)', 
                        'rgba(242,153,121,0.5)', 'rgba(178,113,89,0.5)', 'rgba(102,65,51,0.5)', 'rgba(76,48,38,0.5)', 'rgba(51,32,26,0.5)', 'rgba(217,152,108,0.5)', 'rgba(153,107,77,0.5)', 'rgba(102,71,51,0.5)', 'rgba(76,54,38,0.5)', 'rgba(229,176,115,0.5)', 'rgba(178,137,89,0.5)', 'rgba(140,108,70,0.5)', 'rgba(102,78,51,0.5)', 'rgba(51,39,26,0.5)', 'rgba(217,181,108,0.5)', 'rgba(140,117,70,0.5)', 'rgba(76,64,38,0.5)', 'rgba(178,161,89,0.5)', 'rgba(102,92,51,0.5)', 'rgba(51,46,26,0.5)', 
                        'rgba(229,222,115,0.5)', 'rgba(173,179,89,0.5)', 'rgba(74,77,38,0.5)', 'rgba(229,255,128,0.5)', 'rgba(126,140,70,0.5)', 'rgba(46,51,26,0.5)', 'rgba(170,204,102,0.5)', 'rgba(85,102,51,0.5)', 'rgba(186,242,121,0.5)', 'rgba(127,166,83,0.5)', 'rgba(81,128,64,0.5)', 'rgba(116,204,102,0.5)', 'rgba(127,255,128,0.5)', 'rgba(83,166,94,0.5)', 'rgba(51,102,58,0.5)', 'rgba(38,77,43,0.5)', 'rgba(26,51,29,0.5)', 'rgba(115,230,161,0.5)', 'rgba(77,153,107,0.5)', 'rgba(64,128,98,0.5)', 'rgba(45,89,68,0.5)', 'rgba(102,204,170,0.5)', 'rgba(26,51,43,0.5)', 'rgba(121,242,218,0.5)', 'rgba(83,166,149,0.5)', 'rgba(51,102,92,0.5)', 'rgba(38,77,69,0.5)', 'rgba(102,204,197,0.5)', 'rgba(121,234,242,0.5)', 'rgba(83,160,166,0.5)', 'rgba(57,111,115,0.5)', 'rgba(38,74,77,0.5)', 'rgba(26,49,51,0.5)', 'rgba(108,195,217,0.5)', 'rgba(77,138,153,0.5)', 'rgba(121,202,242,0.5)', 'rgba(89,149,179,0.5)', 'rgba(70,117,140),0.5', 'rgba(51,85,102,0.5)', 'rgba(32,53,64,0.5)', 'rgba(128,196,255,0.5)', 'rgba(102,156,204,0.5)', 'rgba(64,98,128,0.5)', 'rgba(128,179,255,0.5)', 'rgba(102,143,204,0.5)', 'rgba(45,62,89,0.5)', 'rgba(26,36,51,0.5)', 'rgba(121,153,242,0.5)', 'rgba(77,97,153,0.5)', 'rgba(128,145,255,0.5)', 'rgba(89,101,179,0.5)', 'rgba(64,72,128,0.5)', 'rgba(45,51,89,0.5)', 'rgba(32,36,64,0.5)', 'rgba(108,108,217,0.5)', 'rgba(58,51,102,0.5)', 'rgba(162,128,255,0.5)', 'rgba(113,89,179,0.5)', 'rgba(89,70,140)', 'rgba(48,38,77,0.5)', 'rgba(39,26,51,0.5)', 'rgba(128,77,153,0.5)', 'rgba(74,45,89)', 'rgba(172,96,191,0.5)', 
                        'rgba(247,128,255,0.5)', 'rgba(111,57,115,0.5)', 'rgba(62,32,64,0.5)', 'rgba(242,121,218,0.5)', 'rgba(191,96,172,0.5)', 'rgba(89,45,80,0.5)', 'rgba(51,26,46,0.5)', 'rgba(128,64,106,0.5)', 'rgba(166,83,127,0.5)', 
                        'rgba(242,121,170,0.5)', 'rgba(102,51,71,0.5)', 'rgba(51,26,36,0.5)', 'rgba(191,96,121,0.5)', 'rgba(128,64,81,0.5)', 'rgba(77,38,48,0.5)', 'rgba(229,115,130,0.5)', 'rgba(153,77,87,0.5)', 'rgba(102,51,58,0.5)')
        } else {
          rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                        'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                        'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                        'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                        'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                        'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                        'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                        'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
          
          rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                       'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                       'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                       'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                       'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                       'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                       'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        }
        ## Same as precip ...
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        ## color by groups
        colsa <- rgbcolsa[id]
        cols <- rgbcols[id]
        
        # Add all models
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          leg.name <- paste(paste(as.character(as.matrix(gcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='')
          
          
          #if (is.null(input$rowsGcm)) {
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter',
                                  name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          #}
          ## Highlight selected models in tab:models
        }
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.sc.chart.type.pu))) { # Make an enveloppe instead of lines
        
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines', 
                    fill = 'tonexty', fillcolor='rgba(255,127,80,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',name = 'Ens. Mean',
                    line = list(color='rgb(255,127,80)'), showlegend = TRUE,
                    name = 'Average') 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', showlegend = TRUE,
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
        
        if (input$gcm.legend.sc == 'Hide')
          p.sc <- p.sc %>% layout(showlegend = FALSE)
        
      } else if (grepl('box',tolower(input$gcm.sc.chart.type.pu))) {
        p.sc <- plot_ly(df, type = 'box')
        
        month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
        col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                     'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                     'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                     'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                     'rgb(166,206,227)')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          leg.grp <- month.grp[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color=col.grp[",i,"],opacity=0.6),
                                  name = leg.name,showlegend =TRUE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                       line = list(color = 'black', dash = 'dash', width = 2),
                                       legendgroup = leg.grp,
                                       showlegend = TRUE)
        } 
      }
      # Add these lines to modify colors in box plot
      # marker = list(color = 'rgb(135,206,250'),
      # line = list(color = 'rgb(135,206,250'),
      
      ylab <- "Simulated regional temperature [deg. C]"
      
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$gcm.sc.region.pu),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      if (input$gcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      else
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      p.sc$elementId <- NULL
      p.sc
    })
    
    output$gcm.cc.tas.pu <- renderPlotly({
      
      gcm.meta.tas <- gcm.meta.tas.reactive.cc.pu()
      df <- gcm.sc.tas.reactive.cc.pu() - gcm.sc.tas.present.cc.pu()
      
      #df <- df[,-36] # AM Quick fix but has to be removed ... once meta is updated.
      # GCM seasonal cycle
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      
      # define layout output
      #df$ref <- df$ref
      if (input$gcm.cc.chart.type == 'Individual Simulations') {
        ## Make the plot
        #p.sc <- plot_ly(df, x = ~month, y = ~gcm.1,type = 'scatter',mode = 'markers+lines', line = list(width = 2, color = "grey",shape = 'spline'))
        # create plot_ly
        p.sc <- plot_ly(df, x = ~month)
        
        gcms <- colnames(df)[grep('gcm',colnames(df))]
        id <- 1 : (length(df) - 1)
        lev <- levels(factor(id))
        
        if (length(lev) > 50) {
          # 108 distinct colors
          rgbcols <- c('rgb(255,128,128)', 'rgb(178,89,89)', 'rgb(51,26,26)', 'rgb(217,123,108)', 'rgb(140,79,70)', 'rgb(89,51,45)', 'rgb(64,36,32)', 'rgb(242,153,121)', 'rgb(178,113,89)', 
                       'rgb(102,65,51)', 'rgb(76,48,38)', 'rgb(51,32,26)', 'rgb(217,152,108)', 'rgb(153,107,77)', 'rgb(102,71,51)', 'rgb(76,54,38)', 'rgb(229,176,115)', 'rgb(178,137,89)',
                       'rgb(140,108,70)', 'rgb(102,78,51)', 'rgb(51,39,26)', 'rgb(217,181,108)', 'rgb(140,117,70)', 'rgb(76,64,38)', 'rgb(178,161,89)', 'rgb(102,92,51)', 'rgb(51,46,26)', 
                       'rgb(229,222,115)', 'rgb(173,179,89)', 'rgb(74,77,38)', 'rgb(229,255,128)', 'rgb(126,140,70)', 'rgb(46,51,26)', 'rgb(170,204,102)', 'rgb(85,102,51)', 'rgb(186,242,121)', 
                       'rgb(127,166,83)', 'rgb(81,128,64)', 'rgb(116,204,102)', 'rgb(127,255,128)', 'rgb(83,166,94)', 'rgb(51,102,58)', 'rgb(38,77,43)', 'rgb(26,51,29)', 'rgb(115,230,161)', 
                       'rgb(77,153,107)', 'rgb(64,128,98)', 'rgb(45,89,68)', 'rgb(102,204,170)', 'rgb(26,51,43)', 'rgb(121,242,218)', 'rgb(83,166,149)', 'rgb(51,102,92)', 'rgb(38,77,69)',
                       'rgb(102,204,197)', 'rgb(121,234,242)', 'rgb(83,160,166)', 'rgb(57,111,115)', 'rgb(38,74,77)', 'rgb(26,49,51)', 'rgb(108,195,217)', 'rgb(77,138,153)', 'rgb(121,202,242)', 
                       'rgb(89,149,179)', 'rgb(70,117,140)', 'rgb(51,85,102)', 'rgb(32,53,64)', 'rgb(128,196,255)', 'rgb(102,156,204)', 'rgb(64,98,128)', 'rgb(128,179,255)', 'rgb(102,143,204)', 
                       'rgb(45,62,89)', 'rgb(26,36,51)', 'rgb(121,153,242)', 'rgb(77,97,153)', 'rgb(128,145,255)', 'rgb(89,101,179)', 'rgb(64,72,128)', 'rgb(45,51,89)', 'rgb(32,36,64)', 'rgb(108,108,217)', 			      'rgb(58,51,102)', 'rgb(162,128,255)', 'rgb(113,89,179)', 'rgb(89,70,140)', 'rgb(48,38,77)', 'rgb(39,26,51)', 'rgb(128,77,153)', 'rgb(74,45,89)', 'rgb(172,96,191)', 
                       'rgb(247,128,255)', 'rgb(111,57,115)', 'rgb(62,32,64)', 'rgb(242,121,218)', 'rgb(191,96,172)', 'rgb(89,45,80)', 'rgb(51,26,46)', 'rgb(128,64,106)', 'rgb(166,83,127)', 
                       'rgb(242,121,170)', 'rgb(102,51,71)', 'rgb(51,26,36)', 'rgb(191,96,121)', 'rgb(128,64,81)', 'rgb(77,38,48)', 'rgb(229,115,130)', 'rgb(153,77,87)', 'rgb(102,51,58)')
          
          # 108 distinct colors with transparency
          rgbcolsa <- c('rgba(255,128,128,0.5)', 'rgba(178,89,89,0.5)', 'rgba(51,26,26,0.5)', 'rgba(217,123,108,0.5)', 'rgba(140,79,70,0.5)', 'rgba(89,51,45,0.5)', 'rgba(64,36,32,0.5)', 
                        'rgba(242,153,121,0.5)', 'rgba(178,113,89,0.5)', 'rgba(102,65,51,0.5)', 'rgba(76,48,38,0.5)', 'rgba(51,32,26,0.5)', 'rgba(217,152,108,0.5)', 'rgba(153,107,77,0.5)', 'rgba(102,71,51,0.5)', 'rgba(76,54,38,0.5)', 'rgba(229,176,115,0.5)', 'rgba(178,137,89,0.5)', 'rgba(140,108,70,0.5)', 'rgba(102,78,51,0.5)', 'rgba(51,39,26,0.5)', 'rgba(217,181,108,0.5)', 'rgba(140,117,70,0.5)', 'rgba(76,64,38,0.5)', 'rgba(178,161,89,0.5)', 'rgba(102,92,51,0.5)', 'rgba(51,46,26,0.5)', 
                        'rgba(229,222,115,0.5)', 'rgba(173,179,89,0.5)', 'rgba(74,77,38,0.5)', 'rgba(229,255,128,0.5)', 'rgba(126,140,70,0.5)', 'rgba(46,51,26,0.5)', 'rgba(170,204,102,0.5)', 'rgba(85,102,51,0.5)', 'rgba(186,242,121,0.5)', 'rgba(127,166,83,0.5)', 'rgba(81,128,64,0.5)', 'rgba(116,204,102,0.5)', 'rgba(127,255,128,0.5)', 'rgba(83,166,94,0.5)', 'rgba(51,102,58,0.5)', 'rgba(38,77,43,0.5)', 'rgba(26,51,29,0.5)', 'rgba(115,230,161,0.5)', 'rgba(77,153,107,0.5)', 'rgba(64,128,98,0.5)', 'rgba(45,89,68,0.5)', 'rgba(102,204,170,0.5)', 'rgba(26,51,43,0.5)', 'rgba(121,242,218,0.5)', 'rgba(83,166,149,0.5)', 'rgba(51,102,92,0.5)', 'rgba(38,77,69,0.5)', 'rgba(102,204,197,0.5)', 'rgba(121,234,242,0.5)', 'rgba(83,160,166,0.5)', 'rgba(57,111,115,0.5)', 'rgba(38,74,77,0.5)', 'rgba(26,49,51,0.5)', 'rgba(108,195,217,0.5)', 'rgba(77,138,153,0.5)', 'rgba(121,202,242,0.5)', 'rgba(89,149,179,0.5)', 'rgba(70,117,140),0.5', 'rgba(51,85,102,0.5)', 'rgba(32,53,64,0.5)', 'rgba(128,196,255,0.5)', 'rgba(102,156,204,0.5)', 'rgba(64,98,128,0.5)', 'rgba(128,179,255,0.5)', 'rgba(102,143,204,0.5)', 'rgba(45,62,89,0.5)', 'rgba(26,36,51,0.5)', 'rgba(121,153,242,0.5)', 'rgba(77,97,153,0.5)', 'rgba(128,145,255,0.5)', 'rgba(89,101,179,0.5)', 'rgba(64,72,128,0.5)', 'rgba(45,51,89,0.5)', 'rgba(32,36,64,0.5)', 'rgba(108,108,217,0.5)', 'rgba(58,51,102,0.5)', 'rgba(162,128,255,0.5)', 'rgba(113,89,179,0.5)', 'rgba(89,70,140)', 'rgba(48,38,77,0.5)', 'rgba(39,26,51,0.5)', 'rgba(128,77,153,0.5)', 'rgba(74,45,89)', 'rgba(172,96,191,0.5)', 
                        'rgba(247,128,255,0.5)', 'rgba(111,57,115,0.5)', 'rgba(62,32,64,0.5)', 'rgba(242,121,218,0.5)', 'rgba(191,96,172,0.5)', 'rgba(89,45,80,0.5)', 'rgba(51,26,46,0.5)', 'rgba(128,64,106,0.5)', 'rgba(166,83,127,0.5)', 
                        'rgba(242,121,170,0.5)', 'rgba(102,51,71,0.5)', 'rgba(51,26,36,0.5)', 'rgba(191,96,121,0.5)', 'rgba(128,64,81,0.5)', 'rgba(77,38,48,0.5)', 'rgba(229,115,130,0.5)', 'rgba(153,77,87,0.5)', 'rgba(102,51,58,0.5)')
        } else {
          rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                        'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                        'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                        'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                        'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                        'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                        'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                        'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
          
          rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                       'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                       'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                       'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                       'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                       'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                       'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        }
        ## Same as precip ...
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        ## color by groups
        colsa <- rgbcolsa[id]
        cols <- rgbcols[id]
        
        # Add all models
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          leg.name <- paste(paste(as.character(as.matrix(gcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='')
          
          
          #if (is.null(input$rowsGcm)) {
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter',
                                  name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          #}
          ## Highlight selected models in tab:models
        }
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.cc.chart.type))) { # Make an enveloppe instead of lines
        
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines', 
                    fill = 'tonexty', fillcolor='rgba(255,127,80,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',name = 'Ens. Mean',
                    line = list(color='rgb(255,127,80)'), showlegend = TRUE,
                    name = 'Average') 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', showlegend = TRUE,
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
        
        if (input$gcm.legend.sc == 'Hide')
          p.sc <- p.sc %>% layout(showlegend = FALSE)
        
      } else if (grepl('box',tolower(input$gcm.cc.chart.type))) {
        p.sc <- plot_ly(df, type = 'box')
        
        month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
        col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                     'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                     'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                     'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                     'rgb(166,206,227)')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          leg.grp <- month.grp[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color=col.grp[",i,"],opacity=0.6),
                                  name = leg.name,showlegend =TRUE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                       line = list(color = 'black', dash = 'dash', width = 2),
                                       legendgroup = leg.grp,
                                       showlegend = TRUE)
        } 
      }
      # Add these lines to modify colors in box plot
      # marker = list(color = 'rgb(135,206,250'),
      # line = list(color = 'rgb(135,206,250'),
      
      ylab <- "Changes in simulated regional temperature [deg. C]"
      
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$gcm.cc.region),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      if (input$gcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      else
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      p.sc$elementId <- NULL
      p.sc
    })
    
    output$gcm.sc.pr <- renderPlotly({
      gcm.meta.pr <- gcm.meta.pr.reactive()
      df <- gcm.sc.pr.reactive()
      
      if (!is.null(input$gcm.sim.sc))
        if (input$gcm.sim.sc == 'Selected Simulations')
          if (!is.null(input$rowsGcm))
            gcm.meta.pr <- gcm.meta.pr[input$rowsGcm,]
      
      if (input$gcm.outputValues == 'Bias')
        df <- ((df - df[,dim(df)[2]])/df[,dim(df)[2]]) * 100
      else if (input$gcm.outputValues == 'Anomaly') {
        DF <- t(df)
        df <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$gcm.outputValues == 'Change') {
        df <- ((gcm.sc.pr.reactive() - gcm.sc.pr.present())/ gcm.sc.pr.present()) * 100
      }
      
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      if (input$gcm.chart.type == 'Individual Simulations') {
        ## Make the plot
        p.sc <- plot_ly(df,x = ~month)
        
        gcms <- colnames(df)[grep('gcm',colnames(df))]
        if (is.element(input$gcm.groupBy,c('None','---'))) {
          id <- 1 : (length(df) - 1)
          lev <- levels(factor(id))
        }
        else { 
          id <- as.integer(factor(base::subset(gcm.meta.pr, select = input$gcm.groupBy)[[1]]))
          lev <- levels(factor(base::subset(gcm.meta.pr, select = input$gcm.groupBy)[[1]]))
        }
        
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        
        # Color by
        if (input$gcm.colorBy == 'Group') {
          colsa <- rgbcolsa[id]
          cols <- rgbcols[id]
        } else {
          colsa <- rgbcolsa
          cols <- rgbcols
        }
        # browser()
        ## Add all Simulations
        if (is.null(input$rowsGcm)) {
          for (gcm in gcms) {
            i <- which(is.element(gcms,gcm))
            #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
            leg.name <- paste(paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '),
                              paste(substr(lev[id[i]],1,5),'...',sep=''),
                              sep = ' ')
            grp.name <- paste('Group',id[i],sep='')
            
            if (is.element(input$gcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
            
          }
          ## Highlight selected Simulations in tab:models
        } else {
          im <- input$rowsGcm
          for (i in 1:length(im)) {
            leg.name <- paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' ')
            grp.name <- paste('Group',id[i],sep='')
            gcm <- gcms[i]
            if (is.element(input$gcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name, mode = 'lines', legendgroup = grp.name,
                                      colors = ",i,",hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name, mode = 'lines', legendgroup = grp.name,
                                      colors = colsa[im[",i,"]],hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = colsa[im[",i,"]], width = 2, shape ='spline'))",sep='')))
          }            
          
          }
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        } else if (grepl('ensemble', tolower(input$gcm.chart.type))) { # Make an enveloppe instead of lines
          p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                          line = list(color = 'transparent'),name = 'High',showlegend = TRUE) %>%
            add_trace(y = ~low, type = 'scatter', mode = 'lines',showlegend = TRUE,
                      fill = 'tonexty', fillcolor='rgba(135,206,250,0.2)', line = list(color = 'transparent'),name = 'Low') %>%
            add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',line = list(color='rgb(135,206,250)'),
                      name = 'Ens. Mean',showlegend = TRUE) 
          
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                       line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
          
          p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
          
          if (input$gcm.legend.sc == 'Hide') 
            p.sc <- p.sc %>% layout(showlegend = FALSE)
          
        } else if (grepl('box',tolower(input$gcm.chart.type))) {
          p.sc <- plot_ly(df, type = 'box')
          
          month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
          col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                       'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                       'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                       'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                       'rgb(166,206,227)')
          for (i in 1:12) {
            leg.name <- month.abb[i]
            leg.grp <- month.grp[i]
            eval(parse(text = paste("p.sc <- p.sc %>% 
                                    add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                    type = 'box', boxpoints = 'all',
                                    legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                    line = list(color=col.grp[",i,"],opacity=0.6),
                                    name = leg.name,showlegend =TRUE)",sep='')))
            if (!is.null(df$ref))
              p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                         line = list(color = 'black', dash = 'dash', width = 2),
                                         legendgroup = leg.grp,
                                         showlegend = TRUE) 
          } 
        }
      
      if (input$gcm.outputValues == 'Bias')  
        ylab <- "Bias in simulated regional precipitation [%]"
      else if (input$gcm.outputValues == 'Anomaly') 
        ylab <- "Simulated regional precipitation anomalies [mm]"
      else if (input$gcm.outputValues == 'Change')
        ylab <- "Relative change in simulted regional precipitation with regards to present [%]"
      else 
        ylab <- "Simulated regional precipitation [mm]"
      # Format layout 
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$gcm.region),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      
      if (input$gcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      
      p.sc$elementId <- NULL
      p.sc
        })
    
    output$gcm.sc.bias.pr.pu <- renderPlotly({
      gcm.meta.pr <- gcm.meta.pr.reactive.pu()
      df <- gcm.sc.pr.reactive.pu()
      
      df <- ((df - df[,dim(df)[2]])/df[,dim(df)[2]]) * 100
      
      
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      if (input$gcm.chart.type.pu == 'Individual Simulations') {
        ## Make the plot
        p.sc <- plot_ly(df,x = ~month)
        
        gcms <- colnames(df)[grep('gcm',colnames(df))]
        id <- 1 : (length(df) - 1)
        lev <- levels(factor(id))
        
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        
        # Color by
        colsa <- rgbcolsa
        cols <- rgbcols
        # browser()
        ## Add all Simulations
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='')
          
          if (is.element(input$gcm.colorBy, c('None','---')))
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                    name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                    showlegend = TRUE, legendgroup = grp.name,
                                    line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
          else
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                    name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                    showlegend = TRUE, legendgroup = grp.name,
                                    line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          
        }
        ## Highlight selected Simulations in tab:models
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.chart.type))) { # Make an enveloppe instead of lines
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),name = 'High',showlegend = TRUE) %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',showlegend = TRUE,
                    fill = 'tonexty', fillcolor='rgba(135,206,250,0.2)', line = list(color = 'transparent'),name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',line = list(color='rgb(135,206,250)'),
                    name = 'Ens. Mean',showlegend = TRUE) 
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
        
        if (input$gcm.legend.sc == 'Hide') 
          p.sc <- p.sc %>% layout(showlegend = FALSE)
        
      } else if (grepl('box',tolower(input$gcm.chart.type))) {
        p.sc <- plot_ly(df, type = 'box')
        
        month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
        col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                     'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                     'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                     'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                     'rgb(166,206,227)')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          leg.grp <- month.grp[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color=col.grp[",i,"],opacity=0.6),
                                  name = leg.name,showlegend =TRUE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                       line = list(color = 'black', dash = 'dash', width = 2),
                                       legendgroup = leg.grp,
                                       showlegend = TRUE) 
        } 
      }
      
      
      ylab <- "Bias in simulated regional precipitation [%]"
      
      # Format layout 
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$gcm.region.pu),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      
      if (input$gcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      
      p.sc$elementId <- NULL
      p.sc
      })
    
    output$gcm.sc.pr.pu <- renderPlotly({
      gcm.meta.pr <- gcm.meta.pr.reactive.pu()
      df <- gcm.sc.pr.reactive.sc.pu()
      
      #df <- ((df - df[,dim(df)[2]])/df[,dim(df)[2]]) * 100
      
      
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      if (input$gcm.sc.chart.type.pu == 'Individual Simulations') {
        ## Make the plot
        p.sc <- plot_ly(df,x = ~month)
        
        gcms <- colnames(df)[grep('gcm',colnames(df))]
        id <- 1 : (length(df) - 1)
        lev <- levels(factor(id))
        
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        
        # Color by
        colsa <- rgbcolsa
        cols <- rgbcols
        # browser()
        ## Add all Simulations
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='')
          
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                  name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          
        }
        ## Highlight selected Simulations in tab:models
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.sc.chart.type.pu))) { # Make an enveloppe instead of lines
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),name = 'High',showlegend = TRUE) %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',showlegend = TRUE,
                    fill = 'tonexty', fillcolor='rgba(135,206,250,0.2)', line = list(color = 'transparent'),name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',line = list(color='rgb(135,206,250)'),
                    name = 'Ens. Mean',showlegend = TRUE) 
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
        
        if (input$gcm.legend.sc == 'Hide') 
          p.sc <- p.sc %>% layout(showlegend = FALSE)
        
      } else if (grepl('box',tolower(input$gcm.sc.chart.type.pu))) {
        p.sc <- plot_ly(df, type = 'box')
        
        month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
        col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                     'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                     'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                     'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                     'rgb(166,206,227)')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          leg.grp <- month.grp[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color=col.grp[",i,"],opacity=0.6),
                                  name = leg.name,showlegend =TRUE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                       line = list(color = 'black', dash = 'dash', width = 2),
                                       legendgroup = leg.grp,
                                       showlegend = TRUE) 
        } 
      }
      
      
      ylab <- "Seasonal Cycle of area averaged simulated monthly precipitation sums [mm/month]"
      
      # Format layout 
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$gcm.sc.region.pu),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      
      if (input$gcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      
      p.sc$elementId <- NULL
      p.sc
      })
    
    output$gcm.cc.pr.pu <- renderPlotly({
      
      gcm.meta.pr <- gcm.meta.pr.reactive.pu()
      
      df <- ((gcm.sc.pr.reactive.cc.pu() - gcm.sc.pr.present.cc.pu())/ gcm.sc.pr.present.cc.pu()) * 100
      
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('gcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      if (input$gcm.cc.chart.type == 'Individual Simulations') {
        ## Make the plot
        p.sc <- plot_ly(df,x = ~month)
        
        gcms <- colnames(df)[grep('gcm',colnames(df))]
        id <- 1 : (length(df) - 1)
        lev <- levels(factor(id))
        
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        
        # Color by
        colsa <- rgbcolsa
        cols <- rgbcols
        # browser()
        ## Add all Simulations
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='')
          
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                  name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          
        }
        ## Highlight selected Simulations in tab:models
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.cc.chart.type))) { # Make an enveloppe instead of lines
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),name = 'High',showlegend = TRUE) %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',showlegend = TRUE,
                    fill = 'tonexty', fillcolor='rgba(135,206,250,0.2)', line = list(color = 'transparent'),name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',line = list(color='rgb(135,206,250)'),
                    name = 'Ens. Mean',showlegend = TRUE) 
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
        
        if (input$gcm.legend.sc == 'Hide') 
          p.sc <- p.sc %>% layout(showlegend = FALSE)
        
      } else if (grepl('box',tolower(input$gcm.cc.chart.type))) {
        p.sc <- plot_ly(df, type = 'box')
        
        month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
        col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                     'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                     'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                     'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                     'rgb(166,206,227)')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          leg.grp <- month.grp[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color=col.grp[",i,"],opacity=0.6),
                                  name = leg.name,showlegend =TRUE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                       line = list(color = 'black', dash = 'dash', width = 2),
                                       legendgroup = leg.grp,
                                       showlegend = TRUE) 
        } 
      }
      
      
      ylab <- "Future changes in area averaged simulated monthly precipitation sums [mm/month]"
      
      # Format layout 
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$gcm.cc.region),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      
      if (input$gcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      
      p.sc$elementId <- NULL
      p.sc
      })
    
    output$gcm.sc.tas.data <- DT::renderDataTable({
      
      gcm.meta.tas <- gcm.meta.tas.reactive()
      df <- gcm.sc.tas.reactive()
      
      if (!is.null(input$rowsGcm))
        if (input$gcm.sim.sc == 'Selected Simulations')
          gcm.meta.tas <- gcm.meta.tas[input$rowsGcm,]
      
      if (input$gcm.outputValues == 'Bias')
        df <- df - df[,dim(df)[2]]      
      else if (input$gcm.outputValues == 'Anomaly')
        df <- df - colMeans(df)
      else if (input$gcm.outputValues == 'Change') {
        df <- gcm.sc.tas.reactive() - gcm.sc.tas.present()
      }
      caption <- paste('Monthly estimates of regional temperature assuming an 
                       intermediate emission scenarios for the',tolower(input$gcm.period),'averaged over',input$gcm.region,'region.
                       The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                       The last row in the table shows the estimated values from the referance data set (Observation).',
                       sep= ' ')
      
      if (input$gcm.outputValues == 'Bias') {
        df <- df - df[,dim(df)[2]] 
        caption <- paste('Bias [Deg. C] in monthly estimates of regional temperature assuming an 
                         intermediate emission scenarios for the',tolower(input$gcm.period),'averaged over',input$gcm.region,'region.
                         The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                         The bias is computed as the deviation from the referance data set (Observation).',
                         sep= ' ')
      } 
      df.format <- t(round(df,digits = 1))
      colnames(df.format) <- month.abb
      df.format <- data.frame(N = c(1:(dim(df.format)[1]-1),0), 
                              Model = c(as.character(as.vector(gcm.meta.tas$model_id)),'ERA'), 
                              Run = c(as.character(as.vector(gcm.meta.tas$parent_experiment_rip)),'ERA'),
                              Realization = c(as.character(as.vector(gcm.meta.tas$realization)),'ERA'),
                              df.format,stringsAsFactors = FALSE)
      
      DT::datatable(df.format,
                    caption = caption, 
                    selection = list(mode = 'multiple',target = 'row'), 
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
      gcm.meta.pr <- gcm.meta.pr.reactive()
      df <- gcm.sc.pr.reactive()
      
      if (!is.null(input$rowsGcm))
        if (input$gcm.sim.sc == 'Selected Simulations') 
          gcm.meta.pr <- gcm.meta.pr[input$rowsGcm,]
      
      if (input$gcm.outputValues == 'Absolute')
        
        caption <- paste('Monthly estimates of regional precipitation assuming an 
                         intermediate emission scenarios for the',tolower(input$gcm.period),'averaged over',input$gcm.region,'region.
                         The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                         The last row in the table shows the estimated values from the referance data set (Observation).',
                         sep= ' ')
      else if (input$gcm.outputValues == 'Anomaly') {
        df <- df - colMeans(df)
        caption <- paste('Monthly anomalies in estimated regional precipitation assuming an 
                         intermediate emission scenarios for the',tolower(input$gcm.period),'averaged over',input$gcm.region,'region.
                         The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                         The last row in the table shows the estimated values from the referance data set (Observation).',
                         sep= ' ')
      } else if (input$gcm.outputValues == 'Bias') {
        df <- (df - df[,dim(df)[2]])/df[,dim(df)[2]] * 100 
        caption <- paste('Bias [in %] in monthly estimates of regional preciptiation assuming an 
                         intermediate emission scenarios for the',tolower(input$gcm.period),'averaged over',input$gcm.region,'region.
                         The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                         The bias is computed as the deviation from the referance data set (Observation).',
                         sep= ' ')
      }  else if (input$gcm.outputValues == 'Change') {
        df <- ((gcm.sc.pr.reactive() - gcm.sc.pr.present())/ gcm.sc.pr.present()) * 100
        caption <- paste('Change [in %] in monthly estimates of regional preciptiation assuming an 
                         intermediate emission scenarios for the',tolower(input$gcm.period),'averaged over',input$gcm.region,'region.
                         The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                         The changes are computed as the deviation from the historical simulations for the present (1981-2010).',
                         sep= ' ')
      }
      df.format <- t(round(df,digits = 1))
      colnames(df.format) <- month.abb
      df.format <- data.frame(N = c(1:(dim(df.format)[1]-1),0), 
                              Model = c(as.character(as.vector(gcm.meta.pr$model_id)),'ERA'), 
                              Run = c(as.character(as.vector(gcm.meta.pr$parent_experiment_rip)),'ERA'),
                              Realization = c(as.character(as.vector(gcm.meta.pr$realization)),'ERA'),
                              df.format,stringsAsFactors = FALSE)
      
      DT::datatable(caption = caption, 
                    df.format,
                    selection = list(mode = 'multiple',target = 'row'), 
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
    
    ## Seasonal scatter plot DT vs DP 
    output$gcm.scatter <- renderPlotly({
      #
      gcm.meta.pr <- gcm.meta.pr.reactive()
      dpr <- gcm.sc.pr.reactive()
      gcms <- names(dpr)
      if (input$gcm.outputValues == 'Bias')
        dpr <- ((dpr - dpr[,dim(dpr)[2]]) / dpr[,dim(dpr)[2]]) * 100
      else if (input$gcm.outputValues == 'RMSE') {
        dpr <- sqrt((((dpr - dpr[,dim(dpr)[2]]) / dpr[,dim(dpr)[2]]))^2) * 100
      } else if (input$gcm.outputValues == 'Anomaly') {
        DF <- t(dpr)
        dpr <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$gcm.outputValues == 'Change') {
        dpr <- ((dpr - gcm.sc.pr.present())/ gcm.sc.pr.present()) * 100
      } 
      
      gcm.meta.tas <- gcm.meta.tas.reactive()
      
      dtas <- gcm.sc.tas.reactive()
      if (input$gcm.outputValues == 'Bias')
        dtas <- dtas - dtas[,dim(dtas)[2]]
      else if (input$gcm.outputValues == 'RMSE') {
        dtas <- sqrt((dtas-dtas[,dim(dtas)[2]])^2)
      } else if (input$gcm.outputValues == 'Anomaly') {
        DF <- t(dtas)
        dtas <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$gcm.outputValues == 'Change') {
        dtas <- dtas - gcm.sc.tas.present()
      }
      
      if (!is.null(input$rowsGcm))
        if (input$gcm.sim.sc == 'Selected Simulations') {
          dpr <- dpr[input$rowsGcm,]
          dtas <- dtas[input$rowsGcm,]
          gcm.meta.tas <- gcm.meta.tas[input$rowsGcm,]
          gcm.meta.pr <- gcm.meta.pr[input$rowsGcm,]
          gcms <- gcms[input$rowsGcm]
        } 
      
      gcm.name <- function(i) {
        gcmi <- gcm.meta.pr[i,c('model_id','parent_experiment_rip','realization')]
        gcmi[is.na(gcmi)] <- ''
        return(paste(as.character(as.matrix(gcmi)),collapse = '_'))
      }
      inst.name <- function(i) {
        gcmi <- gcm.meta.pr[i,c('model_id')]
        gcmi[is.na(gcmi)] <- ''
        return(paste(as.character(as.matrix(gcmi)),collapse = '_'))
      }
      gcmall <- c(sapply(1:dim(gcm.meta.pr)[1],gcm.name),'ERAINT')
      gcm.inst <- c(sapply(1:dim(gcm.meta.pr)[1],inst.name),'ERAINT')
      
      df <- data.frame(dtas = as.numeric(round(colMeans(dtas),digits = 2)),dpr = as.numeric(round(colMeans(dpr),digits = 2)),
                       gcm.name = gcmall, inst.name = gcm.inst,stringsAsFactors = FALSE)
      
      if (is.element(input$gcm.groupBy,c('None','---'))) {
        id <- 1 : (length(df$dtas) - 1)
        lev <- levels(factor(id))
      }
      else { 
        id <- as.integer(factor(base::subset(gcm.meta.pr, select = input$gcm.groupBy)[[1]]))
        lev <- levels(factor(base::subset(gcm.meta.pr, select = input$gcm.groupBy)[[1]]))
      }
      
      rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                    'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                    'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                    'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                    'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                    'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                    'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                    'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
      
      rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                   'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                   'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                   'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                   'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                   'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                   'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
      
      
      # Color by
      if (input$gcm.colorBy == 'Group') {
        colsa <- rgbcolsa[id]
        cols <- rgbcols[id]
      } else {
        colsa <- rgbcolsa
        cols <- rgbcols
      }
      
      ## Create the plot
      p.sc <- plot_ly(df)
      if ((input$gcm.chart.type == 'Individual Simulations')) {
        ## Add all Simulations
        if (is.null(input$rowsGcm)) {
          for (gcm in gcms) {
            i <- which(is.element(gcms,gcm))
            #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
            leg.name <- paste(paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '),
                              paste(substr(lev[id[i]],1,5),'...',sep=''),
                              sep = ' ')
            grp.name <- paste('Group',id[i],sep='')
            
            if (is.element(input$gcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = ",i,", symbol = 3,size = 12,opacity = 0.7,line = list(width = 2,color = '#FFFFFF')))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = cols[",i,"], symbol = 3,size = 12,opacity=0.7,line = list(width = 2,color = '#FFFFFF')))",sep='')))
            
          }
          ## Highlight selected Simulations in tab:models
        } else {
          im <- input$rowsGcm
          for (i in 1:length(im)) {
            leg.name <- paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' ')
            grp.name <- paste('Group',id[i],sep='')
            gcm <- gcms[i]
            if (is.element(input$gcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = ",i,", symbol = 3,size = 12,opacity = 0.7,line = list(width = 1)))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = cols[im[",i,"]], symbol = 3,size = 12,opacity=0.7,line = list(width = 1))",sep='')))
          }
          }
          } 
      
      if (length(df$dpr) > 1 | length(df$dtas) > 1) {
        # p.sc <-  p.sc  %>%  layout(shapes = list(list(name = 'Env. 90% of all sim',type = 'circle',
        #                                              xref = 'x', x0 = quantile(df$dtas[-length(df$dtas)],probs = 0.05,na.rm = TRUE), x1 = quantile(df$dtas[-length(df$dtas)],0.95,na.rm=TRUE),
        #                                              yref = 'y', y0 = quantile(df$dpr[-length(df$dpr)],0.05,na.rm=TRUE), y1 = quantile(df$dpr[-length(df$dpr)],0.95,na.rm=TRUE),
        #                                              fillcolor = 'rgba(255,127,80,0.4)', line = list(color = 'rgba(255,127,80,0.8)'),
        #                                              opacity = 0.4),
        #                                         list(name = 'Env. of all sim.',type = 'circle',
        #                                              xref = 'x', x0 = min(df$dtas[-length(df$dtas)],na.rm=TRUE), x1 = max(df$dtas[-length(df$dtas)],na.rm=TRUE),
        #                                              yref = 'y', y0 = min(df$dpr[-length(df$dpr)],na.rm=TRUE), y1 = max(df$dpr[-length(df$dpr)],na.rm=TRUE),
        #                                              fillcolor = 'rgba(255,127,80,0.4)', line = list(color = 'rgba(255,127,80,0.8)'),
        #                                              opacity = 0.3)))
        #
        dfe.70 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.6827,draw = FALSE)
        dfe.95 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9545,draw = FALSE)
        dfe.99 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9973,draw = FALSE)
        p.sc <- p.sc %>% 
          add_polygons(x = dfe.99[,1],y = dfe.99[,2] , name = '99% Confidence Level',
                       fillcolor = 'rgba(255,127,80,0.5)',
                       line = list(color = 'rgba(255,127,80,0.6)'),
                       opacity = 0.5) %>%
          add_polygons(x = dfe.95[,1],y = dfe.95[,2] , name = '95% Confidence Level',
                       fillcolor = 'rgba(255,127,80,0.6)',
                       line = list(color = 'rgba(255,127,80,0.7)'),
                       opacity = 0.7) %>% 
          add_polygons(x = dfe.70[,1],y = dfe.70[,2] , name = '70% Confidence Level',
                       fillcolor = 'rgba(255,127,80,0.7)',
                       line = list(color = 'rgba(255,127,80,0.8)'),
                       opacity = 0.8) 
      }
      # p.sc <- p.sc  %>% layout(shapes = list(list(name = 'Env. of all sim.',type = 'circle',
      #                                             xref = 'x', 
      #                                             x0 = mean(df$dtas[-length(df$dtas)],na.rm=TRUE) - sd(df$dtas[-length(df$dtas)],na.rm=TRUE), 
      #                                             x1 = mean(df$dtas[-length(df$dtas)],na.rm=TRUE) + sd(df$dtas[-length(df$dtas)],na.rm=TRUE),
      #                                             yref = 'y', 
      #                                             y0 = mean(df$dpr[-length(df$dpr)],na.rm=TRUE) - sd(df$dpr[-length(df$dpr)],na.rm=TRUE), 
      #                                             y1 = mean(df$dpr[-length(df$dpr)],na.rm=TRUE) + sd(df$dpr[-length(df$dpr)],na.rm=TRUE),
      #                                             fillcolor = 'rgba(255,127,80,0.4)', line = list(color = 'rgba(255,127,80,0.8)'),
      #                                             opacity = 0.4),
      #                                        list(name = 'Env. of all sim.',type = 'circle',
      #                                             xref = 'x', 
      #                                             x0 = mean(df$dtas[-length(df$dtas)],na.rm=TRUE) - 2 * sd(df$dtas[-length(df$dtas)],na.rm=TRUE), 
      #                                             x1 = mean(df$dtas[-length(df$dtas)],na.rm=TRUE) + 2 * sd(df$dtas[-length(df$dtas)],na.rm=TRUE),
      #                                             yref = 'y', 
      #                                             y0 = mean(df$dpr[-length(df$dpr)],na.rm=TRUE) - 2 * sd(df$dpr[-length(df$dpr)],na.rm=TRUE), 
      #                                             y1 = mean(df$dpr[-length(df$dpr)],na.rm=TRUE) + 2 * sd(df$dpr[-length(df$dpr)],na.rm=TRUE),
      #                                             fillcolor = 'rgba(255,127,80,0.4)', line = list(color = 'rgba(255,127,80,0.8)'),
      #                                             opacity = 0.3)))
      if ((input$gcm.chart.type == 'Ensemble of All Simulations') | (input$gcm.chart.type == "Both - Ensemble & Individual Simulations"))
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
      #}
      
      if (input$gcm.legend.sc == 'Display') {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = TRUE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'ERAINT', hoverinfo = 'text+x+y',text='ERAINT',showlegend = TRUE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      } else {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = FALSE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'ERAINT', hoverinfo = 'text+x+y',text='ERAINT',showlegend = FALSE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      }
      if (input$gcm.outputValues == 'Bias') {
        ylab <- 'Bias (absolute) in annual means of regional temperature values [deg. C]'
        xlab <- 'Bias (relative) in annual means of regional precitation values [%]'
      } else if (input$gcm.outputValues == 'Bias') {
        ylab <- 'RMSE (absolute) in annual means of regional temperature values [deg. C]'
        xlab <- 'RMSE (relative) in annual means of regional precitation values [%]'
      } else if (input$gcm.outputValues == 'Anomaly') {
        ylab <- 'Anomaly (absolute) in annual means of regional temperature values [deg. C]'
        xlab <- 'Anomaly (relative) in annual means of regional precitation values [%]'
      } else if (input$gcm.outputValues == 'Change') {
        ylab <- 'Change (absolute) in annual means of regional temperature values [deg. C]'
        xlab <- 'Change (relative) in annual means of regional precitation values [%]'
      } else  {
        ylab <- 'Annual means of regional temperature values [deg. C]'
        xlab <- 'Annual means of regional precitation values [mm/month]'
      }
      
      
      
      p.sc <- p.sc %>% layout(title = paste('Region: ',input$gcm.region),
        paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
        xaxis = list(title = ylab,
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     tickcolor = 'rgb(127,127,127)',
                     ticks = 'outside',
                     zeroline = TRUE),
        yaxis = list(title = xlab,
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     tickcolor = 'rgb(127,127,127)',
                     ticks = 'outside',
                     zeroline = TRUE))
      
      if (input$gcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      else
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      #gcm.dtdp$elementId <- NULL
      # gcm.dtdp
      p.sc$elementId <- NULL
      p.sc
          })
    
    output$gcm.cc.scatter.pu <- renderPlotly({
      #
      gcm.meta.pr <- gcm.meta.pr.reactive.pu()
      dpr <- gcm.sc.pr.reactive.cc.pu()
      gcms <- names(dpr)
      dpr <- ((dpr - gcm.sc.pr.present())/ gcm.sc.pr.present()) * 100
      
      gcm.meta.tas <- gcm.meta.tas.reactive.cc.pu()
      dtas <- gcm.sc.tas.reactive.cc.pu() - gcm.sc.tas.present.cc.pu()
      
      gcm.name <- function(i) {
        gcmi <- gcm.meta.pr[i,c('model_id','parent_experiment_rip','realization')]
        gcmi[is.na(gcmi)] <- ''
        return(paste(as.character(as.matrix(gcmi)),collapse = '_'))
      }
      inst.name <- function(i) {
        gcmi <- gcm.meta.pr[i,c('model_id')]
        gcmi[is.na(gcmi)] <- ''
        return(paste(as.character(as.matrix(gcmi)),collapse = '_'))
      }
      gcmall <- c(sapply(1:dim(gcm.meta.pr)[1],gcm.name),'ERAINT')
      gcm.inst <- c(sapply(1:dim(gcm.meta.pr)[1],inst.name),'ERAINT')
      
      df <- data.frame(dtas = as.numeric(round(colMeans(dtas),digits = 2)),dpr = as.numeric(round(colMeans(dpr),digits = 2)),
                       gcm.name = gcmall, inst.name = gcm.inst,stringsAsFactors = FALSE)
      
      id <- 1 : (length(df$dtas) - 1)
      lev <- levels(factor(id))
      
      rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                    'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                    'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                    'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                    'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                    'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                    'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                    'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
      
      rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                   'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                   'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                   'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                   'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                   'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                   'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
      
      
      # Color by
      colsa <- rgbcolsa[id]
      cols <- rgbcols[id]
      
      ## Create the plot
      p.sc <- plot_ly(df)
      if ((input$gcm.cc.chart.type == 'Individual Simulations')) {
        ## Add all Simulations
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='')
          
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                  name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  marker = list(color = ",i,", symbol = 3,size = 12,opacity = 0.7,line = list(width = 2,color = '#FFFFFF')))",sep='')))
        }
        ## Highlight selected Simulations in tab:models
        } 
      
      if (length(df$dpr) > 1 | length(df$dtas) > 1) {
        # p.sc <-  p.sc  %>%  layout(shapes = list(list(name = 'Env. 90% of all sim',type = 'circle',
        #                                              xref = 'x', x0 = quantile(df$dtas[-length(df$dtas)],probs = 0.05,na.rm = TRUE), x1 = quantile(df$dtas[-length(df$dtas)],0.95,na.rm=TRUE),
        #                                              yref = 'y', y0 = quantile(df$dpr[-length(df$dpr)],0.05,na.rm=TRUE), y1 = quantile(df$dpr[-length(df$dpr)],0.95,na.rm=TRUE),
        #                                              fillcolor = 'rgba(255,127,80,0.4)', line = list(color = 'rgba(255,127,80,0.8)'),
        #                                              opacity = 0.4),
        #                                         list(name = 'Env. of all sim.',type = 'circle',
        #                                              xref = 'x', x0 = min(df$dtas[-length(df$dtas)],na.rm=TRUE), x1 = max(df$dtas[-length(df$dtas)],na.rm=TRUE),
        #                                              yref = 'y', y0 = min(df$dpr[-length(df$dpr)],na.rm=TRUE), y1 = max(df$dpr[-length(df$dpr)],na.rm=TRUE),
        #                                              fillcolor = 'rgba(255,127,80,0.4)', line = list(color = 'rgba(255,127,80,0.8)'),
        #                                              opacity = 0.3)))
        #
        dfe.70 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.6827,draw = FALSE)
        dfe.95 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9545,draw = FALSE)
        dfe.99 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9973,draw = FALSE)
        p.sc <- p.sc %>% 
          add_polygons(x = dfe.99[,1],y = dfe.99[,2] , name = '99% Confidence Level',
                       fillcolor = 'rgba(255,127,80,0.5)',
                       line = list(color = 'rgba(255,127,80,0.6)'),
                       opacity = 0.5) %>%
          add_polygons(x = dfe.95[,1],y = dfe.95[,2] , name = '95% Confidence Level',
                       fillcolor = 'rgba(255,127,80,0.6)',
                       line = list(color = 'rgba(255,127,80,0.7)'),
                       opacity = 0.7) %>% 
          add_polygons(x = dfe.70[,1],y = dfe.70[,2] , name = '70% Confidence Level',
                       fillcolor = 'rgba(255,127,80,0.7)',
                       line = list(color = 'rgba(255,127,80,0.8)'),
                       opacity = 0.8) 
      }
      if ((input$gcm.cc.chart.type == 'Ensemble of All Simulations') | (input$gcm.cc.chart.type == "Both - Ensemble & Individual Simulations"))
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
      #}
      
      if (input$gcm.legend.sc == 'Display') {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = TRUE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'ERAINT', hoverinfo = 'text+x+y',text='ERAINT',showlegend = TRUE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      } else {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = FALSE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'ERAINT', hoverinfo = 'text+x+y',text='ERAINT',showlegend = FALSE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      }
      ylab <- 'Change (absolute) in annual means of regional temperature values [deg. C]'
      xlab <- 'Change (relative) in annual means of regional precitation values [%]'
      
      
      
      p.sc <- p.sc %>% layout(title = paste('Region: ', input$gcm.cc.region),
        paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
        xaxis = list(title = ylab,
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     tickcolor = 'rgb(127,127,127)',
                     ticks = 'outside',
                     zeroline = TRUE),
        yaxis = list(title = xlab,
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     tickcolor = 'rgb(127,127,127)',
                     ticks = 'outside',
                     zeroline = TRUE))
      
      if (input$gcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      else
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      #gcm.dtdp$elementId <- NULL
      # gcm.dtdp
      p.sc$elementId <- NULL
      p.sc
      })
    
    output$gcm.scatter.data <- DT::renderDataTable({
      
      gcm.meta.pr <- gcm.meta.pr.reactive()
      dpr <- gcm.sc.pr.reactive()
      gcms <- names(dpr)
      if (input$gcm.outputValues == 'Bias')
        dpr <- ((dpr - dpr[,dim(dpr)[2]]) / dpr[,dim(dpr)[2]]) * 100
      else if (input$gcm.outputValues == 'RMSE') {
        dpr <- sqrt((((dpr - dpr[,dim(dpr)[2]]) / dpr[,dim(dpr)[2]]))^2) * 100
      } else if (input$gcm.outputValues == 'Anomaly') {
        DF <- t(dpr)
        dpr <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$gcm.outputValues == 'Change') {
        dpr <- ((dpr - gcm.sc.pr.present())/ gcm.sc.pr.present()) * 100
      } 
      
      gcm.meta.tas <- gcm.meta.tas.reactive()
      
      dtas <- gcm.sc.tas.reactive()
      if (input$gcm.outputValues == 'Bias')
        dtas <- dtas - dtas[,dim(dtas)[2]]
      else if (input$gcm.outputValues == 'RMSE') {
        dtas <- sqrt((dtas-dtas[,dim(dtas)[2]])^2)
      } else if (input$gcm.outputValues == 'Anomaly') {
        DF <- t(dtas)
        dtas <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$gcm.outputValues == 'Change') {
        dtas <- dtas - gcm.sc.tas.present()
      }
      
      if (!is.null(input$rowsGcm))
        if (input$gcm.sim.sc == 'Selected Simulations') {
          dpr <- dpr[input$rowsGcm,]
          dtas <- dtas[input$rowsGcm,]
          gcm.meta.tas <- gcm.meta.tas[input$rowsGcm,]
          gcm.meta.pr <- gcm.meta.pr[input$rowsGcm,]
          gcms <- gcms[input$rowsGcm]
        } 
      
      gcm.name <- function(i) {
        gcmi <- gcm.meta.pr[i,c('model_id','parent_experiment_rip','realization')]
        gcmi[is.na(gcmi)] <- ''
        return(paste(as.character(as.matrix(gcmi)),collapse = '_'))
      }
      inst.name <- function(i) {
        gcmi <- gcm.meta.pr[i,c('model_id')]
        gcmi[is.na(gcmi)] <- ''
        return(paste(as.character(as.matrix(gcmi)),collapse = '_'))
      }
      gcmall <- c(sapply(1:dim(gcm.meta.pr)[1],gcm.name),'ERAINT')
      gcm.inst <- c(sapply(1:dim(gcm.meta.pr)[1],inst.name),'ERAINT')
      
      df <- data.frame(dtas = as.numeric(round(colMeans(dtas),digits = 2)),dpr = as.numeric(round(colMeans(dpr),digits = 2)),
                       gcm.name = gcmall, inst.name = gcm.inst,stringsAsFactors = FALSE)
      
      caption <- paste('Annual means of monthly estimates of both regional temperature (deg. C) and precipitation (mm/month) assuming an 
                       intermediate emission scenarios for the',tolower(input$gcm.period),'averaged over',input$gcm.region,'region.
                       The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                       The last row in the table shows the estimated values from the referance data set (last row).',
                       sep= ' ')
      
      if (input$gcm.outputValues == 'Bias') {
        caption <- paste('Annual means of Biases in monthly estimates of both regional temperature (deg. C) and precipitation (%) assuming an 
                         intermediate emission scenarios for the',tolower(input$gcm.period),'averaged over',input$gcm.region,'region.
                         The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                         The bias is computed as the deviation from the referance data set (last row).',
                         sep= ' ')
      } else if (input$gcm.outputValues == 'RMSE'){
        caption <- paste('Annual means of RMSE in monthly estimates of both regional temperature (deg. C) and precipitation (%) assuming an 
                         intermediate emission scenarios for the',tolower(input$gcm.period),'averaged over',input$gcm.region,'region.
                         The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                         The RMSE is computed as the square root mean of deviations from the referance data set (last row).',
                         sep= ' ')
        
      } else if (input$gcm.outputValues == 'Change'){
        caption <- paste('Annual means of Changes in monthly estimates of both regional temperature (deg. C) and precipitation (%) assuming an 
                         intermediate emission scenarios for the',tolower(input$gcm.period),'averaged over',input$gcm.region,'region.
                         The climate models and their corresponding runs are listed in the second column, respectively. 
                         The RMSE is computed as the square root mean of deviations from the referance data set (last row).',
                         sep= ' ')
        
      }
      
      #df.format <- t(round(df,digits = 1))
      #colnames(df.format) <- month.abb
      df.format <- data.frame(N = c(1:dim(df)[1]), 
                              Model = df[,3], 
                              Temperature = df[,1],
                              Preciptiation = df[,2],
                              stringsAsFactors = FALSE)
      
      DT::datatable(df.format,
                    caption = caption, 
                    selection = list(mode = 'multiple',target = 'row'), 
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
    # RCM Seasonal cycle expression and output
    rcm.sc.tas <- renderPlotly({
      
      rcm.meta.tas <- rcm.meta.tas.reactive()
      df <- rcm.sc.tas.reactive()
      
      if (input$rcm.sim.sc == 'Selected Simulations') {
        rcm.meta.tas <- rcm.meta.tas[input$rowsRcm,]
      }
      
      if (input$rcm.outputValues == 'Bias')
        df <- df - df[,dim(df)[2]]      
      else if (input$rcm.outputValues == 'Anomaly') {
        DF <- t(df)
        df <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$rcm.outputValues == 'Change') {
        df <- rcm.sc.tas.reactive() - rcm.sc.tas.present()
      }
      
      #df <- df[,-36] # AM Quick fix but has to be removed ... once meta is updated.
      # rcm seasonal cycle
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      
      # define layout output
      #df$ref <- df$ref
      if (input$rcm.chart.type == 'Individual Simulations') {
        ## Make the plot
        #p.sc <- plot_ly(df, x = ~month, y = ~rcm.1,type = 'scatter',mode = 'markers+lines', line = list(width = 2, color = "grey",shape = 'spline'))
        # create plot_ly
        p.sc <- plot_ly(df, x = ~month)
        
        rcms <- colnames(df)[grep('rcm',colnames(df))]
        if (is.element(input$rcm.groupBy,c('None','---'))) {
          id <- 1 : (length(df) - 1)
          lev <- levels(factor(id))
        }
        else { 
          id <- as.integer(factor(base::subset(rcm.meta.tas, select = input$rcm.groupBy)[[1]]))
          lev <- levels(factor(base::subset(rcm.meta.tas, select = input$rcm.groupBy)[[1]]))
        }
        
        if (length(lev) > 50) {
          # 108 distinct colors
          rgbcols <- c('rgb(255,128,128)', 'rgb(178,89,89)', 'rgb(51,26,26)', 'rgb(217,123,108)', 'rgb(140,79,70)', 'rgb(89,51,45)', 'rgb(64,36,32)', 'rgb(242,153,121)', 'rgb(178,113,89)', 
                       'rgb(102,65,51)', 'rgb(76,48,38)', 'rgb(51,32,26)', 'rgb(217,152,108)', 'rgb(153,107,77)', 'rgb(102,71,51)', 'rgb(76,54,38)', 'rgb(229,176,115)', 'rgb(178,137,89)',
                       'rgb(140,108,70)', 'rgb(102,78,51)', 'rgb(51,39,26)', 'rgb(217,181,108)', 'rgb(140,117,70)', 'rgb(76,64,38)', 'rgb(178,161,89)', 'rgb(102,92,51)', 'rgb(51,46,26)', 
                       'rgb(229,222,115)', 'rgb(173,179,89)', 'rgb(74,77,38)', 'rgb(229,255,128)', 'rgb(126,140,70)', 'rgb(46,51,26)', 'rgb(170,204,102)', 'rgb(85,102,51)', 'rgb(186,242,121)', 
                       'rgb(127,166,83)', 'rgb(81,128,64)', 'rgb(116,204,102)', 'rgb(127,255,128)', 'rgb(83,166,94)', 'rgb(51,102,58)', 'rgb(38,77,43)', 'rgb(26,51,29)', 'rgb(115,230,161)', 
                       'rgb(77,153,107)', 'rgb(64,128,98)', 'rgb(45,89,68)', 'rgb(102,204,170)', 'rgb(26,51,43)', 'rgb(121,242,218)', 'rgb(83,166,149)', 'rgb(51,102,92)', 'rgb(38,77,69)',
                       'rgb(102,204,197)', 'rgb(121,234,242)', 'rgb(83,160,166)', 'rgb(57,111,115)', 'rgb(38,74,77)', 'rgb(26,49,51)', 'rgb(108,195,217)', 'rgb(77,138,153)', 'rgb(121,202,242)', 
                       'rgb(89,149,179)', 'rgb(70,117,140)', 'rgb(51,85,102)', 'rgb(32,53,64)', 'rgb(128,196,255)', 'rgb(102,156,204)', 'rgb(64,98,128)', 'rgb(128,179,255)', 'rgb(102,143,204)', 
                       'rgb(45,62,89)', 'rgb(26,36,51)', 'rgb(121,153,242)', 'rgb(77,97,153)', 'rgb(128,145,255)', 'rgb(89,101,179)', 'rgb(64,72,128)', 'rgb(45,51,89)', 'rgb(32,36,64)', 'rgb(108,108,217)', 			      'rgb(58,51,102)', 'rgb(162,128,255)', 'rgb(113,89,179)', 'rgb(89,70,140)', 'rgb(48,38,77)', 'rgb(39,26,51)', 'rgb(128,77,153)', 'rgb(74,45,89)', 'rgb(172,96,191)', 
                       'rgb(247,128,255)', 'rgb(111,57,115)', 'rgb(62,32,64)', 'rgb(242,121,218)', 'rgb(191,96,172)', 'rgb(89,45,80)', 'rgb(51,26,46)', 'rgb(128,64,106)', 'rgb(166,83,127)', 
                       'rgb(242,121,170)', 'rgb(102,51,71)', 'rgb(51,26,36)', 'rgb(191,96,121)', 'rgb(128,64,81)', 'rgb(77,38,48)', 'rgb(229,115,130)', 'rgb(153,77,87)', 'rgb(102,51,58)')
          
          # 108 distinct colors with transparency
          rgbcolsa <- c('rgba(255,128,128,0.5)', 'rgba(178,89,89,0.5)', 'rgba(51,26,26,0.5)', 'rgba(217,123,108,0.5)', 'rgba(140,79,70,0.5)', 'rgba(89,51,45,0.5)', 'rgba(64,36,32,0.5)', 
                        'rgba(242,153,121,0.5)', 'rgba(178,113,89,0.5)', 'rgba(102,65,51,0.5)', 'rgba(76,48,38,0.5)', 'rgba(51,32,26,0.5)', 'rgba(217,152,108,0.5)', 'rgba(153,107,77,0.5)', 'rgba(102,71,51,0.5)', 'rgba(76,54,38,0.5)', 'rgba(229,176,115,0.5)', 'rgba(178,137,89,0.5)', 'rgba(140,108,70,0.5)', 'rgba(102,78,51,0.5)', 'rgba(51,39,26,0.5)', 'rgba(217,181,108,0.5)', 'rgba(140,117,70,0.5)', 'rgba(76,64,38,0.5)', 'rgba(178,161,89,0.5)', 'rgba(102,92,51,0.5)', 'rgba(51,46,26,0.5)', 
                        'rgba(229,222,115,0.5)', 'rgba(173,179,89,0.5)', 'rgba(74,77,38,0.5)', 'rgba(229,255,128,0.5)', 'rgba(126,140,70,0.5)', 'rgba(46,51,26,0.5)', 'rgba(170,204,102,0.5)', 'rgba(85,102,51,0.5)', 'rgba(186,242,121,0.5)', 'rgba(127,166,83,0.5)', 'rgba(81,128,64,0.5)', 'rgba(116,204,102,0.5)', 'rgba(127,255,128,0.5)', 'rgba(83,166,94,0.5)', 'rgba(51,102,58,0.5)', 'rgba(38,77,43,0.5)', 'rgba(26,51,29,0.5)', 'rgba(115,230,161,0.5)', 'rgba(77,153,107,0.5)', 'rgba(64,128,98,0.5)', 'rgba(45,89,68,0.5)', 'rgba(102,204,170,0.5)', 'rgba(26,51,43,0.5)', 'rgba(121,242,218,0.5)', 'rgba(83,166,149,0.5)', 'rgba(51,102,92,0.5)', 'rgba(38,77,69,0.5)', 'rgba(102,204,197,0.5)', 'rgba(121,234,242,0.5)', 'rgba(83,160,166,0.5)', 'rgba(57,111,115,0.5)', 'rgba(38,74,77,0.5)', 'rgba(26,49,51,0.5)', 'rgba(108,195,217,0.5)', 'rgba(77,138,153,0.5)', 'rgba(121,202,242,0.5)', 'rgba(89,149,179,0.5)', 'rgba(70,117,140),0.5', 'rgba(51,85,102,0.5)', 'rgba(32,53,64,0.5)', 'rgba(128,196,255,0.5)', 'rgba(102,156,204,0.5)', 'rgba(64,98,128,0.5)', 'rgba(128,179,255,0.5)', 'rgba(102,143,204,0.5)', 'rgba(45,62,89,0.5)', 'rgba(26,36,51,0.5)', 'rgba(121,153,242,0.5)', 'rgba(77,97,153,0.5)', 'rgba(128,145,255,0.5)', 'rgba(89,101,179,0.5)', 'rgba(64,72,128,0.5)', 'rgba(45,51,89,0.5)', 'rgba(32,36,64,0.5)', 'rgba(108,108,217,0.5)', 'rgba(58,51,102,0.5)', 'rgba(162,128,255,0.5)', 'rgba(113,89,179,0.5)', 'rgba(89,70,140)', 'rgba(48,38,77,0.5)', 'rgba(39,26,51,0.5)', 'rgba(128,77,153,0.5)', 'rgba(74,45,89)', 'rgba(172,96,191,0.5)', 
                        'rgba(247,128,255,0.5)', 'rgba(111,57,115,0.5)', 'rgba(62,32,64,0.5)', 'rgba(242,121,218,0.5)', 'rgba(191,96,172,0.5)', 'rgba(89,45,80,0.5)', 'rgba(51,26,46,0.5)', 'rgba(128,64,106,0.5)', 'rgba(166,83,127,0.5)', 
                        'rgba(242,121,170,0.5)', 'rgba(102,51,71,0.5)', 'rgba(51,26,36,0.5)', 'rgba(191,96,121,0.5)', 'rgba(128,64,81,0.5)', 'rgba(77,38,48,0.5)', 'rgba(229,115,130,0.5)', 'rgba(153,77,87,0.5)', 'rgba(102,51,58,0.5)')
        } else {
          rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                        'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                        'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                        'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                        'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                        'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                        'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                        'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
          
          rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                       'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                       'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                       'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                       'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                       'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                       'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        }
        ## Same as precip ...
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        ## color by groups
        if (input$rcm.colorBy == 'Group') {
          colsa <- rgbcolsa[id]
          cols <- rgbcols[id]
        } else {
          colsa <- rgbcolsa
          cols <- rgbcols
        }
        
        # Add all models
        if (is.null(input$rowsRcm)) {
          for (rcm in rcms) {
            i <- which(is.element(rcms,rcm))
            leg.name <- paste(paste(as.character(as.matrix(rcm.meta.tas[i,c('gcm','gcm_rip','rcm')])),collapse = ' '),
                              paste(substr(lev[id[i]],1,5),'...',sep=''),
                              sep = ' ')
            grp.name <- paste('Group',id[i],sep='')
            
            
            #if (is.null(input$rowsRcm)) {
            if (is.element(input$rcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = paste(",i,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name, 
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter',
                                      name = paste(",i,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
            #}
            ## Highlight selected models in tab:models
          } 
        } else {
          im <- input$rowsRcm
          for (i in 1:length(im)) {
            leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('gcm','gcm_rip','rcm')])),collapse = '  ')
            grp.name <- paste('Group',id[im[i]],sep='')
            rcm <- rcms[i] #rcms[im[i]]
            if (is.element(input$rcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = paste(im[",i,"],substr(leg.name,1,5),'...',sep=' '), mode = 'lines', legendgroup = grp.name, 
                                      colors = ",i,", hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = paste(im[",i,"],substr(leg.name,1,5),'...',sep=' '), mode = 'lines', legendgroup = grp.name,
                                      colors = colsa[im[",i,"]], hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = colsa[im[",i,"]], width = 2, shape ='spline'))",sep='')))
          }
          }
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
          } else if (grepl('ensemble', tolower(input$rcm.chart.type))) { # Make an enveloppe instead of lines
            
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
              p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'EOBS', mode = 'lines', 
                                         line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
            
            
            
          } else if (grepl('box',tolower(input$rcm.chart.type))) {
            p.sc <- plot_ly(df, type = 'box')
            
            month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
            col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                         'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                         'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                         'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                         'rgb(166,206,227)')
            for (i in 1:12) {
              leg.name <- month.abb[i]
              leg.grp <- month.grp[i]
              eval(parse(text = paste("p.sc <- p.sc %>% 
                                      add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                      type = 'box', boxpoints = 'all',
                                      legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color=col.grp[",i,"],opacity=0.6),
                                      name = leg.name,showlegend =TRUE)",sep='')))
              if (!is.null(df$ref))
                p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                           line = list(color = 'black', dash = 'dash', width = 2),
                                           legendgroup = leg.grp,
                                           showlegend = TRUE)
            } 
          }
      # Add these lines to modify colors in box plot
      # marker = list(color = 'rgb(135,206,250'),
      # line = list(color = 'rgb(135,206,250'),
      
      if (input$rcm.outputValues == 'Bias')  
        ylab <- "Bias in simulated regional temperature [deg. C]"
      else if (input$rcm.outputValues == 'Anomaly')
        ylab <- "Simulated regional temperature anomalies [deg. C]"
      else if (input$rcm.outputValues == 'Change')
        ylab <- "Absolute change in simulted regional temperature with regards to present [deg. C]"
      else 
        ylab <- "Simulated regional temperature [deg. C]"
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$rcm.region),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      if (input$rcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      else
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      p.sc$elementId <- NULL
      p.sc
        })
    
    output$rcm.sc.tas <- rcm.sc.tas
    output$hydro.sc.tas <- rcm.sc.tas
    
    output$rcm.sc.bias.tas.pu <- renderPlotly({
      
      rcm.meta.tas <- rcm.meta.tas.reactive.pu()
      df <- rcm.sc.tas.reactive.pu()
      
      df <- df - df[,dim(df)[2]]      
      
      #df <- df[,-36] # AM Quick fix but has to be removed ... once meta is updated.
      # rcm seasonal cycle
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      
      # define layout output
      #df$ref <- df$ref
      if (input$rcm.chart.type.pu == 'Individual Simulations') {
        ## Make the plot
        #p.sc <- plot_ly(df, x = ~month, y = ~rcm.1,type = 'scatter',mode = 'markers+lines', line = list(width = 2, color = "grey",shape = 'spline'))
        # create plot_ly
        p.sc <- plot_ly(df, x = ~month)
        
        rcms <- colnames(df)[grep('rcm',colnames(df))]
        id <- 1 : (length(df) - 1)
        lev <- levels(factor(id))
        
        if (length(lev) > 50) {
          # 108 distinct colors
          rgbcols <- c('rgb(255,128,128)', 'rgb(178,89,89)', 'rgb(51,26,26)', 'rgb(217,123,108)', 'rgb(140,79,70)', 'rgb(89,51,45)', 'rgb(64,36,32)', 'rgb(242,153,121)', 'rgb(178,113,89)', 
                       'rgb(102,65,51)', 'rgb(76,48,38)', 'rgb(51,32,26)', 'rgb(217,152,108)', 'rgb(153,107,77)', 'rgb(102,71,51)', 'rgb(76,54,38)', 'rgb(229,176,115)', 'rgb(178,137,89)',
                       'rgb(140,108,70)', 'rgb(102,78,51)', 'rgb(51,39,26)', 'rgb(217,181,108)', 'rgb(140,117,70)', 'rgb(76,64,38)', 'rgb(178,161,89)', 'rgb(102,92,51)', 'rgb(51,46,26)', 
                       'rgb(229,222,115)', 'rgb(173,179,89)', 'rgb(74,77,38)', 'rgb(229,255,128)', 'rgb(126,140,70)', 'rgb(46,51,26)', 'rgb(170,204,102)', 'rgb(85,102,51)', 'rgb(186,242,121)', 
                       'rgb(127,166,83)', 'rgb(81,128,64)', 'rgb(116,204,102)', 'rgb(127,255,128)', 'rgb(83,166,94)', 'rgb(51,102,58)', 'rgb(38,77,43)', 'rgb(26,51,29)', 'rgb(115,230,161)', 
                       'rgb(77,153,107)', 'rgb(64,128,98)', 'rgb(45,89,68)', 'rgb(102,204,170)', 'rgb(26,51,43)', 'rgb(121,242,218)', 'rgb(83,166,149)', 'rgb(51,102,92)', 'rgb(38,77,69)',
                       'rgb(102,204,197)', 'rgb(121,234,242)', 'rgb(83,160,166)', 'rgb(57,111,115)', 'rgb(38,74,77)', 'rgb(26,49,51)', 'rgb(108,195,217)', 'rgb(77,138,153)', 'rgb(121,202,242)', 
                       'rgb(89,149,179)', 'rgb(70,117,140)', 'rgb(51,85,102)', 'rgb(32,53,64)', 'rgb(128,196,255)', 'rgb(102,156,204)', 'rgb(64,98,128)', 'rgb(128,179,255)', 'rgb(102,143,204)', 
                       'rgb(45,62,89)', 'rgb(26,36,51)', 'rgb(121,153,242)', 'rgb(77,97,153)', 'rgb(128,145,255)', 'rgb(89,101,179)', 'rgb(64,72,128)', 'rgb(45,51,89)', 'rgb(32,36,64)', 'rgb(108,108,217)', 			      'rgb(58,51,102)', 'rgb(162,128,255)', 'rgb(113,89,179)', 'rgb(89,70,140)', 'rgb(48,38,77)', 'rgb(39,26,51)', 'rgb(128,77,153)', 'rgb(74,45,89)', 'rgb(172,96,191)', 
                       'rgb(247,128,255)', 'rgb(111,57,115)', 'rgb(62,32,64)', 'rgb(242,121,218)', 'rgb(191,96,172)', 'rgb(89,45,80)', 'rgb(51,26,46)', 'rgb(128,64,106)', 'rgb(166,83,127)', 
                       'rgb(242,121,170)', 'rgb(102,51,71)', 'rgb(51,26,36)', 'rgb(191,96,121)', 'rgb(128,64,81)', 'rgb(77,38,48)', 'rgb(229,115,130)', 'rgb(153,77,87)', 'rgb(102,51,58)')
          
          # 108 distinct colors with transparency
          rgbcolsa <- c('rgba(255,128,128,0.5)', 'rgba(178,89,89,0.5)', 'rgba(51,26,26,0.5)', 'rgba(217,123,108,0.5)', 'rgba(140,79,70,0.5)', 'rgba(89,51,45,0.5)', 'rgba(64,36,32,0.5)', 
                        'rgba(242,153,121,0.5)', 'rgba(178,113,89,0.5)', 'rgba(102,65,51,0.5)', 'rgba(76,48,38,0.5)', 'rgba(51,32,26,0.5)', 'rgba(217,152,108,0.5)', 'rgba(153,107,77,0.5)', 'rgba(102,71,51,0.5)', 'rgba(76,54,38,0.5)', 'rgba(229,176,115,0.5)', 'rgba(178,137,89,0.5)', 'rgba(140,108,70,0.5)', 'rgba(102,78,51,0.5)', 'rgba(51,39,26,0.5)', 'rgba(217,181,108,0.5)', 'rgba(140,117,70,0.5)', 'rgba(76,64,38,0.5)', 'rgba(178,161,89,0.5)', 'rgba(102,92,51,0.5)', 'rgba(51,46,26,0.5)', 
                        'rgba(229,222,115,0.5)', 'rgba(173,179,89,0.5)', 'rgba(74,77,38,0.5)', 'rgba(229,255,128,0.5)', 'rgba(126,140,70,0.5)', 'rgba(46,51,26,0.5)', 'rgba(170,204,102,0.5)', 'rgba(85,102,51,0.5)', 'rgba(186,242,121,0.5)', 'rgba(127,166,83,0.5)', 'rgba(81,128,64,0.5)', 'rgba(116,204,102,0.5)', 'rgba(127,255,128,0.5)', 'rgba(83,166,94,0.5)', 'rgba(51,102,58,0.5)', 'rgba(38,77,43,0.5)', 'rgba(26,51,29,0.5)', 'rgba(115,230,161,0.5)', 'rgba(77,153,107,0.5)', 'rgba(64,128,98,0.5)', 'rgba(45,89,68,0.5)', 'rgba(102,204,170,0.5)', 'rgba(26,51,43,0.5)', 'rgba(121,242,218,0.5)', 'rgba(83,166,149,0.5)', 'rgba(51,102,92,0.5)', 'rgba(38,77,69,0.5)', 'rgba(102,204,197,0.5)', 'rgba(121,234,242,0.5)', 'rgba(83,160,166,0.5)', 'rgba(57,111,115,0.5)', 'rgba(38,74,77,0.5)', 'rgba(26,49,51,0.5)', 'rgba(108,195,217,0.5)', 'rgba(77,138,153,0.5)', 'rgba(121,202,242,0.5)', 'rgba(89,149,179,0.5)', 'rgba(70,117,140),0.5', 'rgba(51,85,102,0.5)', 'rgba(32,53,64,0.5)', 'rgba(128,196,255,0.5)', 'rgba(102,156,204,0.5)', 'rgba(64,98,128,0.5)', 'rgba(128,179,255,0.5)', 'rgba(102,143,204,0.5)', 'rgba(45,62,89,0.5)', 'rgba(26,36,51,0.5)', 'rgba(121,153,242,0.5)', 'rgba(77,97,153,0.5)', 'rgba(128,145,255,0.5)', 'rgba(89,101,179,0.5)', 'rgba(64,72,128,0.5)', 'rgba(45,51,89,0.5)', 'rgba(32,36,64,0.5)', 'rgba(108,108,217,0.5)', 'rgba(58,51,102,0.5)', 'rgba(162,128,255,0.5)', 'rgba(113,89,179,0.5)', 'rgba(89,70,140)', 'rgba(48,38,77,0.5)', 'rgba(39,26,51,0.5)', 'rgba(128,77,153,0.5)', 'rgba(74,45,89)', 'rgba(172,96,191,0.5)', 
                        'rgba(247,128,255,0.5)', 'rgba(111,57,115,0.5)', 'rgba(62,32,64,0.5)', 'rgba(242,121,218,0.5)', 'rgba(191,96,172,0.5)', 'rgba(89,45,80,0.5)', 'rgba(51,26,46,0.5)', 'rgba(128,64,106,0.5)', 'rgba(166,83,127,0.5)', 
                        'rgba(242,121,170,0.5)', 'rgba(102,51,71,0.5)', 'rgba(51,26,36,0.5)', 'rgba(191,96,121,0.5)', 'rgba(128,64,81,0.5)', 'rgba(77,38,48,0.5)', 'rgba(229,115,130,0.5)', 'rgba(153,77,87,0.5)', 'rgba(102,51,58,0.5)')
        } else {
          rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                        'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                        'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                        'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                        'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                        'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                        'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                        'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
          
          rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                       'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                       'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                       'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                       'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                       'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                       'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        }
        ## Same as precip ...
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        ## color by groups
        colsa <- rgbcolsa
        cols <- rgbcols
        
        # Add all models
        for (rcm in rcms) {
          i <- which(is.element(rcms,rcm))
          leg.name <- paste(paste(as.character(as.matrix(rcm.meta.tas[i,c('gcm','gcm_rip','rcm')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='')
          
          
          #if (is.null(input$rowsRcm)) {
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter',
                                  name = paste(",i,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          #}
          ## Highlight selected models in tab:models
        } 
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.chart.type.pu))) { # Make an enveloppe instead of lines
        
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
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        
        
      } else if (grepl('box',tolower(input$rcm.chart.type.pu))) {
        p.sc <- plot_ly(df, type = 'box')
        
        month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
        col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                     'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                     'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                     'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                     'rgb(166,206,227)')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          leg.grp <- month.grp[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color=col.grp[",i,"],opacity=0.6),
                                  name = leg.name,showlegend =TRUE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                       line = list(color = 'black', dash = 'dash', width = 2),
                                       legendgroup = leg.grp,
                                       showlegend = TRUE)
        } 
      }
      # Add these lines to modify colors in box plot
      # marker = list(color = 'rgb(135,206,250'),
      # line = list(color = 'rgb(135,206,250'),
      
      
      ylab <- "Bias in simulated regional temperature [deg. C]"
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$rcm.region.pu),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      if (input$rcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      else
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      p.sc$elementId <- NULL
      p.sc
    })
    
    output$rcm.sc.tas.pu <- renderPlotly({
      
      rcm.meta.tas <- rcm.meta.tas.reactive.sc.pu()
      df <- rcm.sc.tas.reactive.sc.pu()
      
      #df <- df[,-36] # AM Quick fix but has to be removed ... once meta is updated.
      # rcm seasonal cycle
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      
      # define layout output
      #df$ref <- df$ref
      if (input$rcm.sc.chart.type.pu == 'Individual Simulations') {
        ## Make the plot
        #p.sc <- plot_ly(df, x = ~month, y = ~rcm.1,type = 'scatter',mode = 'markers+lines', line = list(width = 2, color = "grey",shape = 'spline'))
        # create plot_ly
        p.sc <- plot_ly(df, x = ~month)
        
        rcms <- colnames(df)[grep('rcm',colnames(df))]
        id <- 1 : (length(df) - 1)
        lev <- levels(factor(id))
        
        if (length(lev) > 50) {
          # 108 distinct colors
          rgbcols <- c('rgb(255,128,128)', 'rgb(178,89,89)', 'rgb(51,26,26)', 'rgb(217,123,108)', 'rgb(140,79,70)', 'rgb(89,51,45)', 'rgb(64,36,32)', 'rgb(242,153,121)', 'rgb(178,113,89)', 
                       'rgb(102,65,51)', 'rgb(76,48,38)', 'rgb(51,32,26)', 'rgb(217,152,108)', 'rgb(153,107,77)', 'rgb(102,71,51)', 'rgb(76,54,38)', 'rgb(229,176,115)', 'rgb(178,137,89)',
                       'rgb(140,108,70)', 'rgb(102,78,51)', 'rgb(51,39,26)', 'rgb(217,181,108)', 'rgb(140,117,70)', 'rgb(76,64,38)', 'rgb(178,161,89)', 'rgb(102,92,51)', 'rgb(51,46,26)', 
                       'rgb(229,222,115)', 'rgb(173,179,89)', 'rgb(74,77,38)', 'rgb(229,255,128)', 'rgb(126,140,70)', 'rgb(46,51,26)', 'rgb(170,204,102)', 'rgb(85,102,51)', 'rgb(186,242,121)', 
                       'rgb(127,166,83)', 'rgb(81,128,64)', 'rgb(116,204,102)', 'rgb(127,255,128)', 'rgb(83,166,94)', 'rgb(51,102,58)', 'rgb(38,77,43)', 'rgb(26,51,29)', 'rgb(115,230,161)', 
                       'rgb(77,153,107)', 'rgb(64,128,98)', 'rgb(45,89,68)', 'rgb(102,204,170)', 'rgb(26,51,43)', 'rgb(121,242,218)', 'rgb(83,166,149)', 'rgb(51,102,92)', 'rgb(38,77,69)',
                       'rgb(102,204,197)', 'rgb(121,234,242)', 'rgb(83,160,166)', 'rgb(57,111,115)', 'rgb(38,74,77)', 'rgb(26,49,51)', 'rgb(108,195,217)', 'rgb(77,138,153)', 'rgb(121,202,242)', 
                       'rgb(89,149,179)', 'rgb(70,117,140)', 'rgb(51,85,102)', 'rgb(32,53,64)', 'rgb(128,196,255)', 'rgb(102,156,204)', 'rgb(64,98,128)', 'rgb(128,179,255)', 'rgb(102,143,204)', 
                       'rgb(45,62,89)', 'rgb(26,36,51)', 'rgb(121,153,242)', 'rgb(77,97,153)', 'rgb(128,145,255)', 'rgb(89,101,179)', 'rgb(64,72,128)', 'rgb(45,51,89)', 'rgb(32,36,64)', 'rgb(108,108,217)', 			      'rgb(58,51,102)', 'rgb(162,128,255)', 'rgb(113,89,179)', 'rgb(89,70,140)', 'rgb(48,38,77)', 'rgb(39,26,51)', 'rgb(128,77,153)', 'rgb(74,45,89)', 'rgb(172,96,191)', 
                       'rgb(247,128,255)', 'rgb(111,57,115)', 'rgb(62,32,64)', 'rgb(242,121,218)', 'rgb(191,96,172)', 'rgb(89,45,80)', 'rgb(51,26,46)', 'rgb(128,64,106)', 'rgb(166,83,127)', 
                       'rgb(242,121,170)', 'rgb(102,51,71)', 'rgb(51,26,36)', 'rgb(191,96,121)', 'rgb(128,64,81)', 'rgb(77,38,48)', 'rgb(229,115,130)', 'rgb(153,77,87)', 'rgb(102,51,58)')
          
          # 108 distinct colors with transparency
          rgbcolsa <- c('rgba(255,128,128,0.5)', 'rgba(178,89,89,0.5)', 'rgba(51,26,26,0.5)', 'rgba(217,123,108,0.5)', 'rgba(140,79,70,0.5)', 'rgba(89,51,45,0.5)', 'rgba(64,36,32,0.5)', 
                        'rgba(242,153,121,0.5)', 'rgba(178,113,89,0.5)', 'rgba(102,65,51,0.5)', 'rgba(76,48,38,0.5)', 'rgba(51,32,26,0.5)', 'rgba(217,152,108,0.5)', 'rgba(153,107,77,0.5)', 'rgba(102,71,51,0.5)', 'rgba(76,54,38,0.5)', 'rgba(229,176,115,0.5)', 'rgba(178,137,89,0.5)', 'rgba(140,108,70,0.5)', 'rgba(102,78,51,0.5)', 'rgba(51,39,26,0.5)', 'rgba(217,181,108,0.5)', 'rgba(140,117,70,0.5)', 'rgba(76,64,38,0.5)', 'rgba(178,161,89,0.5)', 'rgba(102,92,51,0.5)', 'rgba(51,46,26,0.5)', 
                        'rgba(229,222,115,0.5)', 'rgba(173,179,89,0.5)', 'rgba(74,77,38,0.5)', 'rgba(229,255,128,0.5)', 'rgba(126,140,70,0.5)', 'rgba(46,51,26,0.5)', 'rgba(170,204,102,0.5)', 'rgba(85,102,51,0.5)', 'rgba(186,242,121,0.5)', 'rgba(127,166,83,0.5)', 'rgba(81,128,64,0.5)', 'rgba(116,204,102,0.5)', 'rgba(127,255,128,0.5)', 'rgba(83,166,94,0.5)', 'rgba(51,102,58,0.5)', 'rgba(38,77,43,0.5)', 'rgba(26,51,29,0.5)', 'rgba(115,230,161,0.5)', 'rgba(77,153,107,0.5)', 'rgba(64,128,98,0.5)', 'rgba(45,89,68,0.5)', 'rgba(102,204,170,0.5)', 'rgba(26,51,43,0.5)', 'rgba(121,242,218,0.5)', 'rgba(83,166,149,0.5)', 'rgba(51,102,92,0.5)', 'rgba(38,77,69,0.5)', 'rgba(102,204,197,0.5)', 'rgba(121,234,242,0.5)', 'rgba(83,160,166,0.5)', 'rgba(57,111,115,0.5)', 'rgba(38,74,77,0.5)', 'rgba(26,49,51,0.5)', 'rgba(108,195,217,0.5)', 'rgba(77,138,153,0.5)', 'rgba(121,202,242,0.5)', 'rgba(89,149,179,0.5)', 'rgba(70,117,140),0.5', 'rgba(51,85,102,0.5)', 'rgba(32,53,64,0.5)', 'rgba(128,196,255,0.5)', 'rgba(102,156,204,0.5)', 'rgba(64,98,128,0.5)', 'rgba(128,179,255,0.5)', 'rgba(102,143,204,0.5)', 'rgba(45,62,89,0.5)', 'rgba(26,36,51,0.5)', 'rgba(121,153,242,0.5)', 'rgba(77,97,153,0.5)', 'rgba(128,145,255,0.5)', 'rgba(89,101,179,0.5)', 'rgba(64,72,128,0.5)', 'rgba(45,51,89,0.5)', 'rgba(32,36,64,0.5)', 'rgba(108,108,217,0.5)', 'rgba(58,51,102,0.5)', 'rgba(162,128,255,0.5)', 'rgba(113,89,179,0.5)', 'rgba(89,70,140)', 'rgba(48,38,77,0.5)', 'rgba(39,26,51,0.5)', 'rgba(128,77,153,0.5)', 'rgba(74,45,89)', 'rgba(172,96,191,0.5)', 
                        'rgba(247,128,255,0.5)', 'rgba(111,57,115,0.5)', 'rgba(62,32,64,0.5)', 'rgba(242,121,218,0.5)', 'rgba(191,96,172,0.5)', 'rgba(89,45,80,0.5)', 'rgba(51,26,46,0.5)', 'rgba(128,64,106,0.5)', 'rgba(166,83,127,0.5)', 
                        'rgba(242,121,170,0.5)', 'rgba(102,51,71,0.5)', 'rgba(51,26,36,0.5)', 'rgba(191,96,121,0.5)', 'rgba(128,64,81,0.5)', 'rgba(77,38,48,0.5)', 'rgba(229,115,130,0.5)', 'rgba(153,77,87,0.5)', 'rgba(102,51,58,0.5)')
        } else {
          rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                        'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                        'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                        'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                        'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                        'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                        'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                        'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
          
          rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                       'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                       'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                       'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                       'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                       'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                       'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        }
        ## Same as precip ...
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        ## color by groups
        colsa <- rgbcolsa
        cols <- rgbcols
        
        # Add all models
        for (rcm in rcms) {
          i <- which(is.element(rcms,rcm))
          leg.name <- paste(paste(as.character(as.matrix(rcm.meta.tas[i,c('gcm','gcm_rip','rcm')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='')
          
          
          #if (is.null(input$rowsRcm)) {
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter',
                                  name = paste(",i,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          #}
          ## Highlight selected models in tab:models
        } 
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.sc.chart.type.pu))) { # Make an enveloppe instead of lines
        
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
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        
        
      } else if (grepl('box',tolower(input$rcm.sc.chart.type.pu))) {
        p.sc <- plot_ly(df, type = 'box')
        
        month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
        col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                     'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                     'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                     'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                     'rgb(166,206,227)')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          leg.grp <- month.grp[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color=col.grp[",i,"],opacity=0.6),
                                  name = leg.name,showlegend =TRUE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                       line = list(color = 'black', dash = 'dash', width = 2),
                                       legendgroup = leg.grp,
                                       showlegend = TRUE)
        } 
      }
      # Add these lines to modify colors in box plot
      # marker = list(color = 'rgb(135,206,250'),
      # line = list(color = 'rgb(135,206,250'),
      
      
      ylab <- "Seasonal Cycle of area averaged simulated temperature [deg. C]"
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$rcm.sc.region.pu),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      if (input$rcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      else
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      p.sc$elementId <- NULL
      p.sc
    })
    
    output$rcm.cc.tas.pu <- renderPlotly({
      
      df <- rcm.sc.tas.reactive.cc.pu() - rcm.sc.tas.present.cc.pu()
      
      #df <- df[,-36] # AM Quick fix but has to be removed ... once meta is updated.
      # rcm seasonal cycle
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      
      # define layout output
      #df$ref <- df$ref
      if (input$rcm.cc.chart.type == 'Individual Simulations') {
        ## Make the plot
        #p.sc <- plot_ly(df, x = ~month, y = ~rcm.1,type = 'scatter',mode = 'markers+lines', line = list(width = 2, color = "grey",shape = 'spline'))
        # create plot_ly
        p.sc <- plot_ly(df, x = ~month)
        
        rcms <- colnames(df)[grep('rcm',colnames(df))]
        id <- 1 : (length(df) - 1)
        lev <- levels(factor(id))
        
        if (length(lev) > 50) {
          # 108 distinct colors
          rgbcols <- c('rgb(255,128,128)', 'rgb(178,89,89)', 'rgb(51,26,26)', 'rgb(217,123,108)', 'rgb(140,79,70)', 'rgb(89,51,45)', 'rgb(64,36,32)', 'rgb(242,153,121)', 'rgb(178,113,89)', 
                       'rgb(102,65,51)', 'rgb(76,48,38)', 'rgb(51,32,26)', 'rgb(217,152,108)', 'rgb(153,107,77)', 'rgb(102,71,51)', 'rgb(76,54,38)', 'rgb(229,176,115)', 'rgb(178,137,89)',
                       'rgb(140,108,70)', 'rgb(102,78,51)', 'rgb(51,39,26)', 'rgb(217,181,108)', 'rgb(140,117,70)', 'rgb(76,64,38)', 'rgb(178,161,89)', 'rgb(102,92,51)', 'rgb(51,46,26)', 
                       'rgb(229,222,115)', 'rgb(173,179,89)', 'rgb(74,77,38)', 'rgb(229,255,128)', 'rgb(126,140,70)', 'rgb(46,51,26)', 'rgb(170,204,102)', 'rgb(85,102,51)', 'rgb(186,242,121)', 
                       'rgb(127,166,83)', 'rgb(81,128,64)', 'rgb(116,204,102)', 'rgb(127,255,128)', 'rgb(83,166,94)', 'rgb(51,102,58)', 'rgb(38,77,43)', 'rgb(26,51,29)', 'rgb(115,230,161)', 
                       'rgb(77,153,107)', 'rgb(64,128,98)', 'rgb(45,89,68)', 'rgb(102,204,170)', 'rgb(26,51,43)', 'rgb(121,242,218)', 'rgb(83,166,149)', 'rgb(51,102,92)', 'rgb(38,77,69)',
                       'rgb(102,204,197)', 'rgb(121,234,242)', 'rgb(83,160,166)', 'rgb(57,111,115)', 'rgb(38,74,77)', 'rgb(26,49,51)', 'rgb(108,195,217)', 'rgb(77,138,153)', 'rgb(121,202,242)', 
                       'rgb(89,149,179)', 'rgb(70,117,140)', 'rgb(51,85,102)', 'rgb(32,53,64)', 'rgb(128,196,255)', 'rgb(102,156,204)', 'rgb(64,98,128)', 'rgb(128,179,255)', 'rgb(102,143,204)', 
                       'rgb(45,62,89)', 'rgb(26,36,51)', 'rgb(121,153,242)', 'rgb(77,97,153)', 'rgb(128,145,255)', 'rgb(89,101,179)', 'rgb(64,72,128)', 'rgb(45,51,89)', 'rgb(32,36,64)', 'rgb(108,108,217)', 			      'rgb(58,51,102)', 'rgb(162,128,255)', 'rgb(113,89,179)', 'rgb(89,70,140)', 'rgb(48,38,77)', 'rgb(39,26,51)', 'rgb(128,77,153)', 'rgb(74,45,89)', 'rgb(172,96,191)', 
                       'rgb(247,128,255)', 'rgb(111,57,115)', 'rgb(62,32,64)', 'rgb(242,121,218)', 'rgb(191,96,172)', 'rgb(89,45,80)', 'rgb(51,26,46)', 'rgb(128,64,106)', 'rgb(166,83,127)', 
                       'rgb(242,121,170)', 'rgb(102,51,71)', 'rgb(51,26,36)', 'rgb(191,96,121)', 'rgb(128,64,81)', 'rgb(77,38,48)', 'rgb(229,115,130)', 'rgb(153,77,87)', 'rgb(102,51,58)')
          
          # 108 distinct colors with transparency
          rgbcolsa <- c('rgba(255,128,128,0.5)', 'rgba(178,89,89,0.5)', 'rgba(51,26,26,0.5)', 'rgba(217,123,108,0.5)', 'rgba(140,79,70,0.5)', 'rgba(89,51,45,0.5)', 'rgba(64,36,32,0.5)', 
                        'rgba(242,153,121,0.5)', 'rgba(178,113,89,0.5)', 'rgba(102,65,51,0.5)', 'rgba(76,48,38,0.5)', 'rgba(51,32,26,0.5)', 'rgba(217,152,108,0.5)', 'rgba(153,107,77,0.5)', 'rgba(102,71,51,0.5)', 'rgba(76,54,38,0.5)', 'rgba(229,176,115,0.5)', 'rgba(178,137,89,0.5)', 'rgba(140,108,70,0.5)', 'rgba(102,78,51,0.5)', 'rgba(51,39,26,0.5)', 'rgba(217,181,108,0.5)', 'rgba(140,117,70,0.5)', 'rgba(76,64,38,0.5)', 'rgba(178,161,89,0.5)', 'rgba(102,92,51,0.5)', 'rgba(51,46,26,0.5)', 
                        'rgba(229,222,115,0.5)', 'rgba(173,179,89,0.5)', 'rgba(74,77,38,0.5)', 'rgba(229,255,128,0.5)', 'rgba(126,140,70,0.5)', 'rgba(46,51,26,0.5)', 'rgba(170,204,102,0.5)', 'rgba(85,102,51,0.5)', 'rgba(186,242,121,0.5)', 'rgba(127,166,83,0.5)', 'rgba(81,128,64,0.5)', 'rgba(116,204,102,0.5)', 'rgba(127,255,128,0.5)', 'rgba(83,166,94,0.5)', 'rgba(51,102,58,0.5)', 'rgba(38,77,43,0.5)', 'rgba(26,51,29,0.5)', 'rgba(115,230,161,0.5)', 'rgba(77,153,107,0.5)', 'rgba(64,128,98,0.5)', 'rgba(45,89,68,0.5)', 'rgba(102,204,170,0.5)', 'rgba(26,51,43,0.5)', 'rgba(121,242,218,0.5)', 'rgba(83,166,149,0.5)', 'rgba(51,102,92,0.5)', 'rgba(38,77,69,0.5)', 'rgba(102,204,197,0.5)', 'rgba(121,234,242,0.5)', 'rgba(83,160,166,0.5)', 'rgba(57,111,115,0.5)', 'rgba(38,74,77,0.5)', 'rgba(26,49,51,0.5)', 'rgba(108,195,217,0.5)', 'rgba(77,138,153,0.5)', 'rgba(121,202,242,0.5)', 'rgba(89,149,179,0.5)', 'rgba(70,117,140),0.5', 'rgba(51,85,102,0.5)', 'rgba(32,53,64,0.5)', 'rgba(128,196,255,0.5)', 'rgba(102,156,204,0.5)', 'rgba(64,98,128,0.5)', 'rgba(128,179,255,0.5)', 'rgba(102,143,204,0.5)', 'rgba(45,62,89,0.5)', 'rgba(26,36,51,0.5)', 'rgba(121,153,242,0.5)', 'rgba(77,97,153,0.5)', 'rgba(128,145,255,0.5)', 'rgba(89,101,179,0.5)', 'rgba(64,72,128,0.5)', 'rgba(45,51,89,0.5)', 'rgba(32,36,64,0.5)', 'rgba(108,108,217,0.5)', 'rgba(58,51,102,0.5)', 'rgba(162,128,255,0.5)', 'rgba(113,89,179,0.5)', 'rgba(89,70,140)', 'rgba(48,38,77,0.5)', 'rgba(39,26,51,0.5)', 'rgba(128,77,153,0.5)', 'rgba(74,45,89)', 'rgba(172,96,191,0.5)', 
                        'rgba(247,128,255,0.5)', 'rgba(111,57,115,0.5)', 'rgba(62,32,64,0.5)', 'rgba(242,121,218,0.5)', 'rgba(191,96,172,0.5)', 'rgba(89,45,80,0.5)', 'rgba(51,26,46,0.5)', 'rgba(128,64,106,0.5)', 'rgba(166,83,127,0.5)', 
                        'rgba(242,121,170,0.5)', 'rgba(102,51,71,0.5)', 'rgba(51,26,36,0.5)', 'rgba(191,96,121,0.5)', 'rgba(128,64,81,0.5)', 'rgba(77,38,48,0.5)', 'rgba(229,115,130,0.5)', 'rgba(153,77,87,0.5)', 'rgba(102,51,58,0.5)')
        } else {
          rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                        'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                        'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                        'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                        'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                        'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                        'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                        'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
          
          rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                       'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                       'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                       'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                       'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                       'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                       'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        }
        ## Same as precip ...
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        ## color by groups
        colsa <- rgbcolsa
        cols <- rgbcols
        
        # Add all models
        for (rcm in rcms) {
          i <- which(is.element(rcms,rcm))
          leg.name <- paste(paste(as.character(as.matrix(rcm.meta.tas[i,c('gcm','gcm_rip','rcm')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='')
          
          
          #if (is.null(input$rowsRcm)) {
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter',
                                  name = paste(",i,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          #}
          ## Highlight selected models in tab:models
        } 
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.cc.chart.type))) { # Make an enveloppe instead of lines
        
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
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        
        
      } else if (grepl('box',tolower(input$rcm.cc.chart.type))) {
        p.sc <- plot_ly(df, type = 'box')
        
        month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
        col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                     'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                     'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                     'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                     'rgb(166,206,227)')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          leg.grp <- month.grp[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color=col.grp[",i,"],opacity=0.6),
                                  name = leg.name,showlegend =TRUE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                       line = list(color = 'black', dash = 'dash', width = 2),
                                       legendgroup = leg.grp,
                                       showlegend = TRUE)
        } 
      }
      # Add these lines to modify colors in box plot
      # marker = list(color = 'rgb(135,206,250'),
      # line = list(color = 'rgb(135,206,250'),
      
      
      ylab <- "Seasonal Cycle of area averaged simulated temperature [deg. C]"
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$rcm.cc.region),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      if (input$rcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      else
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      p.sc$elementId <- NULL
      p.sc
    })
    
    rcm.sc.pr <- renderPlotly({
      rcm.meta.pr <- rcm.meta.pr.reactive()
      df <- rcm.sc.pr.reactive()
      
      if (input$rcm.sim.sc == 'Selected Simulations') {
        rcm.meta.pr <- rcm.meta.pr[input$rowsRcm,]
      }
      
      if (input$rcm.outputValues == 'Bias')
        df <- ((df - df[,dim(df)[2]])/df[,dim(df)[2]]) * 100
      else if (input$rcm.outputValues == 'Anomaly') {
        DF <- t(df)
        df <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$rcm.outputValues == 'Change') {
        df <- ((rcm.sc.pr.reactive() - rcm.sc.pr.present())/ rcm.sc.pr.present()) * 100
      }
      
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      if (input$rcm.chart.type == 'Individual Simulations') {
        ## Make the plot
        p.sc <- plot_ly(df,x = ~month)
        
        rcms <- colnames(df)[grep('rcm',colnames(df))]
        if (is.element(input$rcm.groupBy,c('None','---'))) {
          id <- 1 : (length(df) - 1)
          lev <- levels(factor(id))
        }
        else { 
          id <- as.integer(factor(base::subset(rcm.meta.pr, select = input$rcm.groupBy)[[1]]))
          lev <- levels(factor(base::subset(rcm.meta.pr, select = input$rcm.groupBy)[[1]]))
        }
        
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        
        # Color by
        if (input$rcm.colorBy == 'Group') {
          colsa <- rgbcolsa[id]
          cols <- rgbcols[id]
        } else {
          colsa <- rgbcolsa
          cols <- rgbcols
        }
        
        ## Add all Simulations
        if (is.null(input$rowsRcm)) {
          for (rcm in rcms) {
            i <- which(is.element(rcms,rcm))
            #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
            leg.name <- paste(paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','rcm')])),collapse = ' '),
                              paste(substr(lev[id[i]],1,5),'...',sep=''),
                              sep = ' ')
            grp.name <- paste('Group',id[i],sep='') #leg.name
            
            if (is.element(input$rcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = paste(",i,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', legendgroup = grp.name,
                                      colors = ",i,",hoverinfo = 'text+x+y',text=leg.name, legendgroup = grp.name,
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = paste(",i,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', legendgroup = grp.name,
                                      colors = colsa[",i,"],hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
            
          }
          ## Highlight selected Simulations in tab:models
        } else {
          im <- input$rowsRcm
          for (i in 1:length(im)) {
            leg.name <- paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','rcm')])),collapse = ' ')
            grp.name <- paste('Group',id[i],sep='')
            rcm <- rcms[i]
            if (is.element(input$rcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = paste(",im,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', legendgroup = grp.name,
                                      colors = ",i,",hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = paste(",im,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', legendgroup = grp.name,
                                      colors = colsa[im[",i,"]],hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = colsa[im[",i,"]], width = 2, shape ='spline'))",sep='')))
          }            
          
          }
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        } else if (grepl('ensemble', tolower(input$rcm.chart.type))) { # Make an enveloppe instead of lines
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
            p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                       line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
          
        } else if (grepl('box',tolower(input$rcm.chart.type))) {
          p.sc <- plot_ly(df, type = 'box')
          
          month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
          col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                       'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                       'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                       'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                       'rgb(166,206,227)')
          for (i in 1:12) {
            leg.name <- month.abb[i]
            leg.grp <- month.grp[i]
            eval(parse(text = paste("p.sc <- p.sc %>% 
                                    add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                    type = 'box', boxpoints = 'all',
                                    legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                    line = list(color=col.grp[",i,"],opacity=0.6),
                                    name = leg.name,showlegend =TRUE)",sep='')))
            if (!is.null(df$ref))
              p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                         line = list(color = 'black', dash = 'dash', width = 2),
                                         legendgroup = leg.grp,
                                         showlegend = TRUE) 
          } 
        }
      
      if (input$rcm.outputValues == 'Bias')  
        ylab <- "Bias in simulated regional precipitation [%]"
      else if (input$rcm.outputValues == 'Anomaly') 
        ylab <- "Simulated regional precipitation anomalies [mm]"
      else if (input$rcm.outputValues == 'Change')
        ylab <- "Relative change in simulted regional precipitation with regards to present [%]"
      else 
        ylab <- "Simulated regional precipitation [mm]"
      # Format layout 
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$rcm.region),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      
      if (input$rcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      
      p.sc$elementId <- NULL
      p.sc
        })
    
    output$rcm.sc.bias.pr.pu <- renderPlotly({
      rcm.meta.pr <- rcm.meta.pr.reactive.pu()
      df <- rcm.sc.pr.reactive.pu()
      
      df <- ((df - df[,dim(df)[2]])/df[,dim(df)[2]]) * 100
      
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      if (input$rcm.chart.type == 'Individual Simulations') {
        ## Make the plot
        p.sc <- plot_ly(df,x = ~month)
        
        rcms <- colnames(df)[grep('rcm',colnames(df))]
        id <- 1 : (length(df) - 1)
        lev <- levels(factor(id))
        
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        
        # Color by
        colsa <- rgbcolsa
        cols <- rgbcols
        
        ## Add all Simulations
        for (rcm in rcms) {
          i <- which(is.element(rcms,rcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','rcm')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='') #leg.name
          
          if (is.element(input$rcm.colorBy, c('None','---')))
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                    name = paste(",i,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', legendgroup = grp.name,
                                    colors = ",i,",hoverinfo = 'text+x+y',text=leg.name, legendgroup = grp.name,
                                    line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
          else
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                    name = paste(",i,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', legendgroup = grp.name,
                                    colors = colsa[",i,"],hoverinfo = 'text+x+y',text=leg.name,
                                    line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          
        }
        ## Highlight selected Simulations in tab:models
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.chart.type.pu))) { # Make an enveloppe instead of lines
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
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('box',tolower(input$rcm.chart.type.pu))) {
        p.sc <- plot_ly(df, type = 'box')
        
        month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
        col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                     'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                     'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                     'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                     'rgb(166,206,227)')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          leg.grp <- month.grp[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color=col.grp[",i,"],opacity=0.6),
                                  name = leg.name,showlegend =TRUE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                       line = list(color = 'black', dash = 'dash', width = 2),
                                       legendgroup = leg.grp,
                                       showlegend = TRUE) 
        } 
      }
      
      
      ylab <- "Bias in simulated regional precipitation [%]"
      # Format layout 
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$rcm.region.pu),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      
      if (input$rcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      
      p.sc$elementId <- NULL
      p.sc
      })
    
    output$rcm.sc.pr.pu <- renderPlotly({
      rcm.meta.pr <- rcm.meta.pr.reactive.sc.pu()
      df <- rcm.sc.pr.reactive.sc.pu()
      
      #df <- ((df - df[,dim(df)[2]])/df[,dim(df)[2]]) * 100
      
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      
      if (!is.null(df$ref))
        df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      else
        df.env <- data.frame(low,avg,high,month = factor(month.abb, levels =  month.abb))
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      if (input$rcm.sc.chart.type.pu == 'Individual Simulations') {
        ## Make the plot
        p.sc <- plot_ly(df,x = ~month)
        
        rcms <- colnames(df)[grep('rcm',colnames(df))]
        id <- 1 : (length(df) - 1)
        lev <- levels(factor(id))
        
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        
        # Color by
        colsa <- rgbcolsa
        cols <- rgbcols
        
        ## Add all Simulations
        for (rcm in rcms) {
          i <- which(is.element(rcms,rcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','rcm')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='') #leg.name
          
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                  name = paste(",i,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', legendgroup = grp.name,
                                  colors = colsa[",i,"],hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          
        }
        ## Highlight selected Simulations in tab:models
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.sc.chart.type.pu))) { # Make an enveloppe instead of lines
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
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('box',tolower(input$rcm.sc.chart.type.pu))) {
        p.sc <- plot_ly(df, type = 'box')
        
        month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
        col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                     'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                     'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                     'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                     'rgb(166,206,227)')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          leg.grp <- month.grp[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color=col.grp[",i,"],opacity=0.6),
                                  name = leg.name,showlegend =TRUE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                       line = list(color = 'black', dash = 'dash', width = 2),
                                       legendgroup = leg.grp,
                                       showlegend = TRUE) 
        } 
      }
      
      ylab <- "Seasonal Cycle of area averaged simulated monthly precipitation sums [mm/month]"
      # Format layout 
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$rcm.sc.region.pu),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      
      if (input$rcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      
      p.sc$elementId <- NULL
      p.sc
      })
    
    output$rcm.cc.pr.pu <- renderPlotly({
      rcm.meta.pr <- rcm.meta.pr.reactive.sc.pu()
      
      df <- (rcm.sc.pr.reactive.cc.pu() - rcm.sc.pr.present.cc.pu()) / rcm.sc.pr.present.cc.pu() * 100
      
      df.env <- NULL
      low <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,min,na.rm=TRUE),digits = 2)
      high <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,max,na.rm=TRUE),digits = 2)
      avg <- round(apply(subset(df,select = grep('rcm',colnames(df))),1,mean,na.rm=TRUE),digits = 2)
      df.env <- data.frame(low,avg,high,ref = round(df$ref,digits = 2),month = factor(month.abb, levels =  month.abb))
      
      #The default order will be alphabetized unless specified as below:
      df$month <- factor(month.abb, levels = month.abb)
      
      if (input$rcm.cc.chart.type == 'Individual Simulations') {
        ## Make the plot
        p.sc <- plot_ly(df,x = ~month)
        
        rcms <- colnames(df)[grep('rcm',colnames(df))]
        id <- 1 : (length(df) - 1)
        lev <- levels(factor(id))
        
        rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                      'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                      'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                      'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                      'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                      'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                      'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                      'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
        
        rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                     'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                     'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                     'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                     'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                     'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                     'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
        
        
        # Color by
        colsa <- rgbcolsa
        cols <- rgbcols
        
        ## Add all Simulations
        for (rcm in rcms) {
          i <- which(is.element(rcms,rcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','rcm')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='') #leg.name
          
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                  name = paste(",i,",substr(leg.name,1,5),'...',sep=' '), mode = 'lines', legendgroup = grp.name,
                                  colors = colsa[",i,"],hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          
        }
        ## Highlight selected Simulations in tab:models
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.cc.chart.type))) { # Make an enveloppe instead of lines
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
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('box',tolower(input$rcm.cc.chart.type))) {
        p.sc <- plot_ly(df, type = 'box')
        
        month.grp <- c(1,1,2,2,2,3,3,3,4,4,4,1)
        col.grp <- c('rgb(166,206,227)','rgb(166,206,227)',
                     'rgb(253,191,111)','rgb(253,191,111)', 'rgb(253,191,111)',
                     'rgb(251,154,153)','rgb(251,154,153)','rgb(251,154,153)', 
                     'rgb(202,178,214)','rgb(202,178,214)','rgb(202,178,214)',
                     'rgb(166,206,227)')
        for (i in 1:12) {
          leg.name <- month.abb[i]
          leg.grp <- month.grp[i]
          eval(parse(text = paste("p.sc <- p.sc %>% 
                                  add_trace(y = ~as.numeric(as.vector(df[",i,",1:(dim(df)[2]-2)])),
                                  type = 'box', boxpoints = 'all',
                                  legendgroup = leg.grp, hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color=col.grp[",i,"],opacity=0.6),
                                  name = leg.name,showlegend =TRUE)",sep='')))
          if (!is.null(df$ref))
            p.sc <- p.sc %>% add_trace(y = df$ref[i], type = 'box', name = leg.name,
                                       line = list(color = 'black', dash = 'dash', width = 2),
                                       legendgroup = leg.grp,
                                       showlegend = TRUE) 
        } 
      }
      
      ylab <- "Seasonal Cycle of area averaged simulated monthly precipitation sums [mm/month]"
      # Format layout 
      p.sc <- p.sc %>% layout(title = paste("Region: ", input$rcm.cc.region),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Months",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = ylab,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE))
      
      if (input$rcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      
      p.sc$elementId <- NULL
      p.sc
      })
    
    output$rcm.sc.pr <- rcm.sc.pr
    
    output$hydro.sc.pr <- rcm.sc.pr
    
    output$rcm.sc.tas.data <- DT::renderDataTable({
      
      rcm.meta.tas <- rcm.meta.tas.reactive()
      df <- rcm.sc.tas.reactive()
      
      if (input$rcm.sim.sc == 'Selected Simulations') {
        rcm.meta.tas <- rcm.meta.tas[input$rowsRcm,]
      }
      
      caption <- paste('Simulated regional temperature [deg. C] assuming an 
                       intermediate emission scenarios for the',tolower(input$rcm.period),'averaged over',input$rcm.region,'region.
                       The climate models and their corresponding runs are listed in the second column and third columns, respectively. 
                       The last row in the table shows the estimated values from the referance data set (Observation).',sep= ' ')
      
      if (input$rcm.outputValues == 'Bias') {
        df <- df - df[,dim(df)[2]]      
        caption <- paste('Bias in simulated regional temperature [deg. C] assuming an 
                         intermediate emission scenarios for the',tolower(input$rcm.period),'averaged over',input$rcm.region,'region.
                         The climate models and their corresponding runs are listed in the second column and third columns, respectively. 
                         The last row in the table shows the estimated values from the referance data set (Observation).',sep= ' ')
      } else if (input$rcm.outputValues == 'Anomaly') {
        DF <- t(df)
        df <- as.data.frame(t(DF - rowMeans(DF)))
        caption <- paste('Simulated regional temperature anomalies [deg. C] assuming an 
                         intermediate emission scenarios for the',tolower(input$rcm.period),'averaged over',input$rcm.region,'region.
                         The climate models and their corresponding runs are listed in the second column and third columns, respectively. 
                         The last row in the table shows the estimated values from the referance data set (Observation).',sep= ' ')
      } else if (input$rcm.outputValues == 'Change') {
        df <- rcm.sc.tas.reactive() - rcm.sc.tas.present()
        caption <- paste('Absolute changes in regional temperature [deg. C] assuming an 
                         intermediate emission scenarios for the',tolower(input$rcm.period),'averaged over',input$rcm.region,'region.
                         The climate models and their corresponding runs are listed in the second column and third columns, respectively. 
                         Changes are computed with regards to the reference period 1981-2010.',sep= ' ')
      }
      
      df.format <- t(round(df,digits = 2))
      colnames(df.format) <- month.abb
      df.format <- data.frame(N = c(1:(dim(df.format)[1]-1),0), 
                              GCM = c(as.character(as.vector(rcm.meta.tas$gcm)),'EOBS'), 
                              Run = c(as.character(as.vector(rcm.meta.tas$gcm_rip)),'EOBS'),
                              RCM = c(as.character(as.vector(rcm.meta.tas$rcm)),'EOBS'),
                              df.format,stringsAsFactors = FALSE)
      DT::datatable(df.format,
                    caption = caption, 
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
    
    output$rcm.sc.pr.data <- DT::renderDataTable({
      
      rcm.meta.pr <- rcm.meta.pr.reactive()
      df <- rcm.sc.pr.reactive()
      
      if (input$rcm.sim.sc == 'Selected Simulations') {
        rcm.meta.pr <- rcm.meta.pr[input$rowsRcm,]
      }
      
      caption <- paste('Simulated regional precipitation [mm/month] assuming an 
                       intermediate emission scenarios for the',tolower(input$rcm.period),'averaged over',input$rcm.region,'region.
                       The climate models and their corresponding runs are listed in the second column and third columns, respectively. 
                       The last row in the table shows the estimated values from the referance data set (Observation).',sep= ' ')
      
      if (input$rcm.outputValues == 'Bias') {
        df <- ((df - df[,dim(df)[2]])/df[,dim(df)[2]]) * 100
        caption <- paste('Bias in simulated regional precipitation [%] assuming an 
                         intermediate emission scenarios for the',tolower(input$rcm.period),'averaged over',input$rcm.region,'region.
                         The climate models and their corresponding runs are listed in the second column and third columns, respectively. 
                         The last row in the table shows the estimated values from the referance data set (Observation).',sep= ' ')
      } else if (input$rcm.outputValues == 'Anomaly') {
        DF <- t(df)
        df <- as.data.frame(t(DF - rowMeans(DF)))
        caption <- paste('Simulated regional precipitation anomalies [mm/month] assuming an 
                         intermediate emission scenarios for the',tolower(input$rcm.period),'averaged over',input$rcm.region,'region.
                         The climate models and their corresponding runs are listed in the second column and third columns, respectively. 
                         The last row in the table shows the estimated values from the referance data set (Observation).',sep= ' ')
      } else if (input$rcm.outputValues == 'Change') {
        df <- ((rcm.sc.pr.reactive() - rcm.sc.pr.present())/ rcm.sc.pr.present()) * 100
        caption <- paste('Relative changes in regional precipitation [%] assuming an 
                         intermediate emission scenarios for the',tolower(input$rcm.period),'averaged over',input$rcm.region,'region.
                         The climate models and their corresponding runs are listed in the second column and third columns, respectively. 
                         Changes are computed with regards to the reference period 1981-2010',sep= ' ')
      }
      
      
      df.format <- t(round(df,digits = 2))
      colnames(df.format) <- month.abb
      df.format <- data.frame(N = c(1:(dim(df.format)[1]-1),0), 
                              Model = c(as.character(as.vector(rcm.meta.pr$gcm)),'EOBS'), 
                              Run = c(as.character(as.vector(rcm.meta.pr$gcm_rip)),'EOBS'),
                              RCM = c(as.character(as.vector(rcm.meta.pr$rcm)),'EOBS'),
                              df.format,stringsAsFactors = FALSE)
      DT::datatable(caption = caption, 
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
    
    output$rcm.scatter <- renderPlotly({
      #
      rcm.meta.pr <- rcm.meta.pr.reactive()
      dpr <- rcm.sc.pr.reactive()
      rcms <- names(dpr)
      if (input$rcm.outputValues == 'Bias')
        dpr <- ((dpr - dpr[,dim(dpr)[2]]) / dpr[,dim(dpr)[2]]) * 100
      else if (input$rcm.outputValues == 'RMSE') {
        dpr <- sqrt((((dpr - dpr[,dim(dpr)[2]]) / dpr[,dim(dpr)[2]]))^2) * 100
      } else if (input$rcm.outputValues == 'Anomaly') {
        DF <- t(dpr)
        dpr <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$rcm.outputValues == 'Change') {
        dpr <- ((dpr - rcm.sc.pr.present())/ rcm.sc.pr.present()) * 100
      } 
      
      rcm.meta.tas <- rcm.meta.tas.reactive()
      
      dtas <- rcm.sc.tas.reactive()
      if (input$rcm.outputValues == 'Bias')
        dtas <- dtas - dtas[,dim(dtas)[2]]
      else if (input$rcm.outputValues == 'RMSE') {
        dtas <- sqrt((dtas-dtas[,dim(dtas)[2]])^2)
      } else if (input$rcm.outputValues == 'Anomaly') {
        DF <- t(dtas)
        dtas <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$rcm.outputValues == 'Change') {
        dtas <- dtas - rcm.sc.tas.present()
      }
      
      if (!is.null(input$rowsRcm))
        if (input$rcm.sim.sc == 'Selected Simulations') {
          dpr <- dpr[input$rowsRcm,]
          dtas <- dtas[input$rowsRcm,]
          rcm.meta.tas <- rcm.meta.tas[input$rowsRcm,]
          rcm.meta.pr <- rcm.meta.pr[input$rowsRcm,]
          rcms <- rcms[input$rowsRcm]
        } 
      
      rcm.name <- function(i) {
        rcmi <- rcm.meta.pr[i,c('gcm','gcm_rip','rcm')]
        rcmi[is.na(rcmi)] <- ''
        return(paste(as.character(as.matrix(rcmi)),collapse = '_'))
      }
      inst.name <- function(i) {
        rcmi <- rcm.meta.pr[i,c('model_id')]
        rcmi[is.na(rcmi)] <- ''
        return(paste(as.character(as.matrix(rcmi)),collapse = '_'))
      }
      rcmall <- c(sapply(1:dim(rcm.meta.pr)[1],rcm.name),'ERAINT')
      rcm.inst <- c(sapply(1:dim(rcm.meta.pr)[1],inst.name),'ERAINT')
      
      df <- data.frame(dtas = as.numeric(round(colMeans(dtas),digits = 2)),dpr = as.numeric(round(colMeans(dpr),digits = 2)),
                       rcm.name = rcmall, inst.name = rcm.inst,stringsAsFactors = FALSE)
      
      if (is.element(input$rcm.groupBy,c('None','---'))) {
        id <- 1 : (length(df$dtas) - 1)
        lev <- levels(factor(id))
      }
      else { 
        id <- as.integer(factor(base::subset(rcm.meta.pr, select = input$rcm.groupBy)[[1]]))
        lev <- levels(factor(base::subset(rcm.meta.pr, select = input$rcm.groupBy)[[1]]))
      }
      
      rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                    'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                    'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                    'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                    'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                    'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                    'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                    'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
      
      rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                   'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                   'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                   'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                   'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                   'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                   'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
      
      
      # Color by
      if (input$rcm.colorBy == 'Group') {
        colsa <- rgbcolsa[id]
        cols <- rgbcols[id]
      } else {
        colsa <- rgbcolsa
        cols <- rgbcols
      }
      
      ## Create the plot
      p.sc <- plot_ly(df)
      if ((input$rcm.chart.type == 'Individual Simulations')) {
        ## Add all Simulations
        if (is.null(input$rowsRcm)) {
          for (rcm in rcms) {
            i <- which(is.element(rcms,rcm))
            #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
            leg.name <- paste(paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','gcm_rip','rcm')])),collapse = ' '),
                              paste(substr(lev[id[i]],1,5),'...',sep=''),
                              sep = ' ')
            grp.name <- paste('Group',id[i],sep='')
            
            if (is.element(input$rcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = ",i,", symbol = 3,size = 12,opacity = 0.7,line = list(width = 2,color = '#FFFFFF')))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = cols[",i,"], symbol = 3,size = 12,opacity=0.7,line = list(width = 2,color = '#FFFFFF')))",sep='')))
            
          }
          ## Highlight selected Simulations in tab:models
        } else {
          im <- input$rowsRcm
          for (i in 1:length(im)) {
            leg.name <- paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','gcm_rip','rcm')])),collapse = ' ')
            grp.name <- paste('Group',id[i],sep='')
            rcm <- rcms[i]
            if (is.element(input$rcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = ",i,", symbol = 3,size = 12,opacity = 0.7,line = list(width = 1)))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = cols[im[",i,"]], symbol = 3,size = 12,opacity=0.7,line = list(width = 1))",sep='')))
          }
          }
          } 
      
      if (length(df$dpr) > 1 | length(df$dtas) > 1) {
        # p.sc <-  p.sc  %>%  layout(shapes = list(list(name = 'Env. 90% of all sim',type = 'circle',
        #                                               xref = 'x', x0 = quantile(df$dtas[-length(df$dtas)],probs = 0.05,na.rm = TRUE), x1 = quantile(df$dtas[-length(df$dtas)],0.95,na.rm=TRUE),
        #                                               yref = 'y', y0 = quantile(df$dpr[-length(df$dpr)],0.05,na.rm=TRUE), y1 = quantile(df$dpr[-length(df$dpr)],0.95,na.rm=TRUE),
        #                                               fillcolor = 'rgba(255,127,80,0.4)', line = list(color = 'rgba(255,127,80,0.8)'),
        #                                               opacity = 0.4),
        #                                          list(name = 'Env. of all sim.',type = 'circle',
        #                                               xref = 'x', x0 = min(df$dtas[-length(df$dtas)],na.rm=TRUE), x1 = max(df$dtas[-length(df$dtas)],na.rm=TRUE),
        #                                               yref = 'y', y0 = min(df$dpr[-length(df$dpr)],na.rm=TRUE), y1 = max(df$dpr[-length(df$dpr)],na.rm=TRUE),
        #                                               fillcolor = 'rgba(255,127,80,0.4)', line = list(color = 'rgba(255,127,80,0.8)'),
        #                                               opacity = 0.3)))
        
        dfe.70 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.6827,draw = FALSE)
        dfe.95 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9545,draw = FALSE)
        dfe.99 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9973,draw = FALSE)
        p.sc <- p.sc %>% 
          add_polygons(x = dfe.99[,1],y = dfe.99[,2] , name = '99% Confidence Level',
                       fillcolor = 'rgba(255,127,80,0.5)',
                       line = list(color = 'rgba(255,127,80,0.6)'),
                       opacity = 0.5) %>%
          add_polygons(x = dfe.95[,1],y = dfe.95[,2] , name = '95% Confidence Level',
                       fillcolor = 'rgba(255,127,80,0.6)',
                       line = list(color = 'rgba(255,127,80,0.7)'),
                       opacity = 0.7) %>% 
          add_polygons(x = dfe.70[,1],y = dfe.70[,2] , name = '70% Confidence Level',
                       fillcolor = 'rgba(255,127,80,0.7)',
                       line = list(color = 'rgba(255,127,80,0.8)'),
                       opacity = 0.8)  
      }
      
      if ((input$rcm.chart.type == 'Ensemble of All Simulations') | (input$rcm.chart.type == "Both - Ensemble & Individual Simulations"))
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
      #}
      
      if (input$rcm.legend.sc == 'Display') {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = TRUE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'ERAINT', hoverinfo = 'text+x+y',text='ERAINT',showlegend = TRUE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      } else {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = FALSE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'ERAINT', hoverinfo = 'text+x+y',text='ERAINT',showlegend = FALSE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      }
      if (input$rcm.outputValues == 'Bias') {
        ylab <- 'Bias (absolute) in annual means of regional temperature values [deg. C]'
        xlab <- 'Bias (relative) in annual means of regional precitation values [%]'
      } else if (input$rcm.outputValues == 'Bias') {
        ylab <- 'RMSE (absolute) in annual means of regional temperature values [deg. C]'
        xlab <- 'RMSE (relative) in annual means of regional precitation values [%]'
      } else if (input$rcm.outputValues == 'Anomaly') {
        ylab <- 'Anomaly (absolute) in annual means of regional temperature values [deg. C]'
        xlab <- 'Anomaly (relative) in annual means of regional precitation values [%]'
      } else if (input$rcm.outputValues == 'Change') {
        ylab <- 'Change (absolute) in annual means of regional temperature values [deg. C]'
        xlab <- 'Change (relative) in annual means of regional precitation values [%]'
      } else  {
        ylab <- 'Annual means of regional temperature values [deg. C]'
        xlab <- 'Annual means of regional precitation values [mm/month]'
      }
      
      p.sc <- p.sc %>% layout(title = paste('Region : ',input$rcm.region),
        paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
        xaxis = list(title = ylab,
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     tickcolor = 'rgb(127,127,127)',
                     ticks = 'outside',
                     zeroline = TRUE),
        yaxis = list(title = xlab,
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     tickcolor = 'rgb(127,127,127)',
                     ticks = 'outside',
                     zeroline = TRUE))
      
      if (input$rcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      else
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      #rcm.dtdp$elementId <- NULL
      # rcm.dtdp
      p.sc$elementId <- NULL
      p.sc
          })
    
    output$rcm.cc.scatter.pu <- renderPlotly({
      #
      rcm.meta.pr <- rcm.meta.pr.reactive.pu()
      dpr <- rcm.sc.pr.reactive.cc.pu()
      rcms <- names(dpr)
      dpr <- ((dpr - rcm.sc.pr.present.cc.pu())/ rcm.sc.pr.present.cc.pu()) * 100
      
      rcm.meta.tas <- rcm.meta.tas.reactive.cc.pu()
      
      dtas <- rcm.sc.tas.reactive.cc.pu() - rcm.sc.tas.present.cc.pu()
      
      rcm.name <- function(i) {
        rcmi <- rcm.meta.pr[i,c('gcm','gcm_rip','rcm')]
        rcmi[is.na(rcmi)] <- ''
        return(paste(as.character(as.matrix(rcmi)),collapse = '_'))
      }
      inst.name <- function(i) {
        rcmi <- rcm.meta.pr[i,c('model_id')]
        rcmi[is.na(rcmi)] <- ''
        return(paste(as.character(as.matrix(rcmi)),collapse = '_'))
      }
      rcmall <- c(sapply(1:dim(rcm.meta.pr)[1],rcm.name),'ERAINT')
      rcm.inst <- c(sapply(1:dim(rcm.meta.pr)[1],inst.name),'ERAINT')
      
      df <- data.frame(dtas = as.numeric(round(colMeans(dtas),digits = 2)),dpr = as.numeric(round(colMeans(dpr),digits = 2)),
                       rcm.name = rcmall, inst.name = rcm.inst,stringsAsFactors = FALSE)
      
      id <- 1 : (length(df$dtas) - 1)
      lev <- levels(factor(id))
      
      rgbcolsa <- c('rgba(45,51,38,0.5)', 'rgba(87,77,102,0.5)', 'rgba(255,191,200,0.5)', 'rgba(140,129,105,0.5)', 'rgba(234,191,255,0.5)', 'rgba(172,230,195,0.5)',
                    'rgba(86,105,115,0.5)', 'rgba(115,86,94,0.5)', 'rgba(230,195,172,0.5)', 'rgba(255,234,191,0.5)', 'rgba(124,140,105,0.5)', 'rgba(51,26,43,0.5)',
                    'rgba(191,96,172,0.5)', 'rgba(184,204,102,0.5)', 'rgba(153,87,77,0.5)', 'rgba(96,134,191,0.5)', 'rgba(230,115,145,0.5)', 'rgba(255,145,128,0.5)', 
                    'rgba(229,161,115,0.5)', 'rgba(22,58,89,0.5)', 'rgba(85,89,22,0.5)', 'rgba(127,83,32,0.5)', 'rgba(80,179,45,0.5)', 'rgba(18,51,13,0.5)', 'rgba(64,16,22,0.5)',
                    'rgba(22,16,64,0.5)', 'rgba(86,29,115,0.5)', 'rgba(54,98,217,0.5)', 'rgba(255,191,64,0.5)', 'rgba(61,182,242,0.5)', 'rgba(126,57,230,0.5)', 'rgba(51,38,13,0.5)', 
                    'rgba(178,0,95,0.5)', 'rgba(0,128,85,0.5)', 'rgba(26,0,191,0.5)', 'rgba(255,0,238,0.5)', 'rgba(178,0,0,0.5)', 'rgba(0,202,217,0.5)', 'rgba(0,230,153,0.5)', 
                    'rgba(0,255,34,0.5)', 'rgba(204,0,54,0.5)', 'rgba(102,0,14,0.5)', 'rgba(229,92,0,0.5)', 'rgba(0,107,115,0.5)', 'rgba(77,0,51,0.5)', 'rgba(204,255,0,0.5)', 
                    'rgba(140,112,0,0.5)', 'rgba(12,89,0,0.5)')
      
      rgbcols <- c('rgb(45,51,38)', 'rgb(87,77,102)', 'rgb(255,191,200)', 'rgb(140,129,105)', 'rgb(234,191,255)', 'rgb(172,230,195)', 'rgb(86,105,115)', 
                   'rgb(115,86,94)', 'rgb(230,195,172)', 'rgb(255,234,191)', 'rgb(124,140,105)', 'rgb(51,26,43)', 'rgb(191,96,172)', 'rgb(184,204,102)', 
                   'rgb(153,87,77)', 'rgb(96,134,191)', 'rgb(230,115,145)', 'rgb(255,145,128)', 'rgb(229,161,115)', 'rgb(22,58,89)', 'rgb(85,89,22)', 
                   'rgb(127,83,32)', 'rgb(80,179,45)', 'rgb(18,51,13)', 'rgb(64,16,22)', 'rgb(22,16,64)', 'rgb(86,29,115)', 'rgb(54,98,217)', 'rgb(255,191,64)',
                   'rgb(61,182,242)', 'rgb(126,57,230)', 'rgb(51,38,13)', 'rgb(178,0,95)', 'rgb(0,128,85)', 'rgb(26,0,191)', 'rgb(255,0,238)', 'rgb(178,0,0)', 
                   'rgb(0,202,217)', 'rgb(0,230,153)', 'rgb(0,255,34)', 'rgb(204,0,54)', 'rgb(102,0,14)', 'rgb(229,92,0)', 'rgb(0,107,115)', 'rgb(77,0,51)', 
                   'rgb(204,255,0)', 'rgb(140,112,0)', 'rgb(12,89,0)')
      
      
      # Color by
      colsa <- rgbcolsa[id]
      cols <- rgbcols[id]
      
      ## Create the plot
      p.sc <- plot_ly(df)
      if ((input$rcm.cc.chart.type == 'Individual Simulations')) {
        ## Add all Simulations
        for (rcm in rcms) {
          i <- which(is.element(rcms,rcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','gcm_rip','rcm')])),collapse = ' '),
                            paste(substr(lev[id[i]],1,5),'...',sep=''),
                            sep = ' ')
          grp.name <- paste('Group',id[i],sep='')
          
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                  name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  marker = list(color = ",i,", symbol = 3,size = 12,opacity=0.7,line = list(width = 2,color = '#FFFFFF')))",sep='')))
          
        }
      } 
      
      if (length(df$dpr) > 1 | length(df$dtas) > 1) {
        # p.sc <-  p.sc  %>%  layout(shapes = list(list(name = 'Env. 90% of all sim',type = 'circle',
        #                                               xref = 'x', x0 = quantile(df$dtas[-length(df$dtas)],probs = 0.05,na.rm = TRUE), x1 = quantile(df$dtas[-length(df$dtas)],0.95,na.rm=TRUE),
        #                                               yref = 'y', y0 = quantile(df$dpr[-length(df$dpr)],0.05,na.rm=TRUE), y1 = quantile(df$dpr[-length(df$dpr)],0.95,na.rm=TRUE),
        #                                               fillcolor = 'rgba(255,127,80,0.4)', line = list(color = 'rgba(255,127,80,0.8)'),
        #                                               opacity = 0.4),
        #                                          list(name = 'Env. of all sim.',type = 'circle',
        #                                               xref = 'x', x0 = min(df$dtas[-length(df$dtas)],na.rm=TRUE), x1 = max(df$dtas[-length(df$dtas)],na.rm=TRUE),
        #                                               yref = 'y', y0 = min(df$dpr[-length(df$dpr)],na.rm=TRUE), y1 = max(df$dpr[-length(df$dpr)],na.rm=TRUE),
        #                                               fillcolor = 'rgba(255,127,80,0.4)', line = list(color = 'rgba(255,127,80,0.8)'),
        #                                               opacity = 0.3)))
        
        dfe.70 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.6827,draw = FALSE)
        dfe.95 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9545,draw = FALSE)
        dfe.99 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9973,draw = FALSE)
        p.sc <- p.sc %>% 
          add_polygons(x = dfe.99[,1],y = dfe.99[,2] , name = '99% Confidence Level',
                       fillcolor = 'rgba(255,127,80,0.5)',
                       line = list(color = 'rgba(255,127,80,0.6)'),
                       opacity = 0.5) %>%
          add_polygons(x = dfe.95[,1],y = dfe.95[,2] , name = '95% Confidence Level',
                       fillcolor = 'rgba(255,127,80,0.6)',
                       line = list(color = 'rgba(255,127,80,0.7)'),
                       opacity = 0.7) %>% 
          add_polygons(x = dfe.70[,1],y = dfe.70[,2] , name = '70% Confidence Level',
                       fillcolor = 'rgba(255,127,80,0.7)',
                       line = list(color = 'rgba(255,127,80,0.8)'),
                       opacity = 0.8)  
      }
      
      if ((input$rcm.cc.chart.type == 'Ensemble of All Simulations') | (input$rcm.cc.chart.type == "Both - Ensemble & Individual Simulations"))
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
      #}
      
      if (input$rcm.legend.sc == 'Display') {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = TRUE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'ERAINT', hoverinfo = 'text+x+y',text='ERAINT',showlegend = TRUE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      } else {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = FALSE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'ERAINT', hoverinfo = 'text+x+y',text='ERAINT',showlegend = FALSE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      }
      ylab <- 'Change (absolute) in annual means of regional temperature values [deg. C]'
      xlab <- 'Change (relative) in annual means of regional precitation values [%]'
      
      p.sc <- p.sc %>% layout(title = paste('Region:', input$rcm.cc.region),
        paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
        xaxis = list(title = ylab,
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     tickcolor = 'rgb(127,127,127)',
                     ticks = 'outside',
                     zeroline = TRUE),
        yaxis = list(title = xlab,
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     tickcolor = 'rgb(127,127,127)',
                     ticks = 'outside',
                     zeroline = TRUE))
      
      if (input$rcm.legend.sc == 'Hide')
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      else
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      #rcm.dtdp$elementId <- NULL
      # rcm.dtdp
      p.sc$elementId <- NULL
      p.sc
      })
    
    
    output$rcm.scatter.data <- DT::renderDataTable({
      
      rcm.meta.pr <- rcm.meta.pr.reactive()
      dpr <- rcm.sc.pr.reactive()
      rcms <- names(dpr)
      if (input$rcm.outputValues == 'Bias')
        dpr <- ((dpr - dpr[,dim(dpr)[2]]) / dpr[,dim(dpr)[2]]) * 100
      else if (input$rcm.outputValues == 'RMSE') {
        dpr <- sqrt((((dpr - dpr[,dim(dpr)[2]]) / dpr[,dim(dpr)[2]]))^2) * 100
      } else if (input$rcm.outputValues == 'Anomaly') {
        DF <- t(dpr)
        dpr <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$rcm.outputValues == 'Change') {
        dpr <- ((dpr - rcm.sc.pr.present())/ rcm.sc.pr.present()) * 100
      } 
      
      rcm.meta.tas <- rcm.meta.tas.reactive()
      
      dtas <- rcm.sc.tas.reactive()
      if (input$rcm.outputValues == 'Bias')
        dtas <- dtas - dtas[,dim(dtas)[2]]
      else if (input$rcm.outputValues == 'RMSE') {
        dtas <- sqrt((dtas-dtas[,dim(dtas)[2]])^2)
      } else if (input$rcm.outputValues == 'Anomaly') {
        DF <- t(dtas)
        dtas <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$rcm.outputValues == 'Change') {
        dtas <- dtas - rcm.sc.tas.present()
      }
      
      if (!is.null(input$rowsRcm))
        if (input$rcm.sim.sc == 'Selected Simulations') {
          dpr <- dpr[input$rowsRcm,]
          dtas <- dtas[input$rowsRcm,]
          rcm.meta.tas <- rcm.meta.tas[input$rowsRcm,]
          rcm.meta.pr <- rcm.meta.pr[input$rowsRcm,]
          rcms <- rcms[input$rowsRcm]
        } 
      
      rcm.name <- function(i) {
        rcmi <- rcm.meta.pr[i,c('gcm','gcm_rip','rcm')]
        rcmi[is.na(rcmi)] <- ''
        return(paste(as.character(as.matrix(rcmi)),collapse = '_'))
      }
      inst.name <- function(i) {
        rcmi <- rcm.meta.pr[i,c('model_id')]
        rcmi[is.na(rcmi)] <- ''
        return(paste(as.character(as.matrix(rcmi)),collapse = '_'))
      }
      rcmall <- c(sapply(1:dim(rcm.meta.pr)[1],rcm.name),'ERAINT')
      rcm.inst <- c(sapply(1:dim(rcm.meta.pr)[1],inst.name),'ERAINT')
      
      df <- data.frame(dtas = as.numeric(round(colMeans(dtas),digits = 2)),dpr = as.numeric(round(colMeans(dpr),digits = 2)),
                       rcm.name = rcmall, inst.name = rcm.inst,stringsAsFactors = FALSE)
      
      caption <- paste('Annual means of monthly estimates of both regional temperature (deg. C) and precipitation (mm/month) assuming an 
                       intermediate emission scenarios for the',tolower(input$rcm.period),'averaged over',input$rcm.region,'region.
                       The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                       The last row in the table shows the estimated values from the referance data set (last row).',
                       sep= ' ')
      
      if (input$rcm.outputValues == 'Bias') {
        caption <- paste('Annual means of Biases in monthly estimates of both regional temperature (deg. C) and precipitation (%) assuming an 
                         intermediate emission scenarios for the',tolower(input$rcm.period),'averaged over',input$rcm.region,'region.
                         The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                         The bias is computed as the deviation from the referance data set (last row).',
                         sep= ' ')
      } else if (input$rcm.outputValues == 'RMSE'){
        caption <- paste('Annual means of RMSE in monthly estimates of both regional temperature (deg. C) and precipitation (%) assuming an 
                         intermediate emission scenarios for the',tolower(input$rcm.period),'averaged over',input$rcm.region,'region.
                         The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                         The RMSE is computed as the square root mean of deviations from the referance data set (last row).',
                         sep= ' ')
        
      } else if (input$rcm.outputValues == 'Change'){
        caption <- paste('Annual means of Changes in monthly estimates of both regional temperature (deg. C) and precipitation (%) assuming an 
                         intermediate emission scenarios for the',tolower(input$rcm.period),'averaged over',input$rcm.region,'region.
                         The climate models and their corresponding runs are listed in the second column, respectively. 
                         The RMSE is computed as the square root mean of deviations from the referance data set (last row).',
                         sep= ' ')
        
      }
      
      #df.format <- t(round(df,digits = 1))
      #colnames(df.format) <- month.abb
      df.format <- data.frame(N = c(1:dim(df)[1]), 
                              Model = df[,3], 
                              Temperature = df[,1],
                              Preciptiation = df[,2],
                              stringsAsFactors = FALSE)
      
      DT::datatable(df.format,
                    caption = caption, 
                    selection = list(mode = 'multiple',target = 'row'), 
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
      DT::datatable(rcm.meta.all,
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
    
    # output$shopcart <- renderInfoBox({
    #   infoBox(
    #     "Shopping box",
    #     input$rows,color = 'aqua',
    #     icon = icon("credit-card")
    #   )
    # })  
    # 
    # output$messageMenu <- renderMenu({
    #   msgs <- apply(msgData(), 1, function(row) {
    #     messageItem(from = row[["from"]], message = row[["message"]])
    #   })
    #   
    #   ntfs <- apply(notData(), 1, function(row) {
    #     notificationItem(text = row[["message"]], status = row[["status"]],
    #                      icon = icon("shopping-cart", lib = "glyphicon"))
    #   })
    #   
    #   dropdownMenu(type = "messages", .list = msgs)
    #   dropdownMenu(type = "notifications", .list = ntfs, icon = icon("shopping-cart", lib = "glyphicon"))
    #   
    # })
    
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
      selData <- dcc %>% filter(season == selS, rcp == selR , period == selP)
      # browser()
      # plot
      for (i in 1:length(selData[,6])) {
        
        leg.name <- paste(paste(as.character(as.matrix(selData[i,6])),collapse = ' '),'...',sep='')
        
        eval(parse(text = paste("p <- p %>% add_trace(data=selData, x = ~ t2m[",i,"],y = ~ mu[",i,"]*100,type = 'scatter', 
                                name = leg.name, mode = 'markers', hoverinfo = 'text+x+y',text=leg.name,
                                showlegend = TRUE, 
                                marker = list(color = ",i,", symbol = 3,size = 12,opacity=0.7,line = list(width = 2,color = '#FFFFFF')))",sep='')))
      }
      
      # p <- p %>% add_trace(data = selData, name = 'All',x = ~t2m, y = ~mu, type = "scatter",
      #                      mode = "markers", colors = I('blue'), 
      #                      marker = list(size = 14, symbol = '+', color = 'blue', line = list(width = 1,color = I('blue')),opacity = 0.2), 
      #                      text = ~paste("<br> Model: ", selData[,6], "</br> Score:", 2)) 
      
      # if (length(input$rows)>0) {
      #   
      #   p <- p %>% add_trace(data = selData %>% filter(model == input$rows), name = 'Changes',x = ~t2m, y = ~mu, type = "scatter",
      #                        mode = "markers", colors = I('blue'), 
      #                        marker = list(size = 14, symbol = 'circles', color = 'blue', line = list(width = 1,color = I('blue')),opacity = 0.2), 
      #                        text = ~paste("<br> Model: ", selData[,6], "</br> Score:", 2))
      # }
      # 
      de <- dataEllipse(x = as.matrix(selData[,1:2]),levels = 0.90,draw = FALSE) # 0.05,0.1,0.25,0.5,0.75,0.9,
      #dev.off()
      
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
      
      
      if (length(input$rows.cc) > 0) { # replaced selModel
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
               yaxis = list(dtick = 1, title = "Precipitation Change [%]",zerolinewidth = 1),
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
    
    #' output$gcm.scatter <- renderPlotly({
    #'   
    #'   season <- switch(input$gcm.season,
    #'                    'Annual (All seasons)'='ann',
    #'                    'Winter (DJF)'=c('dec','jan','feb'),
    #'                    'Spring (MAM)'=c('mar','apr','may'),
    #'                    'Summer (JJA)'=c('jun','jul','aug'),
    #'                    'Autumn (SON)'=c('sep','oct','nov'))
    #'   #'annual mean'='ann','winter'='djf','spring'='mam',
    #'   #'summer'='jja','autumn'='son')
    #'   period <- switch(tolower(as.character(input$period)),
    #'                    "2071-2100"='ff',
    #'                    "2021-2050"='nf')
    #'   
    #'   gcms <- names(stats$tas$ff)
    #'   if(tolower(input$region)=="global") {
    #'     coord <- list(lon=c(-180,180),lat=c(-90,90))
    #'     dtas <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
    #'       stats$tas[[period]][[gcm]][["mean"]][[s]])) - 
    #'         mean(sapply(season, function(s)
    #'           stats$tas$present[[gcm]][["mean"]][[s]])))
    #'     dpr <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
    #'       stats$pr[[period]][[gcm]][["mean"]][[s]])) - 
    #'         mean(sapply(season, function(s)
    #'           stats$pr$present[[gcm]][["mean"]][[s]])))
    #'     #dtas <- sapply(gcms, function(gcm) stats$tas[[period]][[gcm]][["mean"]][[season]] - 
    #'     #                 stats$tas$present[[gcm]][["mean"]][[season]]) 
    #'     #dpr <- sapply(gcms, function(gcm) stats$pr[[period]][[gcm]][["mean"]][[season]] - 
    #'     #                stats$pr$present[[gcm]][["mean"]][[season]])
    #'   } else {
    #'     #i.srex <- which(srex$name==input$region)
    #'     region <- 'ALA' #srex$label[i.srex]
    #'     dtas <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
    #'       stats$tas[[period]][[gcm]][[region]][["mean"]][[s]])) - 
    #'         mean(sapply(season, function(s) 
    #'           stats$tas$present[[gcm]][[region]][["mean"]][[s]])))
    #'     dpr <- sapply(gcms, function(gcm) mean(sapply(season, function(s)
    #'       stats$pr[[period]][[gcm]][[region]][["mean"]][[s]])) - 
    #'         mean(sapply(season, function(s) 
    #'           stats$pr$present[[gcm]][[region]][["mean"]][[s]])))
    #'     #dtas <- sapply(gcms, function(gcm) stats$tas[[period]][[gcm]][[region]][["mean"]][[season]] - 
    #'     #                stats$tas$present[[gcm]][[region]][["mean"]][[season]])
    #'     #dpr <- sapply(gcms, function(gcm) stats$pr[[period]][[gcm]][[region]][["mean"]][[season]] - 
    #'     #                                  stats$pr$present[[gcm]][[region]][["mean"]][[season]])
    #'   }
    #'   im <- as.numeric(gsub(":.*","",input$gcms))
    #'   
    #'   d <- data.frame(var1 = dtas, var2 = dpr*(60*60*24))
    #'   # colnames(d) <- c('Temperature Change','Precipitation Change')
    #'   p <- plot_ly(d, x = ~var1, y = ~var2, type = 'scatter', color = paste('gcms',1:9), 
    #'                # Hover text:
    #'                text = ~paste("Model N : ", im, '$<br> Score:', 2),
    #'                marker = list(size = 15, symbol = 'cross-open-dot',
    #'                              color = colscal(n=9),
    #'                              line = list(color = col2rgb('grey40'))),
    #'                mode = 'markers') 
    #'   
    #'   p <- p %>%
    #'     layout(title = paste(toupper(input$im), 
    #'                          "Climate Change signal realtive to the base period modeled by ", sep =' '),
    #'            xaxis = list(title = "Temperature Change [deg. C]"),
    #'            yaxis = list(title = "Precipitation Change [%]"))  
    #'   p  
    #'   
    #'   # scatterplot(dtas,dpr*(60*60*24),ix=NULL,xlim=input$tlim,ylim=input$plim,
    #'   #             xlab="Temperature change (deg C)",ylab="Precipitation change (mm/day)",
    #'   #             main=paste("Climate change assuming RCP4.5\npresent day (1981-2010) to",input$period),
    #'   #             show.legend=FALSE,im=im,
    #'   #             legend=seq(length(dtas)),pal=NULL,#pal="cat",pch=21,
    #'   #             pch=as.character(seq(length(dtas))),cex=1.5,lwd=1.5,new=FALSE)
    #' })
    #' 
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
  
  # observe(priority = -1,{
  #   showNotification(paste("Selected Simulations are : ",paste((input$rowsGcm),collapse = '/')),type = 'message')
  #   updateTabsetPanel(session, "tabs", 'score5.tabs')
  # })
  
  
  #observe(
  #  showNotification(
  #       tags$div(tags$p(tags$h4("We have additionally included statistics for European countries derived from the regional climate model simulations."))),
  #		 action = NULL, duration = 20, closeButton = TRUE,id = NULL, type = c("warning"),session = getDefaultReactiveDomain())
  #) 
  
  observe(
    showModal(modalDialog(
      footer = modalButton("Accept"),
      title = "Disclaimer",
      tags$div(tags$p(tags$h4("This web application is a prototype and provides a straighforward and simple evaluation of the quality of climate models in simulating 
                              basic climatic features such as the seasoanl cycle of mean air temperature and precipitation over various 
                              regions in the world. This prototype is developed through the C3S DECM project and is the copyright of the Norwegian Meteorological Institute (2018).
                              By clicking on 'Accept' you accept the terms of use in the"), tags$h4(tags$a(href= 'http://climate.copernicus.eu/disclaimer-privacy','Framework Agreement for Copernicus services')),
                      tags$h4('Any feedbacks are welcome!'),
                      tags$h5('email to abdelkader@met.no or send your feedback from the following website'), tags$a(href= 'https://climatedatasite.net','https://climatedatasite.net')
      ))))
    )
  
  
  # observe(
  #   if ((input$gcm.stat == 'Spatial Correlation') & (input$gcm.period != 'present (1981-2010)') |
  #       (input$gcm.stat.pu == 'Spatial Correlation') & (input$gcm.period.pu != 'present (1981-2010)') |
  #       (input$rcm.stat == 'Spatial Correlation') & (input$rcm.period != 'present (1981-2010)') |
  #       (input$rcm.stat.pu == 'Spatial Correlation') & (input$rcm.period.pu != 'present (1981-2010)'))
  #     
  #     showModal(modalDialog(
  #       footer = modalButton("Continue"),
  #       title = "Error",
  #       tags$h4('The spatial correlation is only computed for the present (1981-2010). Please refine your selection.')
  #     ))
  # )
  
  observeEvent(input$gcm.groupBy,{
    if (!is.element(input$gcm.groupBy,c('None','---')))
      updateSelectInput(inputId = "gcm.chart.type",session = session,
                        choices = c("Individual Simulations","Ensemble of All Simulations",
                                    "Box Plots of All Simulations"),
                        selected = "Individual Simulations")
    updateSelectInput(session = session, inputId = "gcm.sim.sc", label = "Simulations", 
                      choices = c("All Simulations","Selected Simulations","None"),
                      selected = "All Simulations")
  })
  observeEvent(input$gcm.colorBy,{
    if (!is.element(input$gcm.groupBy,c('None','---')))
      updateSelectInput(inputId = "gcm.chart.type",session = session,
                        choices = c("Individual Simulations","Ensemble of All Simulations","Ensemble of Selected Simulations",
                                    "Box Plots of All Simulations","Box Plots of Selected Simulations"),
                        selected = "Individual Simulations")
    updateSelectInput(session = session, inputId = "gcm.sim.sc", label = "Simulations", 
                      choices = c("All Simulations","Selected Simulations","None"),
                      selected = "All Simulations")
  })
  
  
  observeEvent(input$rowsGcm,{
    updateSelectInput(session = session, inputId = "gcm.sim.sc", label = "Simulations", 
                      choices = c("All Simulations","Selected Simulations","None"),
                      selected = "Selected Simulations")
  })
  
  output$frame <- renderUI({
    tags$iframe(src="http://157.249.177.25:3838/BarentsAtlas/",width = '100%', height = '950')
  })
  
  # GCM info text output
  output$simulation = renderInfoBox({
    txt <- tags$h5('The climate simulations constitue a representation of possible climate outcomes and are based on 
                   climate models that are run on different temporal and spatial scales to provide the best representation
                   of the climate signal over a region of interest and for a specific time horizon (past, present, or future). 
                   The climate simulations evaluated here are based on climate model ouptuts 
                   collected from the Climate Model Intercomparison Project - Phase5 (CMIP5), the Coordinated Regional Climate Downscaling Experiment over Europe (EURO-CORDEX),
                   and the Empirical-Statistical Downcaling project (ESD) at the Norwegian Meteorolocial Institute to produce the best estimates of 
                   global/regional/local climate signal that in turn can be used in impact studies.')   
    infoBox('What Climate Simulations evaluated here !',txt, icon = shiny::icon("table"),color = 'olive')
  })
  
  txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
  
  # GCM info text output
  output$figcaption.gcm.sc.tas = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.gcm.tas.pu = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the bias in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.gcm.tas.cc = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the changes in simulated historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.gcm.sc.tas.pu = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.gcm.sc.pr = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected monthly precipitation totals. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.gcm.pr.pu = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the bias in historical and projected monthly precipitation totals. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.gcm.sc.pr.pu = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected monthly precipitation totals. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.gcm.pr.cc = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the changes in seasonal cycle in historical and projected monthly precipitation totals. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.gcm.scatter = renderInfoBox({
    txt <- tags$h5('Interactive Scatter Plot showing surface air mean temperature VS mean monthly sums of precipitaiton. The orange star and red envelope show the mean and the spread from the multi-model ensemble of simulations. 
                   The black star shows the corresponding values from reanalysis data used as reference (ERAINT).')   
    infoBox('How to read the scatter plot!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.gcm.cc.scatter = renderInfoBox({
    txt <- tags$h5('Interactive Scatter Plot showing changes in surface air mean temperature VS mean monthly sums of precipitaiton. The orange star and red envelope show the mean and the spread from the multi-model ensemble of simulated changes. 
                   The black star shows the corresponding values from reanalysis data used as reference (ERAINT).')   
    infoBox('How to read the scatter plot!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  # RCM info text output
  output$figcaption.rcm.sc.tas = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.rcm.tas.cc = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the future changes seasonal cycle in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.rcm.sc.tas.pu = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.rcm.sc.pr = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected monthly precipitation totals. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.rcm.pr.cc = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the future changes in seasonal cycle in historical and projected monthly precipitation totals. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.rcm.sc.pr.pu = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected monthly precipitation totals. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.rcm.cc.scatter = renderInfoBox({
    txt <- tags$h5('Interactive charts evaluating the future changes in seasonal cycle in historical and projected monthly precipitation totals. The orange line and envelope show the mean and the spread from the multi-model ensemble of simulations. The black line shows the seasonal cycle from reanalysis data used as reference.')   
    infoBox('How to read the chart!',txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
  })
  
  output$figcaption.rcm.scatter = renderInfoBox({
    txt <- tags$h5('Interactive Scatter Plot showing surface air mean temperature VS mean monthly sums of precipitaiton. The orange star and red envelope show the mean and the spread from the multi-model ensemble of simulations. 
                   The black star shows the corresponding values from reanalysis data used as reference (ERAINT).')   
    infoBox('How to read the scatter plot!',txt, icon = shiny::icon("line-chart-o"),color = 'olive')
  })
  
  # Text info
  txtTips <- tags$h5('You can modify the type of output from the "Settings & Outputs" box and choose between options of showing individual simulations, envelope of the ensemble model simulations, or box plots. They let you show anomalies and group/colour the results according to the metadata. “Individual Simulations” allow double-click on specific climate models listed in the legend or the metadata table to isolate one or a group of simulations.')
  txtMoreTips <- tags$h5('Other options include zooming in/out, comparing simulations, and downloading the graphic. The types of evaluation includes the mean seasonal cycle of the mean as well as the spatial standard deviation or spatial correlation, and you can download the data and further details about the simulations by selecting the tabs labelled “Data” or “Metadata”. The evaluation shown here are for multi-model ensemble of CMIP5 RCP4.5 simulations.')
  txtRemember <- tags$h5('These simulations are based on models and data to represent the climate system. Those models are in turn based on coarse resolution, different parameterization schemes and simplifications of physical processes which systematically lead to deviations (biases) from the reference data.')
  
  figTips = renderInfoBox({
    infoBox('Tips on how to modify the chart to meet your needs!',txtTips, icon = shiny::icon("info-sign", lib = "glyphicon"),color = 'orange')
  })
  
  figMoreTips = renderInfoBox({
    infoBox('More Tips on how to use the chart!',txtMoreTips, icon = shiny::icon("plus-sign", lib = "glyphicon"),color = 'light-blue')
  })
  
  figRemember = renderInfoBox({
    infoBox('Recommendations on how to use the chart!',txtRemember, icon = shiny::icon("asterisk", lib = "glyphicon"),color = 'red')
  })
  
  output$figTips.gcm.tas <- figTips
  output$figMoreTips.gcm.tas <- figMoreTips
  output$figRemember.gcm.tas <- figRemember
  
  output$figTips.gcm.sc.tas.pu <- figTips
  output$figMoreTips.gcm.sc.tas.pu <- figMoreTips
  output$figRemember.gcm.sc.tas.pu <- figRemember
  
  output$figTips.gcm.tas.pu <- figTips
  output$figMoreTips.gcm.tas.pu <- figMoreTips
  output$figRemember.gcm.tas.pu <- figRemember
  
  output$figTips.gcm.pr <- figTips
  output$figMoreTips.gcm.pr <- figMoreTips
  output$figRemember.gcm.pr <- figRemember
  
  output$figTips.gcm.sc.pr.pu <- figTips
  output$figMoreTips.gcm.sc.pr.pu <- figMoreTips
  output$figRemember.gcm.sc.pr.pu <- figRemember
  
  output$figTips.gcm.pr.pu <- figTips
  output$figMoreTips.gcm.pr.pu <- figMoreTips
  output$figRemember.gcm.pr.pu <- figRemember
  
  output$figTips.gcm.pr.cc <- figTips
  output$figMoreTips.gcm.pr.cc <- figMoreTips
  output$figRemember.gcm.pr.cc <- figRemember
  
  output$figTips.gcm.tas.cc <- figTips
  output$figMoreTips.gcm.tas.cc <- figMoreTips
  output$figRemember.gcm.tas.cc <- figRemember
  
  output$figTips.gcm.scatter <- figTips
  output$figMoreTips.gcm.scatter <- figMoreTips
  output$figRemember.gcm.scatter <- figRemember
  
  output$figTips.gcm.cc.scatter <- figTips
  output$figMoreTips.gcm.cc.scatter <- figMoreTips
  output$figRemember.gcm.cc.scatter <- figRemember
  
  output$figTips.rcm.tas <- figTips
  output$figMoreTips.rcm.tas <- figMoreTips
  output$figRemember.rcm.tas <- figRemember
  
  output$figTips.rcm.tas.pu <- figTips
  output$figMoreTips.rcm.tas.pu <- figMoreTips
  output$figRemember.rcm.tas.pu <- figRemember
  
  output$figTips.rcm.sc.tas.pu <- figTips
  output$figMoreTips.rcm.sc.tas.pu <- figMoreTips
  output$figRemember.rcm.sc.tas.pu <- figRemember
  
  output$figTips.rcm.pr <- figTips
  output$figMoreTips.rcm.pr <- figMoreTips
  output$figRemember.rcm.pr <- figRemember
  
  output$figTips.rcm.pr.pu <- figTips
  output$figMoreTips.rcm.pr.pu <- figMoreTips
  output$figRemember.rcm.pr.pu <- figRemember
  
  output$figTips.rcm.sc.pr.pu <- figTips
  output$figMoreTips.rcm.sc.pr.pu <- figMoreTips
  output$figRemember.rcm.sc.pr.pu <- figRemember
  
  output$figTips.rcm.pr.cc <- figTips
  output$figMoreTips.rcm.pr.cc <- figMoreTips
  output$figRemember.rcm.pr.cc <- figRemember
  
  output$figTips.rcm.tas.cc <- figTips
  output$figMoreTips.rcm.tas.cc <- figMoreTips
  output$figRemember.rcm.tas.cc <- figRemember
  
  output$figTips.rcm.scatter <- figTips
  output$figMoreTips.rcm.scatter <- figMoreTips
  output$figRemember.rcm.scatter <- figRemember
  
  output$figTips.rcm.cc.scatter <- figTips
  output$figMoreTips.rcm.cc.scatter <- figMoreTips
  output$figRemember.rcm.cc.scatter <- figRemember
  
  output$figTips.rcm.cc.scatter <- figTips
  output$figMoreTips.rcm.cc.scatter <- figMoreTips
  output$figRemember.rcm.cc.scatter <- figRemember
  
  txtTable <- tags$h5('Monthly estimates of regional temperature assuming an intermediate emission scenarios for the present (1981-2010) averaged over Global region. The climate models and their corresponding runs are listed in the second and third columns, respectively. The last row in the table shows the estimated values from the referance data set (Observation)')
  
  output$tabcaption = renderInfoBox({
    infoBox('How to read the table!',txtTable, icon = shiny::icon("table"),color = 'orange')
  })
  
  observeEvent(input$gcm.sc.period.pu,{
    if (input$gcm.sc.period.pu != 'Present (1981-2010)')
      updateSelectInput(session = session, inputId = "gcm.sc.stat.pu", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation'))
    else 
      updateSelectInput(session = session, inputId = "gcm.sc.stat.pu", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation','Spatial Correlation')) 
    
  })
  
  observeEvent(input$gcm.period.pu,{
    if (input$gcm.period.pu != 'Present (1981-2010)')
      updateSelectInput(session = session, inputId = "gcm.stat.pu", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation'))
    else 
      updateSelectInput(session = session, inputId = "gcm.stat.pu", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation','Spatial Correlation')) 
    
  })
  observeEvent(input$gcm.period,{
    if (input$gcm.period != 'Present (1981-2010)')
      updateSelectInput(session = session, inputId = "gcm.stat", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation'))
    else 
      updateSelectInput(session = session, inputId = "gcm.stat", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation','Spatial Correlation')) 
    
  })
  
  observeEvent(input$rcm.period,{
    if (input$rcm.period != 'Present (1981-2010)')
      updateSelectInput(session = session, inputId = "rcm.stat", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation'))
    else 
      updateSelectInput(session = session, inputId = "rcm.stat", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation','Spatial Correlation')) 
    
  })
  observeEvent(input$gcm.cc.period,{
    if (input$gcm.cc.period != 'Present (1981-2010)')
      updateSelectInput(session = session, inputId = "gcm.cc.stat", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation'))
    else 
      updateSelectInput(session = session, inputId = "gcm.cc.stat", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation','Spatial Correlation')) 
    
  })
  
  observeEvent(input$rcm.sc.period.pu,{
    if (input$rcm.sc.period.pu != 'Present (1981-2010)')
      updateSelectInput(session = session, inputId = "rcm.sc.stat.pu", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation'))
    else 
      updateSelectInput(session = session, inputId = "rcm.sc.stat.pu", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation','Spatial Correlation')) 
    
  })
  
  observeEvent(input$rcm.period.pu,{
    if (input$rcm.period.pu != 'Present (1981-2010)')
      updateSelectInput(session = session, inputId = "rcm.stat.pu", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation'))
    else 
      updateSelectInput(session = session, inputId = "rcm.stat.pu", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation','Spatial Correlation')) 
    
  })
  
  observeEvent(input$rcm.cc.period,{
    if (input$rcm.cc.period != 'Present (1981-2010)')
      updateSelectInput(session = session, inputId = "rcm.cc.stat", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation'))
    else 
      updateSelectInput(session = session, inputId = "rcm.cc.stat", 
                        label = "Statistics",
                        choices = c('Mean','Standard Deviation','Spatial Correlation')) 
    
  })


#-------------------------------------------------------
# O. Räty 09.08.2018  
# Added server side code used in the drought example
# Needs to be harmonized with the rest of the code!
  
  observeEvent(input$showHelp1,{
    showNotification("Choose the drought indicator", action = NULL, duration = NULL, type = "message")
    #    showModal(modalDialog(title = "Test", "Choose the drought indicator", easyClose = T))
    
  })
  
  output$DesignBox2 <- renderText({
    paste(toupper(input$index),", multi-model mean of ",tolower(names(category)[which(category==input$category)]),
          ", ",input$scale,"-month aggregation, ",
          names(statistic)[which(statistic==input$statistic)],sep="")
  })
  
  output$map.spi <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3, maxZoom = 6)) %>%
      addProviderTiles(providers$Esri.WorldStreetMap,
                       #addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addTiles(group = "OSM", layerId = 0) %>%
      setView( lng = 15, lat = 45, zoom = 4 ) %>%
      setMaxBounds(lng1 = -40, lat1 = 0, lng2 = 70, lat2 = 80) %>%
      onRender(
        "function(el,x){
        this.on('mousemove', function(e) {
        var lat = e.latlng.lat;
        var lng = e.latlng.lng;
        var coord = [lat, lng];
        Shiny.onInputChange('hover_coordinates', coord)
        });
        this.on('mouseout', function(e) {
        Shiny.onInputChange('hover_coordinates', null)
        })
  }"
      )
    })
  
  observeEvent(input$map_click,{
    leafletProxy("map.spi") %>%
      clearPopups() %>%
      addPopups(lng = input$hover_coordinates[2], lat = input$hover_coordinates[1], 
                popup = paste("lon = ",round(input$hover_coordinates[2],2),"lat = ",round(input$hover_coordinates[1]),2))
  })
  
  # Generate map layers from the database
  observeEvent(input$submit.spi, {
    path <- "/homeappl/home/oraty/appl_taito/R/DECM/back-end/data/"
    referenceFile <- paste(path,paste(paste(input$index, "statistics", input$scale, "mon", input$category, 
                                 "1981-2010", sep = "_"), "rda",sep="."),sep="")
    env <- reactiveFileReader(intervalMillis = 1000, session = session, filePath = referenceFile, LoadToEnvironment)
    reference <- lapply(env()[[names(env())[1]]], "[[", input$statistic)
    
    scenarioFile <- paste(path,paste(paste(input$index, "statistics", input$scale, "mon", input$category, 
                                "2021-2050", sep = "_"),"rda",sep="."),sep="")
    env <- reactiveFileReader(intervalMillis = 1000, session = session, filePath = scenarioFile, LoadToEnvironment)
    scenario1 <-  lapply(env()[[names(env())[1]]], "[[", input$statistic)
    
    scenarioFile <- paste(path,paste(paste(input$index, "statistics", input$scale, "mon", input$category, 
                                "2071-2100", sep = "_"),"rda",sep="."),sep="")
    env <- reactiveFileReader(intervalMillis = 1000, session = session, filePath = scenarioFile, LoadToEnvironment)
    scenario2 <-  lapply(env()[[names(env())[1]]], "[[", input$statistic)
    
    reactiveData$brickReference <- processToRasterBrick(reference)#,input$statistic)
    reactiveData$brickScenario1 <- processToRasterBrick(scenario1)#,input$statistic)
    reactiveData$brickScenario2 <- processToRasterBrick(scenario2)#,input$statistic)
    
    meanReference <- mean(reactiveData$brickReference)
    meanScenario1 <- mean(reactiveData$brickScenario1)
    meanScenario2 <- mean(reactiveData$brickScenario2)
    
    meanChange1 <- meanScenario1 - meanReference
    meanChange2 <- meanScenario2 - meanReference
    
    reactiveMapVars$palAbs <- getRasterPalette(raster::stack(meanReference,meanScenario1,meanScenario2), type = "bin")
    reactiveMapVars$palChange <- getRasterPalette(raster::brick(meanChange1,meanChange2), type = "bin")
    
    reactiveMapVars$groups <- c("1981-2010", "2021-2050", "2071-2100", sprintf('Change, 1981-2010 \u2192 2021-2050'),
                                sprintf('Change, 1981-2010 \u2192 2071-2100'))
    
    leafletProxy("map.spi") %>%
      clearShapes() %>%
      clearControls() %>%
      clearValues() %>%
      clearImages() %>%
      addRasterImage(meanReference, group = reactiveMapVars$groups[1], layerId = 1, 
                     colors = reactiveMapVars$palAbs$pal, opacity = 0.9) %>%
      addRasterImage(meanScenario1, group = reactiveMapVars$groups[2], layerId = 2,
                     colors = reactiveMapVars$palAbs$pal, opacity = 0.9) %>%
      addRasterImage(meanScenario2, group = reactiveMapVars$groups[3], layerId = 2,
                     colors = reactiveMapVars$palAbs$pal, opacity = 0.9) %>%
      addRasterImage(meanChange1, group = reactiveMapVars$groups[4], layerId = 3,
                     colors = reactiveMapVars$palChange$pal, opacity = 0.9) %>%
      addRasterImage(meanChange2, group = reactiveMapVars$groups[5], layerId = 3,
                     colors = reactiveMapVars$palChange$pal, opacity = 0.9) %>%
      addLegend(position = "bottomleft", pal = reactiveMapVars$palAbs$pal, title = input$statistic, 
                values = reactiveMapVars$palAbs$range, group = reactiveMapVars$groups[2]) %>%
      addLegend(position = "bottomleft", pal = reactiveMapVars$palChange$pal, title = paste("Change in",input$statistic,sep=" "),
                values = reactiveMapVars$palChange$range, group = reactiveMapVars$groups[3]) %>%
      addDrawToolbar(
        targetGroup = "draw",
        circleOptions = F, #disable certain drawing tools for now
        markerOptions = F,
        polylineOptions = F, 
        circleMarkerOptions = F,
        singleFeature = T, #Can draw only single feature at time
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      ) %>%
      addLayersControl(
        baseGroups = reactiveMapVars$groups,
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  # Binding of the legend to the baseGroup not implemented in leaflet yet.
  # Below is a hack, which tries to achieve this (with poor results so far).
  
  # legHelper <- reactive({
  #   if(length(input$map_groups)>1 & all(!is.null(reactiveMapVars$groups))){
  #     return(c(input$map_groups, reactiveMapVars$groups))
  #     print("here")
  #   } else{
  #     return(NULL)
  #     print("there")
  #   }
  # })
  # 
  # observeEvent(legHelper(), ignoreNULL = T, ignoreInit = F, {
  #   print(isolate(input$map.spi_groups))
  #   map <- leafletProxy("map") %>%
  #   clearControls() %>%
  #   clearValues()
  #   thisGroup <- which(input$map_groups != "OSM" & input$map_groups != "draw")
  #   if(input$map.spi_groups[thisGroup] == reactiveMapVars$groups[1]){
  #     map %>% addLegend(position = "bottomleft", pal = reactiveMapVars$palAbs$pal, 
  #                         values = reactiveMapVars$palAbs$range, group = reactiveMapVars$groups[1])
  #   }else if(input$map.spi_groups[thisGroup] == reactiveMapVars$groups[2]){
  #     map %>% addLegend(position = "bottomleft", pal = reactiveMapVars$palAbs$pal, 
  #                         values = reactiveMapVars$palAbs$range, group = reactiveMapVars$groups[2])
  #   }else if(input$map.spi_groups[thisGroup] == reactiveMapVars$groups[3]){
  #     map %>% addLegend(position = "bottomleft", pal = reactiveMapVars$palChange$pal, 
  #                         values = reactiveMapVars$palChange$range, group = reactiveMapVars$groups[3])
  #   }
  # })
  
  #Box plot testing, r
  boxPlotInput <- reactive({
    
    reference <- updateRegion(
      data = reactiveData$brickReference,
      polygon_coordinates = input$map.spi_draw_new_feature$geometry$coordinates[[1]]
    )
    
    scenario1 <- updateRegion(
      data = reactiveData$brickScenario1,
      polygon_coordinates = input$map.spi_draw_new_feature$geometry$coordinates[[1]]
    )
    
    scenario2 <- updateRegion(
      data = reactiveData$brickScenario2,
      polygon_coordinates = input$map.spi_draw_new_feature$geometry$coordinates[[1]]
    )
    
    change1 <- scenario1-reference
    change2 <- scenario2-reference
    set <- c(rep("Reference",length(reference)),rep("Scenario 2021-2050",length(scenario1)), 
             rep("Scenario 2071-2100",length(scenario2)), rep("Change 2021-2050",length(change1)),
             rep("Change 2071-2100",length(change1)))
    output <- data.frame(Type = set, Value = c(reference, scenario1, scenario2, change1, change2))
    output$Type <- factor(output$Type, levels = c("Reference","Scenario 2021-2050","Scenario 2071-2100",
                                                  "Change 2021-2050", "Change 2071-2100"))
    return(output)
  })
  
  output$plotBoxStatistics <- renderPlotly({
    plot_ly(boxPlotInput(), x = ~Type, y = ~Value, type = "box", boxpoints = "all", jitter = 0)
    
  })
  
  
  #Scatter plot input
  scatterPlotInput <- reactive({
    
    refX <- updateRegion(
      data = baseData[["tas"]]$ref,
      polygon_coordinates = input$map.spi_draw_new_feature$geometry$coordinates[[1]]
    )
    
    refY <- updateRegion(
      data = baseData[["pr"]]$ref,
      polygon_coordinates = input$map.spi_draw_new_feature$geometry$coordinates[[1]]
    )
    
    xChange1 <- updateRegion(
      data = baseData[["tas"]]$scen1,
      polygon_coordinates = input$map.spi_draw_new_feature$geometry$coordinates[[1]]
    ) - refX
    
    yChange1 <- 100*(updateRegion(
      data = baseData[["pr"]]$scen1,
      polygon_coordinates = input$map.spi_draw_new_feature$geometry$coordinates[[1]]
    ) - refY)/refY
    
    xChange2 <- updateRegion(
      data = baseData[["tas"]]$scen2,
      polygon_coordinates = input$map.spi_draw_new_feature$geometry$coordinates[[1]]
    ) - refX
    
    yChange2 <- 100*(updateRegion(
      data = baseData[["pr"]]$scen2,
      polygon_coordinates = input$map.spi_draw_new_feature$geometry$coordinates[[1]]
    ) - refY)/refY
    
    output <- list(xChange1 = xChange1, yChange1 = yChange1, xChange2 = xChange2, yChange2 = yChange2)
    
    return(output)
  })
  
  #Scatter plot  
  output$plotScatterStatistics <- renderPlotly({
    indData <- boxPlotInput()
    axisData <- scatterPlotInput()
    
    data1 <- data.frame(data = indData$Value[which(indData$Type=="Change 2021-2050")], x = axisData$xChange1, y = axisData$yChange1)
    data2 <- data.frame(data = indData$Value[which(indData$Type=="Change 2071-2100")], x = axisData$xChange2, y = axisData$yChange2)
    limits1 <- rep(ceiling(max(abs(min(data1$data)),abs(max(data1$data)))))*c(-1,1)
    limits2 <- rep(ceiling(max(abs(min(data1$data)),abs(max(data1$data)))))*c(-1,1)
    symbols <- seq(nrow(data1))
    
    titleOpts1 <- list(title = paste("Change in",input$statistic,",",sprintf('1981-2010 \u2192 2021-2050'),sep=" "),
                       size = 12
    ) 
    xAxisopts1 <- list(title = paste(sprintf('Temperature change (\u2103),  1981-2010 \u2192 2021-2050'), sep = " "),
                       size = 16)
    yAxisopts1 <- list(title = paste("Precipitation change (%),",sprintf('1981-2010 \u2192 2021-2050'), sep = " "),
                       size = 16)
    
    p1 <- plot_ly(data1, x = ~x, y = ~y,
                  type = "scatter",
                  mode = "markers",
                  showlegend = F,
                  marker = list(
                    symbol = symbols,
                    size = 10,
                    color = ~data,
                    reversescale =T,
                    cmin = limits1[1],
                    cmax = limits1[2]
                  ),
                  text = ~paste("Change: ", data)) %>%
      layout(xaxis = xAxisopts1, yaxis = yAxisopts1, title = titleOpts1)
    
    titleOpts2 <- list(title = paste("Change in",input$statistic,",",sprintf('Change, 2071-2100 \u2192 2071-2100'),sep=" "),
                       size = 16
    ) 
    xAxisopts2 <- list(title = paste(sprintf('Temperature change (\u2103), 1981-2010 \u2192 2071-2100'), sep = " "),
                       size = 16)
    yAxisopts2 <- list(title = paste("Precipitation change (%),",sprintf('1981-2010 \u2192 2071-2100'), sep = " "),
                       size = 16)
    p2 <- plot_ly(data2, x = ~x, y = ~y,
                  type = "scatter",
                  mode = "markers",
                  showlegend = F,
                  marker = list(
                    symbol = symbols,
                    size = 10,
                    color = ~data,
                    reversescale =T,
                    cmin = limits2[1],
                    cmax = limits2[2],
                    colorbar = list(
                      len = 1,
                      y = 0.5,
                      ticks = "outside",
                      title = paste("Change in", input$statistic, sep = " "),
                      titlefont = list(
                        size = 16
                      ),
                      titleside = "top"
                    )
                  ),
                  text = ~paste("Change: ", data)) %>%
      layout(xaxis = xAxisopts2, yaxis = yAxisopts2, title = titleOpts2) 
    
    subplot(p1,p2, margin = 0.04, titleY = T, titleX = T)    

  })
  
}






