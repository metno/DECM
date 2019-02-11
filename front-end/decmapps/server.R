# define the back-end
library(shinyBS)

function(input, output,session) {
  
  ## Messages / Notifications / Tasks
  
  addTooltip(session = session,id = 'gcm.period.pu',title = 'this is a test')
  
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
  data(package='DECM', "metaextract")
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
  
  observe(
    showNotification(
      tags$div(tags$p(tags$h1("Please wait ... loading ..."))),
      action = NULL, duration = 5, closeButton = FALSE,id = NULL, type = c("warning"),session = getDefaultReactiveDomain())
  )
  
  observe({
    output$browser <- DT::renderDataTable({
      #
      x <- filter.sim()
      dat <- x %>% mutate(URL = sprintf(paste0("<a href='", URL,"' target='_blank'>Get data</a>")))
      DT::datatable(dat,escape = FALSE)#,selection = 'single',options = list(), style="bootstrap")
    })
    addPopover(session = session, id = "browser", title = "Climate Model Simulations", 
               content = 'Climate model simulations are the result of running a global climate model, a regional climate model, or a combination of the two, all constitute suitable tools are suitable tools (computer programs) to simulate climate variables such as temperature, wind, precipitation, humidity, radiation, … based on the laws of physics and physical processes from the past to decades ahead.',
               placement = 'top',options = list(container = 'body'))
  })
  
  output$glossary <- DT::renderDataTable({
    DT::datatable(
      rbind(c('Project','Name of the project that runs the simulations'),
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
  
  addPopover(session = session, id = "glossary", title = "Data", content = 'this is a test',trigger = 'click')
  
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
  gcm.sc.vals <- function(param,region,period,stat) {
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
    return(rcm.sc.vals(param = 'tas',region = input$rcm.region,  period = 'Present (1981-2010)',stat = input$rcm.stat.pu))
  })
  
  rcm.sc.pr.present <- reactive({
    return(rcm.sc.vals(param = 'pr',region = input$rcm.region,  period = 'Present (1981-2010)',stat = input$rcm.stat))
  })
  
  rcm.sc.pr.present.pu <- reactive({
    return(rcm.sc.vals(param = 'pr',region = input$rcm.region,  period = 'Present (1981-2010)',stat = input$rcm.stat.pu))
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
                             fill = TRUE, color = I("#b21c1c"),weight = 3,label = 'Europe',stroke = TRUE, opacity = 0.5,noClip = TRUE) 
    else if (region == 'BI - British Isles')
      m <- m %>% addPolygons(lng = c(-10,2,2,-10,-10),
                             lat = c(50,50,59,59,50),  
                             fill = TRUE, color = I("#b21c1c"),weight = 3,label = 'BI - British Isles',stroke = TRUE, opacity = 0.5,noClip = TRUE) 
    else if (region == 'IP - Iberian Peninsula')
      m <- m %>% addPolygons(lng = c(-10,3,3,-10,-10), 
                             lat = c(36,36,44,44,36), 
                             fill = TRUE, color = I("#b21c1c"),weight = 3,label = 'IP - Iberian Peninsula',stroke = TRUE, opacity = 0.5,noClip = TRUE) 
    else if (region == 'FR - FRANCE')
      m <- m %>% addPolygons(lng = c(-5,5,5,-5,-5), 
                             lat = c(44,44,50,50,44), 
                             fill = TRUE, color = I("#b21c1c"),weight = 3,label = 'FR - FRANCE',stroke = TRUE, opacity = 0.5,noClip = TRUE) 
    else if (region == 'ME - Mid Europe')
      m <- m %>% addPolygons(lng = c(2,16,16,2,2), 
                             lat = c(48,48,55,55,48), 
                             fill = TRUE, color = I("#b21c1c"),weight = 3,label = 'ME - Mid Europe',stroke = TRUE, opacity = 0.5,noClip = TRUE) 
    else if (region == 'SC - Scandinavia')
      m <- m %>% addPolygons(lng = c(5,30,30,5,5), 
                             lat = c(55,55,70,70,55), 
                             fill = TRUE, color = I("#b21c1c"),weight = 3,label = 'SC - Scandinavia',stroke = TRUE, opacity = 0.5,noClip = TRUE) 
    else if (region == 'AL - ALPS')
      m <- m %>% addPolygons(lng = c(5,15,15,5,5), 
                             lat = c(44,44,48,48,44), 
                             fill = TRUE, color = I("#b21c1c"),weight = 3,label = 'AL - ALPS',stroke = TRUE, opacity = 0.5,noClip = TRUE) 
    else if (region == 'MD - Mediterranean')
      m <- m %>% addPolygons(lng = c(3,25,25,3,3), 
                             lat = c(36,36,44,44,36), 
                             fill = TRUE, color = I("#b21c1c"),weight = 3,label = 'MD - Mediterranean',stroke = TRUE, opacity = 0.5,noClip = TRUE) 
    else if (region == 'EA - Eastern Europe')
      m <- m %>% addPolygons(lng = c(16,30,30,16,16), 
                             lat = c(44,44,54,54,44), 
                             fill = TRUE, color = I("#b21c1c"),weight = 3,label = 'EA - Eastern Europe',stroke = TRUE, opacity = 0.5,noClip = TRUE)
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
      if (region != 'NOR - Norway')      
        selected_polygon <- intersect(selected_polygon,SpP)
      polygon_labelPt <- selected_polygon@polygons[[1]]@labpt
      #center the view on the polygon 
      m <- m %>% setView(lng=polygon_labelPt[1],lat=polygon_labelPt[2],zoom=6)
      
      m <- m %>% addPolylines(fill = TRUE, stroke = TRUE, color = I("#b21c1c"),weight = 1.5, opacity = 0.5,
                              data=selected_polygon,noClip = TRUE,
                              group="highlighted_polygon") 
      
    } 
    
    invisible(m)
  }
  
  ## Sectoral communication example 
  spi <- function(freq=1,group='ED',stat='nEvents',period='1981-2010') {
    
    spi.file <- paste('data/spei_statistics_',freq,'_mon_',group,'_',period,'.rda',sep='')
    load(spi.file)
    #browser()
    spi <- array(NA,dim=c(length(droughtStatistics), dim(droughtStatistics[[1]][[stat]])))
    for (i in 1:length(droughtStatistics))
      spi[i,,] <- droughtStatistics[[i]][[stat]]
    attr(spi,'longitude') <- attr(droughtStatistics[[1]][[1]],'dimnames')[[1]]
    attr(spi,'latitude') <- attr(droughtStatistics[[1]][[1]],'dimnames')[[2]]
    invisible(spi)
  }
  
  spi.reactive <- reactive({
    invisible(spi(input$spi.freq,substr(input$spi.group,1,2),input$spi.stat,substr(input$spi.period,1,9)))
  })
  
  
  observe(priority = 0, { # 
    
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
                               fill = TRUE, stroke = TRUE, color = I("#b21c1c"),weight = 3,label = 'Global') 
      } else {
        m <- m %>% addPolygons(lng = srex$coords[[id-1]][1,], lat = srex$coords[[id-1]][2,], 
                               fill = TRUE, stroke = TRUE, color = I("#b21c1c"),weight = 3,label = srex$name[id-1]) 
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
                               fill = TRUE, stroke = TRUE, color = I("#b21c1c"),weight = 3,label = 'Global') 
      } else {
        m <- m %>% addPolygons(lng = srex$coords[[id-1]][1,], lat = srex$coords[[id-1]][2,], 
                               fill = TRUE, stroke = TRUE, color = I("#b21c1c"),weight = 3,label = srex$name[id-1]) 
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
                               fill = TRUE, stroke = TRUE, color = I("#b21c1c"),weight = 3,label = 'Global') 
      } else {
        m <- m %>% addPolygons(lng = srex$coords[[id-1]][1,], lat = srex$coords[[id-1]][2,], 
                               fill = TRUE, stroke = TRUE, color = I("#b21c1c"),weight = 3,label = srex$name[id-1]) 
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
                               fill = TRUE, stroke = TRUE, color = I("#b21c1c"),weight = 3,label = 'Global') 
      } else {
        m <- m %>% addPolygons(lng = srex$coords[[id-1]][1,], lat = srex$coords[[id-1]][2,], 
                               fill = TRUE, stroke = TRUE, color = I("#b21c1c"),weight = 3,label = srex$name[id-1]) 
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
            leg.name <- paste(lev[id[i]],paste(as.character(as.matrix(gcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '))
            leg.name.abb <- paste(i)
            grp.name <- paste('Group',id[i],sep='')
            
            #if (is.null(input$rowsGcm)) {
            if (is.element(input$gcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name, 
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter',
                                      name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
            #}
            ## Highlight selected models in tab:models
          } 
        } else {
          im <- input$rowsGcm
          for (i in 1:length(im)) {
            
            leg.name <- paste(im[i],paste(as.character(as.matrix(gcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '))
            leg.name.abb <- paste(im[i])
            grp.name <- paste('Group',id[im[i]],sep='')
            gcm <- gcms[i] #gcms[im[i]]
            if (is.element(input$gcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name.abb, mode = 'lines', legendgroup = grp.name, 
                                      colors = ",i,", hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name.abb, mode = 'lines', legendgroup = grp.name,
                                      colors = colsa[im[",i,"]], hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = colsa[im[",i,"]], width = 2, shape ='spline'))",sep='')))
          }
        }
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.chart.type))) { # Make an enveloppe instead of lines
        
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines', 
                    fill = 'tonexty', fillcolor='rgba(255,145,145,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',name = 'Ens. Mean',
                    line = list(color='#b21c1c'), showlegend = TRUE,
                    name = 'Average') 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', showlegend = TRUE,
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
        
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
        ylab <- "Bias in temperature [deg. C]"
      else if (input$gcm.outputValues == 'Anomaly')
        ylab <- "Temperature anomalies [deg. C]"
      else if (input$gcm.outputValues == 'Change')
        ylab <- "Change in temperature [deg. C]"
      else 
        ylab <- "Simulated regional temperature [deg. C]"
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$gcm.region),
                                showarrow=F,
                                font=list(size=16,weight='bold')
                              )
      )
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
          
          leg.name <- paste(lev[id[i]],paste(as.character(as.matrix(gcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '))
          leg.name.abb <- paste(i)
          grp.name <- paste('Group',id[i],sep='')
          
          #if (is.null(input$rowsGcm)) {
          if (is.element(input$gcm.colorBy, c('None','---')))
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                    name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                    showlegend = TRUE, legendgroup = grp.name, 
                                    line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
          else
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter',
                                    name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                    showlegend = TRUE, legendgroup = grp.name,
                                    line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          #}
          ## Highlight selected models in tab:models
        } 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.chart.type.pu))) { # Make an enveloppe instead of lines
        
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines', 
                    fill = 'tonexty', fillcolor='rgba(255,145,145,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',name = 'Ens. Mean',
                    line = list(color='#b21c1c'), showlegend = TRUE,
                    name = 'Average') 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', showlegend = TRUE,
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
        
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
      
      ylab <- "Bias in temperature [deg. C]"
      
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$gcm.region.pu),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
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
          leg.name.abb <- paste(lev[id[i]])
          leg.name <- paste(lev[id[i]],paste(as.character(as.matrix(gcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '))
          grp.name <- paste('Group',id[i],sep='')
          
          
          #if (is.null(input$rowsGcm)) {
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter',
                                  name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          #}
          ## Highlight selected models in tab:models
        }
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.sc.chart.type.pu))) { # Make an enveloppe instead of lines
        
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines', 
                    fill = 'tonexty', fillcolor='rgba(255,145,145,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',name = 'Ens. Mean',
                    line = list(color='#b21c1c'), showlegend = TRUE,
                    name = 'Average') 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', showlegend = TRUE,
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5, y = -0.2))
        
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
      
      ylab <- "Temperature [deg. C]"
      
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE), 
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste(input$gcm.sc.region.pu, input$gcm.sc.period.pu,
                                           input$gcm.sc.chart.type.pu,
                                           input$gcm.sc.stat.pu,sep=' | '),
                                showarrow=F,
                                font=list(size=14,color="grey"))
      )
      
      
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
          leg.name.abb <- paste(lev[id[i]])
          leg.name <- paste(lev[id[i]], paste(as.character(as.matrix(gcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '))
          grp.name <- paste('Group',id[i],sep='')
          
          #if (is.null(input$rowsGcm)) {
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter',
                                  name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          #}
          ## Highlight selected models in tab:models
        }
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.cc.chart.type))) { # Make an enveloppe instead of lines
        
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines', 
                    fill = 'tonexty', fillcolor='rgba(255,145,145,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',name = 'Ens. Mean',
                    line = list(color='#b21c1c'), showlegend = TRUE,
                    name = 'Average') 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', showlegend = TRUE,
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x = 0.5, y = -0.2))
        
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
      
      ylab <- "Changes in temperature [deg. C]"
      
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region |", input$gcm.cc.region),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
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
        ## Add all Simulations
        if (is.null(input$rowsGcm)) {
          for (gcm in gcms) {
            i <- which(is.element(gcms,gcm))
            #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
            leg.name.abb <- paste(lev[id[i]])
            leg.name <- paste(lev[id[i]],paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '))
            grp.name <- paste('Group',id[i],sep='')
            
            if (is.element(input$gcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
            
          }
          ## Highlight selected Simulations in tab:models
        } else {
          im <- input$rowsGcm
          for (i in 1:length(im)) {
            leg.name.abb <- paste(im[i])
            leg.name <- paste(im[i],paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '))
            grp.name <- paste('Group',id[i],sep='')
            gcm <- gcms[i]
            if (is.element(input$gcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name.abb, mode = 'lines', legendgroup = grp.name,
                                      colors = ",i,",hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                      name = leg.name.abb, mode = 'lines', legendgroup = grp.name,
                                      colors = colsa[im[",i,"]],hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = colsa[im[",i,"]], width = 2, shape ='spline'))",sep='')))
          }            
          
        }
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.chart.type))) { # Make an enveloppe instead of lines
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),name = 'High',showlegend = TRUE) %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',showlegend = TRUE,
                    fill = 'tonexty', fillcolor='rgba(255,145,145,0.2)', line = list(color = 'transparent'),name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',line = list(color='#b21c1c'),
                    name = 'Ens. Mean',showlegend = TRUE) 
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
        
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
        ylab <- "Bias in precipitation [%]"
      else if (input$gcm.outputValues == 'Anomaly') 
        ylab <- "Precipitation anomalies [mm]"
      else if (input$gcm.outputValues == 'Change')
        ylab <- "Change in precipitation [%]"
      else 
        ylab <- "Simulated regional precipitation [mm]"
      # Format layout 
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region |", input$gcm.region),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
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
        ## Add all Simulations
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(lev[id[i]],paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '))
          leg.name.abb <- paste(lev[id[i]])
          
          grp.name <- paste('Group',id[i],sep='')
          
          if (is.element(input$gcm.colorBy, c('None','---')))
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                    name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                    showlegend = TRUE, legendgroup = grp.name,
                                    line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
          else
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                    name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                    showlegend = TRUE, legendgroup = grp.name,
                                    line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          
        }
        ## Highlight selected Simulations in tab:models
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.chart.type))) { # Make an enveloppe instead of lines
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),name = 'High',showlegend = TRUE) %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',showlegend = TRUE,
                    fill = 'tonexty', fillcolor='rgba(135,206,250,0.2)', line = list(color = 'transparent'),name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',line = list(color='rgb(35, 132, 170)'),
                    name = 'Ens. Mean',showlegend = TRUE) 
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
        
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
      
      
      ylab <- "Bias in precipitation [%]"
      
      # Format layout 
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region |", input$gcm.region.pu),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
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
        ## Add all Simulations
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(lev[id[i]],paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '))
          leg.name.abb <- paste(lev[id[i]])
          grp.name <- paste('Group',id[i],sep='')
          
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                  name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          
        }
        ## Highlight selected Simulations in tab:models
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.sc.chart.type.pu))) { # Make an enveloppe instead of lines
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),name = 'High',showlegend = TRUE) %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',showlegend = TRUE,
                    fill = 'tonexty', fillcolor='rgba(135,206,250,0.2)', line = list(color = 'transparent'),name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',line = list(color='rgb(35, 132, 170)'),
                    name = 'Ens. Mean',showlegend = TRUE) 
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
        
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
      
      
      ylab <- "Precipitation [mm/month]"
      
      # Format layout 
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$gcm.sc.region.pu),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
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
        ## Add all Simulations
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(lev[id[i]],paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '))
          leg.name.abb <- paste(lev[id[i]])
          grp.name <- paste('Group',id[i],sep='')
          
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",gcm,",type = 'scatter', 
                                  name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = colsa[",i,"], width = 2, shape ='spline'))",sep='')))
          
        }
        ## Highlight selected Simulations in tab:models
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$gcm.cc.chart.type))) { # Make an enveloppe instead of lines
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),name = 'High',showlegend = TRUE) %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',showlegend = TRUE,
                    fill = 'tonexty', fillcolor='rgba(135,206,250,0.2)', line = list(color = 'transparent'),name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',line = list(color='rgb(35, 132, 170)'),
                    name = 'Ens. Mean',showlegend = TRUE) 
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'ERAINT', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
        
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
      
      
      ylab <- "Future changes in simulated precipitation [mm/month]"
      
      # Format layout 
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region |", input$gcm.cc.region),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
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
        
        caption <- paste('Monthly estimates of regional preciptiation assuming an 
                         intermediate emission scenarios for the',tolower(input$gcm.period),'averaged over',input$gcm.region,'region.
                         The climate models and their corresponding runs are listed in the second and third columns, respectively. 
                         The last row in the table shows the estimated values from the referance data set (Observation).',
                         sep= ' ')
      else if (input$gcm.outputValues == 'Anomaly') {
        df <- df - colMeans(df)
        caption <- paste('Monthly anomalies in estimated regional preciptiation assuming an 
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
      
      if (length(df$dpr) > 1 | length(df$dtas) > 1) {
        
        dfe.70 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.6827,draw = FALSE)
        dfe.95 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9545,draw = FALSE)
        dfe.99 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9973,draw = FALSE)
        p.sc <- p.sc %>% 
          add_polygons(x = dfe.99[,1],y = dfe.99[,2] , name = '99% CL',
                       fillcolor = 'rgba(255,127,80,0.5)',
                       line = list(color = 'rgba(255,127,80,0.6)'),
                       opacity = 0.5) %>%
          add_polygons(x = dfe.95[,1],y = dfe.95[,2] , name = '95% CL',
                       fillcolor = 'rgba(255,127,80,0.6)',
                       line = list(color = 'rgba(255,127,80,0.7)'),
                       opacity = 0.7) %>% 
          add_polygons(x = dfe.70[,1],y = dfe.70[,2] , name = '70% CL',
                       fillcolor = 'rgba(255,127,80,0.7)',
                       line = list(color = 'rgba(255,127,80,0.8)'),
                       opacity = 0.8) 
      }
      
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
                                      marker = list(color = ",i,", symbol = 3 ,size = 12,opacity = 0.7,line = list(width = 1,color = '#FFFFFF')))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = cols[",i,"], symbol = 3,size = 12,opacity=0.7,line = list(width = 1,color = '#FFFFFF')))",sep='')))
            
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
      
      if ((input$gcm.chart.type == 'Ensemble of All Simulations') | (input$gcm.chart.type == "Both - Ensemble & Individual Simulations"))
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
      #}
      
      if (input$gcm.legend.sc == 'Display') {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = TRUE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'REF', hoverinfo = 'text+x+y',text='ERAINT',showlegend = TRUE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      } else {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = FALSE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'REF', hoverinfo = 'text+x+y',text='ERAINT',showlegend = FALSE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      }
      if (input$gcm.outputValues == 'Bias') {
        ylab <- 'Bias in temperature [deg. C]'
        xlab <- 'Bias in precitation [%]'
      } else if (input$gcm.outputValues == 'Bias') {
        ylab <- 'RMSE in temperature [deg. C]'
        xlab <- 'RMSE in precitation [%]'
      } else if (input$gcm.outputValues == 'Anomaly') {
        ylab <- 'Temperature anomalies [deg. C]'
        xlab <- 'Precitation anomalies [%]'
      } else if (input$gcm.outputValues == 'Change') {
        ylab <- 'Change in temperature [deg. C]'
        xlab <- 'Change in precitation [%]'
      } else  {
        ylab <- 'Temperature [deg. C]'
        xlab <- 'Precitation [mm/month]'
      }
      
      
      
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = TRUE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region |", input$gcm.region),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
      if (input$gcm.legend.sc == 'Hide') {
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      } else {
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      }
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
      
      df <- data.frame(dtas = as.numeric(round(colMeans(dtas),digits = 2)),
                       dpr = as.numeric(round(colMeans(dpr),digits = 2)),
                       gcm.name = gcmall, inst.name = gcm.inst, stringsAsFactors = FALSE)
      
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
      
      ### Add ellipsoid
      if (length(df$dpr) > 1 | length(df$dtas) > 1) {
        
        dfe.70 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.6827,draw = FALSE)
        dfe.95 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9545,draw = FALSE)
        dfe.99 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9973,draw = FALSE)
        p.sc <- p.sc %>% 
          add_polygons(x = dfe.99[,1],y = dfe.99[,2] , name = '99% CL',
                       fillcolor = 'rgba(255,127,80,0.5)',
                       line = list(color = 'rgba(255,127,80,0.6)'),
                       opacity = 0.5) %>%
          add_polygons(x = dfe.95[,1],y = dfe.95[,2] , name = '95% CL',
                       fillcolor = 'rgba(255,127,80,0.6)',
                       line = list(color = 'rgba(255,127,80,0.7)'),
                       opacity = 0.7) %>% 
          add_polygons(x = dfe.70[,1],y = dfe.70[,2] , name = '70% CL',
                       fillcolor = 'rgba(255,127,80,0.7)',
                       line = list(color = 'rgba(255,127,80,0.8)'),
                       opacity = 0.8) 
      }
      
      if ((input$gcm.cc.chart.type == 'Individual Simulations')) {
        ## Add all Simulations
        for (gcm in gcms) {
          i <- which(is.element(gcms,gcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(lev[id[i]],paste(as.character(as.matrix(gcm.meta.pr[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = ' '))
          leg.name.abb <- lev[id[i]]
          grp.name <- paste('Group',id[i],sep='')
          
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                  name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  marker = list(color = ",i,",symbol = 3, size = 12,opacity = 0.7,line = list(width = 1,color = '#FFFFFF')))",sep='')))
        }
        ## Highlight selected Simulations in tab:models
      } 
      
      
      if ((input$gcm.cc.chart.type == 'Ensemble of All Simulations') | (input$gcm.cc.chart.type == "Both - Ensemble & Individual Simulations"))
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
      #}
      
      if (input$gcm.legend.sc == 'Display') {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = TRUE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'REF', hoverinfo = 'text+x+y',text='ERAINT',showlegend = TRUE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      } else {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = FALSE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'REF', hoverinfo = 'text+x+y',text='ERAINT',showlegend = FALSE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      }
      ylab <- 'Change in temperature [deg. C]'
      xlab <- 'Change in precitation [%]'
      
      
      
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = TRUE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$gcm.cc.region),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
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
            
            leg.name <- paste(lev[id[i]],paste(as.character(as.matrix(rcm.meta.tas[i,c('gcm','gcm_rip','rcm')])),collapse = ' '))
            leg.name.abb <- paste(i)
            grp.name <- paste('Group',id[i],sep='')
            
            #if (is.null(input$rowsRcm)) {
            if (is.element(input$rcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name =  leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name, 
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter',
                                      name =  leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
            #}
            ## Highlight selected models in tab:models
          } 
        } else {
          im <- input$rowsRcm
          for (i in 1:length(im)) {
            leg.name <- paste(i,paste(as.character(as.matrix(rcm.meta.tas[i,c('gcm','gcm_rip','rcm')])),collapse = '  '))
            leg.name.abb <- paste(i)
            grp.name <- paste('Group',id[im[i]],sep='')
            rcm <- rcms[i] #rcms[im[i]]
            if (is.element(input$rcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = leg.name.abb, mode = 'lines', legendgroup = grp.name, 
                                      colors = ",i,", hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = ",i,", width = 1.5, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = leg.name.abb, mode = 'lines', legendgroup = grp.name,
                                      colors = colsa[im[",i,"]], hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = colsa[im[",i,"]], width = 1.5, shape ='spline'))",sep='')))
          }
        }
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'REF', text = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.chart.type))) { # Make an enveloppe instead of lines
        
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = FALSE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',
                    fill = 'tonexty', fillcolor='rgba(255,145,145,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',
                    line = list(color='#b21c1c'),
                    name = 'Average') 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
        
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
        ylab <- "Bias in temperature [deg. C]"
      else if (input$rcm.outputValues == 'Anomaly')
        ylab <- "Temperature anomalies [deg. C]"
      else if (input$rcm.outputValues == 'Change')
        ylab <- "Change in temperature [deg. C]"
      else 
        ylab <- "Temperature [deg. C]"
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$rcm.region),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
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
          leg.name <- paste(paste(as.character(as.matrix(rcm.meta.tas[i,c('gcm','gcm_rip','rcm')])),collapse = ' '))          
          leg.name.abb <- paste(i)
          grp.name <- paste('Group',id[i],sep='')
          
          #if (is.null(input$rowsRcm)) {
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter',
                                  name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          #}
          ## Highlight selected models in tab:models
        } 
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'REF', text = 'EOBS', text ='REF',mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.chart.type.pu))) { # Make an enveloppe instead of lines
        
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',
                    fill = 'tonexty', fillcolor='rgba(255,145,145,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',
                    line = list(color='#b21c1c'),
                    name = 'Average') 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'EOBS',mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
        
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
      
      
      ylab <- "Bias in temperature [deg. C]"
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$rcm.region.pu),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      if (input$rcm.chart.type.pu != 'Individual Simulations')
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5))
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
          leg.name <- paste(as.character(i),paste(as.character(as.matrix(rcm.meta.tas[i,c('gcm','gcm_rip','rcm')])),collapse = ' '))
          leg.name.abb <- paste(as.character(i))
          grp.name <- paste('Group',id[i],sep='')
          
          #if (is.null(input$rowsRcm)) {
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter',
                                  name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          #}
          ## Highlight selected models in tab:models
        } 
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'REF', text = 'EOBS',mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.sc.chart.type.pu))) { # Make an enveloppe instead of lines
        
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',
                    fill = 'tonexty', fillcolor='rgba(255,145,145,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',
                    line = list(color='#b21c1c'),
                    name = 'Average') 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
        
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
      
      
      ylab <- "Temperature [deg. C]"
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region: ", input$rcm.sc.region.pu),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
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
          leg.name <- paste(i, paste(as.character(as.matrix(rcm.meta.tas[i,c('gcm','gcm_rip','rcm')])),collapse = ' '))
          leg.name.abb <- paste(i)
          grp.name <- paste('Group',id[i],sep='')
          
          
          #if (is.null(input$rowsRcm)) {
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter',
                                  name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          #}
          ## Highlight selected models in tab:models
        } 
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref, type = 'scatter', name = 'REF', text = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.cc.chart.type))) { # Make an enveloppe instead of lines
        
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = FALSE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',
                    fill = 'tonexty', fillcolor='rgba(255,145,145,0.2)', line = list(color = 'transparent'),
                    showlegend = FALSE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',
                    line = list(color='#b21c1c'),
                    name = 'Average') 
        
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'EOBS',mode = 'lines', 
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
      
      
      ylab <- "Temperature [deg. C]"
      p.sc <- p.sc %>% layout(title = FALSE,
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "",
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$rcm.cc.region),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      if (input$rcm.sc.chart.type.pu != 'Individual Simulations')
        
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
            leg.name <- paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','rcm')])),collapse = ' ')
            leg.name.abb <- paste(i)
            grp.name <- paste('Group',id[i],sep='') #leg.name
            
            if (is.element(input$rcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = leg.name.abb , mode = 'lines', legendgroup = grp.name,
                                      colors = ",i,",hoverinfo = 'text+x+y',text=leg.name, legendgroup = grp.name,
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = leg.name.abb, mode = 'lines', legendgroup = grp.name,
                                      colors = colsa[",i,"],hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
            
          }
          ## Highlight selected Simulations in tab:models
        } else {
          im <- input$rowsRcm
          for (i in 1:length(im)) {
            leg.name <- paste(im[i],paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','rcm')])),collapse = ' '),sep =' ')
            leg.name.abb <- paste(im[i]) 
            grp.name <- paste('Group',id[i],sep='')
            rcm <- rcms[i]
            if (is.element(input$rcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = leg.name.abb , mode = 'lines', legendgroup = grp.name,
                                      colors = ",i,",hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                      name = leg.name.abb , mode = 'lines', legendgroup = grp.name,
                                      colors = colsa[im[",i,"]],hoverinfo = 'text+x+y',text=leg.name,
                                      line = list(color = colsa[im[",i,"]], width = 2, shape ='spline'))",sep='')))
          }            
          
        }
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.chart.type))) { # Make an enveloppe instead of lines
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',
                    fill = 'tonexty', fillcolor='rgba(135,206,250,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',
                    line = list(color='rgb(35, 132, 170)'),
                    name = 'Average') 
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))          
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
        ylab <- "Bias in precipitation [%]"
      else if (input$rcm.outputValues == 'Anomaly') 
        ylab <- "Precipitation anomalies [mm]"
      else if (input$rcm.outputValues == 'Change')
        ylab <- "Change in precipitation [%]"
      else 
        ylab <- "Precipitation [mm/month]"
      # Format layout 
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$rcm.region.pu),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
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
          leg.name <- paste(i,paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','rcm')])),collapse = ' '))
          leg.name.abb <- paste(i) 
          grp.name <- paste('Group',id[i],sep='') #leg.name
          
          if (is.element(input$rcm.colorBy, c('None','---')))
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                    name = leg.name.abb, mode = 'lines', legendgroup = grp.name,
                                    colors = ",i,",hoverinfo = 'text+x+y',text=leg.name, legendgroup = grp.name,
                                    line = list(color = ",i,", width = 2, shape ='spline'))",sep='')))
          else
            eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                    name = leg.name.abb, mode = 'lines', legendgroup = grp.name,
                                    colors = colsa[",i,"],hoverinfo = 'text+x+y',text=leg.name,
                                    line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          
        }
        ## Highlight selected Simulations in tab:models
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.chart.type.pu))) { # Make an enveloppe instead of lines
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',
                    fill = 'tonexty', fillcolor='rgba(135,206,250,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',
                    line = list(color='rgb(35, 132, 170)'),
                    name = 'Average') 
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'EOBS', mode = 'lines', 
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
      
      
      ylab <- "Bias in precipitation [%]"
      # Format layout 
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$rcm.region.pu),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
      if (grepl('ensemble',tolower(input$rcm.chart.type.pu)))
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
      
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
          leg.name <- paste(i,paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','rcm')])),collapse = ' '))
          leg.name.abb <- paste(i) 
          grp.name <- paste('Group',id[i],sep='') #leg.name
          
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                  name = leg.name.abb, mode = 'lines', legendgroup = grp.name,
                                  colors = colsa[",i,"],hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          
        }
        ## Highlight selected Simulations in tab:models
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.sc.chart.type.pu))) { # Make an enveloppe instead of lines
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = TRUE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',
                    fill = 'tonexty', fillcolor='rgba(135,206,250,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',
                    line = list(color='rgb(35, 132, 170)'),
                    name = 'Average') 
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
        
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
      
      ylab <- "Precipitation [mm/month]"
      # Format layout 
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$rcm.sc.region.pu),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
      if (input$rcm.sc.chart.type.pu != 'Individual Simulations')
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
      
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
          leg.name <- paste(i,paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','rcm')])),collapse = ' '),sep =' ')
          leg.name.abb <- paste(i)
          grp.name <- paste('Group',id[i],sep='') #leg.name
          
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(y = ~ ",rcm,",type = 'scatter', 
                                  name = leg.name.abb , mode = 'lines', legendgroup = grp.name,
                                  colors = colsa[",i,"],hoverinfo = 'text+x+y',text=leg.name,
                                  line = list(color = cols[",i,"], width = 2, shape ='spline'))",sep='')))
          
        }
        ## Highlight selected Simulations in tab:models
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'REF', text = 'EOBS', mode = 'lines', 
                                     line = list(color = 'black', width = 2, dash = 'dash', shape ='spline'))
        
      } else if (grepl('ensemble', tolower(input$rcm.cc.chart.type))) { # Make an enveloppe instead of lines
        p.sc <- plot_ly(df.env, x = ~month, y = ~high, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = FALSE, name = 'High') %>%
          add_trace(y = ~low, type = 'scatter', mode = 'lines',
                    fill = 'tonexty', fillcolor='rgba(135,206,250,0.2)', line = list(color = 'transparent'),
                    showlegend = FALSE, name = 'Low') %>%
          add_trace(x = ~month, y = ~avg, type = 'scatter', mode = 'lines',
                    line = list(color='rgb(35, 132, 170)'),
                    name = 'Average') 
        
        if (!is.null(df$ref))
          p.sc <- p.sc %>% add_trace(y = ~ref,type = 'scatter', name = 'ERAINT', text = 'REF', mode = 'lines', 
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
      
      ylab <- "Precipitation [mm/month]"
      # Format layout 
      p.sc <- p.sc %>% layout(title = FALSE,
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "",
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
                                           zeroline = FALSE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$rcm.cc.region),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
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
        rcmi <- rcm.meta.pr[i,c('rcm')]#'model_id')]
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
      
      if (length(df$dpr) > 1 | length(df$dtas) > 1) {
        
        dfe.70 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.6827,draw = FALSE)
        dfe.95 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9545,draw = FALSE)
        dfe.99 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9973,draw = FALSE)
        p.sc <- p.sc %>% 
          add_polygons(x = dfe.99[,1],y = dfe.99[,2] , name = '99% CL',
                       fillcolor = 'rgba(255,127,80,0.5)',
                       line = list(color = 'rgba(255,127,80,0.6)'),
                       opacity = 0.5) %>%
          add_polygons(x = dfe.95[,1],y = dfe.95[,2] , name = '95% CL',
                       fillcolor = 'rgba(255,127,80,0.6)',
                       line = list(color = 'rgba(255,127,80,0.7)'),
                       opacity = 0.7) %>% 
          add_polygons(x = dfe.70[,1],y = dfe.70[,2] , name = '70% CL',
                       fillcolor = 'rgba(255,127,80,0.7)',
                       line = list(color = 'rgba(255,127,80,0.8)'),
                       opacity = 0.8)  
      }
      
      if ((input$rcm.chart.type == 'Individual Simulations')) {
        ## Add all Simulations
        if (is.null(input$rowsRcm)) {
          for (rcm in rcms) {
            i <- which(is.element(rcms,rcm))
            #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
            leg.name <- paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','gcm_rip','rcm')])),collapse = ' ')
            leg.name.abb <- paste(i)
            grp.name <- paste('Group',id[i],sep='')
            
            if (is.element(input$rcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = ",i,", symbol = 3,size = 12,opacity = 0.7,line = list(width = 1,color = '#FFFFFF')))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = cols[",i,"], symbol = 3,size = 12,opacity=0.7,line = list(width = 1,color = '#FFFFFF')))",sep='')))
            
          }
          ## Highlight selected Simulations in tab:models
        } else {
          im <- input$rowsRcm
          for (i in 1:length(im)) {
            leg.name <- paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','gcm_rip','rcm')])),collapse = ' ')
            leg.name.abb <- paste(im[i])
            grp.name <- paste('Group',id[i],sep='')
            rcm <- rcms[i]
            if (is.element(input$rcm.colorBy, c('None','---')))
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = ",i,", symbol = 3,size = 12,opacity = 0.7,line = list(width = 1)))",sep='')))
            else
              eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                      name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                      showlegend = TRUE, legendgroup = grp.name,
                                      marker = list(color = cols[im[",i,"]], symbol = 3,size = 12,opacity=0.7,line = list(width = 1))",sep='')))
          }
        }
      } 
      
      
      
      if ((input$rcm.chart.type == 'Ensemble of All Simulations') | (input$rcm.chart.type == "Both - Ensemble & Individual Simulations"))
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
      #}
      
      if (input$rcm.legend.sc == 'Display') {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = TRUE,
                                   marker = list(color = '#871010', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'REF', hoverinfo = 'text+x+y',text='ERAINT',showlegend = TRUE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      } else {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = FALSE,
                                   marker = list(color = '#871010', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'REF', hoverinfo = 'text+x+y',text='ERAINT',showlegend = FALSE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      }
      if (input$rcm.outputValues == 'Bias') {
        ylab <- 'Bias in temperature [deg. C]'
        xlab <- 'Bias in precitation [%]'
      } else if (input$rcm.outputValues == 'Bias') {
        ylab <- 'RMSE in temperature [deg. C]'
        xlab <- 'RMSE in precitation [%]'
      } else if (input$rcm.outputValues == 'Anomaly') {
        ylab <- 'Temperature anomalies [deg. C]'
        xlab <- 'Precitation anomalies [%]'
      } else if (input$rcm.outputValues == 'Change') {
        ylab <- 'Change in temperature [deg. C]'
        xlab <- 'Change in precitation [%]'
      } else  {
        ylab <- 'Precitation [mm/month]'
        xlab <- 'Temperature [deg. C]'
      }
      
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = TRUE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$rcm.region),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
      if (input$rcm.legend.sc == 'Hide') {
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      } else {
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      }
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
        rcmi <- rcm.meta.pr[i,c('rcm')]#'model_id')]
        rcmi[is.na(rcmi)] <- ''
        return(paste(as.character(as.matrix(rcmi)),collapse = '_'))
      }
      
      rcmall <- c(sapply(1:nrow(rcm.meta.pr),rcm.name),'ERAINT')
      rcm.inst <- c(sapply(1:nrow(rcm.meta.pr),inst.name),'ERAINT')
      df <- data.frame(dtas = as.numeric(round(colMeans(dtas),digits = 2)),
                       dpr = as.numeric(round(colMeans(dpr),digits = 2)),
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
      
      if (length(df$dpr) > 1 | length(df$dtas) > 1) {
        
        dfe.70 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.6827,draw = FALSE)
        dfe.95 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9545,draw = FALSE)
        dfe.99 <- dataEllipse(x = as.matrix(df[,c('dtas','dpr')]),levels = 0.9973,draw = FALSE)
        p.sc <- p.sc %>% 
          add_polygons(x = dfe.99[,1],y = dfe.99[,2] , name = '99% CL',
                       fillcolor = 'rgba(255,127,80,0.5)',
                       line = list(color = 'rgba(255,127,80,0.6)'),
                       opacity = 0.5) %>%
          add_polygons(x = dfe.95[,1],y = dfe.95[,2] , name = '95% CL',
                       fillcolor = 'rgba(255,127,80,0.6)',
                       line = list(color = 'rgba(255,127,80,0.7)'),
                       opacity = 0.7) %>% 
          add_polygons(x = dfe.70[,1],y = dfe.70[,2] , name = '70% CL',
                       fillcolor = 'rgba(255,127,80,0.7)',
                       line = list(color = 'rgba(255,127,80,0.8)'),
                       opacity = 0.8)  
      }
      
      if ((input$rcm.cc.chart.type == 'Individual Simulations')) {
        ## Add all Simulations
        for (rcm in rcms) {
          i <- which(is.element(rcms,rcm))
          #leg.name <- paste(as.character(as.matrix(rcm.meta.tas[i,c('institute_id','model_id','parent_experiment_rip','realization')])),collapse = '  ')
          leg.name <- paste(as.character(as.matrix(rcm.meta.pr[i,c('gcm','gcm_rip','rcm')])),collapse = ' ')
          leg.name.abb <- paste(i)
          grp.name <- paste('Group',id[i],sep='')
          
          eval(parse(text = paste("p.sc <- p.sc %>% add_trace(x = ~",df$dtas[i],",y = ~ ",df$dpr[i],",type = 'scatter',mode = 'markers',
                                  name = leg.name.abb, mode = 'lines', hoverinfo = 'text+x+y',text=leg.name,
                                  showlegend = TRUE, legendgroup = grp.name,
                                  marker = list(color = ",i,", symbol = 3,size = 12,opacity=0.7,line = list(width = 2,color = '#FFFFFF')))",sep='')))
          
        }
      } 
      
      
      
      if ((input$rcm.cc.chart.type == 'Ensemble of All Simulations') | (input$rcm.cc.chart.type == "Both - Ensemble & Individual Simulations"))
        p.sc <- p.sc %>% layout(legend = list(orientation = "h",xanchor = "center",x =0.5,y=-0.2))
      #}
      
      if (input$rcm.legend.sc == 'Display') {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = TRUE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'REF', hoverinfo = 'text+x+y',text='ERAINT',showlegend = TRUE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      } else {
        p.sc <- p.sc %>% add_trace(x = ~mean(df$dtas[-length(df$dpr)]),y = ~ mean(df$dpr[-length(df$dpr)]),type = 'scatter',mode = 'markers',
                                   name = 'Ens. Mean', hoverinfo = 'text+x+y',text='Ens. Mean',showlegend = FALSE,
                                   marker = list(color = 'rgb(255,127,80)', symbol = 17,line =list(width = 2,color = '#FFFFFF'), size = 20))
        
        p.sc <- p.sc %>% add_trace(x = df$dtas[length(df$dtas)],y = df$dpr[length(df$dpr)],type = 'scatter',mode = 'markers',
                                   name = 'REF', hoverinfo = 'text+x+y',text='ERAINT',showlegend = FALSE,
                                   marker = list(color = 'black', symbol = 17,line = list(width = 2,color = '#FFFFFF'), size = 20,opacity=0.7))
      }
      ylab <- 'Change in temperature [deg. C]'
      xlab <- 'Change in precipitation [%]'
      
      p.sc <- p.sc %>% layout(title = FALSE,
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
                                           zeroline = TRUE),
                              annotations = list(
                                yref="paper",
                                xref="paper",
                                y=1.07,
                                x=0,
                                text=paste("Region | ", input$rcm.cc.region),
                                showarrow=F,
                                font=list(size=14,weight='bold')
                              )
      )
      
      if (input$rcm.legend.sc == 'Hide') {
        p.sc <- p.sc %>% layout(showlegend = FALSE)
      } else {
        p.sc <- p.sc %>% layout(showlegend = TRUE)
      }
      #rcm.dtdp$elementId <- NULL
      # rcm.dtdp
      p.sc$elementId <- NULL
      p.sc
    })
    
    output$rcm.scatter.data <- DT::renderDataTable({
      
      rcm.meta.pr <- rcm.meta.pr.reactive()
      dpr <- rcm.sc.pr.reactive()
      rcms <- names(dpr)
      if (input$rcm.outputValues == 'Bias') {
        dpr <- ((dpr - dpr[,dim(dpr)[2]]) / dpr[,dim(dpr)[2]]) * 100
      } else if (input$rcm.outputValues == 'RMSE') {
        dpr <- sqrt((((dpr - dpr[,dim(dpr)[2]]) / dpr[,dim(dpr)[2]]))^2) * 100
      } else if (input$rcm.outputValues == 'Anomaly') {
        DF <- t(dpr)
        dpr <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$rcm.outputValues == 'Change') {
        dpr <- ((dpr - rcm.sc.pr.present())/ rcm.sc.pr.present()) * 100
      } 
      
      rcm.meta.tas <- rcm.meta.tas.reactive()
      
      dtas <- rcm.sc.tas.reactive()
      if (input$rcm.outputValues == 'Bias') {
        dtas <- dtas - dtas[,dim(dtas)[2]]
      } else if (input$rcm.outputValues == 'RMSE') {
        dtas <- sqrt((dtas-dtas[,dim(dtas)[2]])^2)
      } else if (input$rcm.outputValues == 'Anomaly') {
        DF <- t(dtas)
        dtas <- as.data.frame(t(DF - rowMeans(DF)))
      } else if (input$rcm.outputValues == 'Change') {
        dtas <- dtas - rcm.sc.tas.present()
      }
      
      if (!is.null(input$rowsRcm)) {
        if (input$rcm.sim.sc == 'Selected Simulations') {
          dpr <- dpr[input$rowsRcm,]
          dtas <- dtas[input$rowsRcm,]
          rcm.meta.tas <- rcm.meta.tas[input$rowsRcm,]
          rcm.meta.pr <- rcm.meta.pr[input$rowsRcm,]
          rcms <- rcms[input$rowsRcm]
        } 
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
    
    ## compute summary statistics e.g. ens. mean, quantiles, etc ... use this form 
    #spi.stat <- spi.reactive()
    output$spi.settings <- renderText({
      paste(input$spi.sim,' | ',input$spi.freq,' | ',input$spi.group,' | ',input$spi.period,' | ',input$spi.stat)
    })
    output$map.spi <- renderLeaflet({
      zmap <- spi.reactive()
      cat('Observe','SPI-Index',paste('SPI-',input$it,sep=''),'GROUP', input$group,'PERIOD',input$period)
      cat(sep = '\n')
      x <- as.numeric(attr(zmap,'longitude'))
      y <- as.numeric(attr(zmap,'latitude'))
      #z <- apply(zmap,c(2,3),mean,na.rm=TRUE)
      z <- zmap[grep(input$spi.sim,rcm.names),,]
      #z[is.na(z)] <- -999
      ## 
      #Create raster object
      dat1 <- list(x=x,y = y, z = z)
      dim(dat1$z) <- c(length(dat1$x),length(dat1$y))
      r <- raster(dat1)
      print(print(object.size(r),units = 'Mb'))
      
      rev <- FALSE
      col <- 'warm'
      rng <- round(range(r@data@values,na.rm=TRUE),digits = 1)
      breaks <- c(0,max(rng))
      #breaks <- seq(-5,5,0.5)
      leg.title <- "SPI [-]"
      
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
        setView(lat=55,lng = 8, zoom = 4) %>%
        addRasterImage(x = r,colors = pal, opacity = 0.6)
        m <- m %>% addLegend("bottomleft", values=round(r@data@values, digits = 2), 
                             title=leg.title, colors = rev(colscal(col= col, rev = rev, n=length(pretty(breaks,n = 10)))),
                             labels = rev(pretty(breaks,n = 10)),#pal=pal, 
                             layerId="colorLegend")  # labFormat = myLabelFormat(reverse_order = F)
       m
    })
  })
  
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
  
  ## Documentation
  observe({
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
    
    txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The continuous line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
    
    # GCM info text output
    output$figcaption.gcm.sc.tas = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The continuous line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.gcm.tas.pu = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the bias in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The continuous line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.gcm.tas.cc = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the changes in simulated historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The continuous line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.gcm.sc.tas.pu = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The continuous line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.gcm.sc.pr = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected monthly precipitation totals. The blue line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.gcm.pr.pu = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the bias in historical and projected monthly precipitation totals. The blue line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.gcm.sc.pr.pu = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected monthly precipitation totals. The blue line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.gcm.pr.cc = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the changes in seasonal cycle in historical and projected monthly precipitation totals. The blue line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.gcm.scatter = renderInfoBox({
      txt <- tags$h5('Interactive Scatter Plot showing surface air mean temperature VS mean monthly sums of precipitation. The orange star and red envelope show the mean and the spread from the multi-model ensemble of simulations. 
                   The black star shows the corresponding values from reanalysis data used as reference (ERAINT).')   
      infoBox(strong('How to read the scatter plot!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.gcm.cc.scatter = renderInfoBox({
      txt <- tags$h5('Interactive Scatter Plot showing changes in surface air mean temperature VS mean monthly sums of precipitation. The orange star and red envelope show the mean and the spread from the multi-model ensemble of simulated changes. 
                   The black star shows the corresponding values from reanalysis data used as reference (ERAINT).')   
      infoBox(strong('How to read the scatter plot!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    # RCM info text output
    output$figcaption.rcm.sc.tas = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The continuous line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.rcm.tas.cc = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the future changes seasonal cycle in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The continuous line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    output$figcaption.rcm.tas.pu = renderInfoBox({
      txt <- tags$h5('Interactive charts  evaluating the bias in seasonal cycle of historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The continuous line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    output$figcaption.rcm.sc.tas.pu = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected surface air temperature assuming the intermediate (RCP4.5) emission scenario. The continuous line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.rcm.sc.pr = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected monthly precipitation totals. The blue line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.rcm.pr.cc = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the future changes in seasonal cycle in historical and projected monthly precipitation totals. The blue line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.rcm.pr.pu = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the bias in seasonal cycle of historical and projected monthly precipitation totals assuming the intermediate (RCP4.5) emission scenario. The blue line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.rcm.sc.pr.pu = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the seasonal cycle in historical and projected monthly precipitation totals. The blue line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.rcm.cc.scatter = renderInfoBox({
      txt <- tags$h5('Interactive charts evaluating the future changes in seasonal cycle in historical and projected monthly precipitation totals. The blue line and envelope show the mean and the spread from the multi-model ensemble of simulations. The dashed line shows the seasonal cycle from reanalysis data used as reference.')   
      infoBox(strong('How to read the chart!'),txt, icon = shiny::icon("bar-chart-o"),color = 'olive')
    })
    
    output$figcaption.rcm.scatter = renderInfoBox({
      txt <- tags$h5('Interactive Scatter Plot showing surface air mean temperature VS mean monthly sums of precipitation. The orange star and red envelope show the mean and the spread from the multi-model ensemble of simulations. 
                   The black star shows the corresponding values from reanalysis data used as reference (ERAINT).')   
      infoBox(strong('How to read the scatter plot!'),txt, icon = shiny::icon("line-chart-o"),color = 'olive')
    })
    
    # Text info
    txtTips <- tags$h5('You can modify the type of output from the "Settings & Outputs" box and choose between options of showing individual simulations, envelope of the ensemble model simulations, or box plots. They let you show anomalies and group/colour the results according to the metadata. “Individual Simulations” allow double-click on specific climate models listed in the legend or the metadata table to isolate one or a group of simulations.')
    txtMoreTips <- tags$h5('Other options include zooming in/out, comparing simulations, and downloading the graphic. The types of evaluation includes the mean seasonal cycle of the mean as well as the spatial standard deviation or spatial correlation, and you can download the data and further details about the simulations by selecting the tabs labelled “Data” or “Metadata”. The evaluation shown here are for multi-model ensemble of CMIP5 RCP4.5 simulations.')
    txtRemember <- tags$h5('These simulations are based on models and data to represent the climate system. Those models are in turn based on coarse resolution, different parameterization schemes and simplifications of physical processes which systematically lead to deviations (biases) from the reference data.')
    
    figTips = renderInfoBox({
      infoBox(strong('Tips on how to modify the chart to meet your needs!'),txtTips, icon = shiny::icon("info-sign", lib = "glyphicon"),color = 'orange')
    })
    
    figMoreTips = renderInfoBox({
      infoBox(strong('More Tips on how to use the chart!'),txtMoreTips, icon = shiny::icon("plus-sign", lib = "glyphicon"),color = 'light-blue')
    })
    
    figRemember = renderInfoBox({
      infoBox(strong('Recommendations on how to use the chart!'),txtRemember, icon = shiny::icon("asterisk", lib = "glyphicon"),color = 'red')
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
}





