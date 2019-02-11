# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution
library(shinydashboard)
library(shinyBS)

data("metaextract")
M <- data.frame(list(Project=meta$project_id,Experiment=meta$experiment_id,GCM=meta$gcm,
                     RIP=meta$gcm_rip, RCM=meta$rcm, VAR=meta$var, Unit=meta$unit, Resolution=paste(meta$resolution,"deg"),
                     Domain=paste(gsub(","," - ",meta$lon),"E"," / ",paste(gsub(","," - ",meta$lat)),"N",sep=""), 
                     Years=gsub(",","-",gsub("-[0-9]{2}","",meta$dates)), URL=meta$url))

dashboardPage(title = 'Data Evaluation for Climate Models (DECM)',skin = 'red',
              dashboardHeader(title = tags$h1('DATA EVALUATION FOR CLIMATE MODELS'), #titleWidth = '500px', 
                              #set height for header
                              tags$li(class = "dropdown",
                                      tags$style(".main-header {height: 60px}"),
                                      tags$style(".main-header .logo {height: 60px}"),
                                      tags$style(".main-header .logo {text-align :left !important;}"),
                                      tags$style(".main-header {text-align :left !important;}"),
                                      tags$style(".main-header .sidebar-toggle {color:#EEE;margin: left !important;}")
                              )
              ),
              dashboardSidebar(collapsed = FALSE,width = '400px',
                               sidebarMenu(
                                 menuItem("Product Users", tabName = 'pu',startExpanded = TRUE,
                                          menuSubItem("Explore the Simulations", tabName = "browse"),
                                          menuSubItem("Seasonal Cycle", tabName = "seasonalCycle",selected = TRUE),
                                          menuSubItem("Models' Biases", tabName = "bias"),
                                          #menuSubItem("test iframe", tabName = "score1"),
                                          #menuSubItem("Individual Model", tabName = "score4"),
                                          menuSubItem("Changes in Climate", tabName = "score5"),
                                          menuSubItem("Models' Spread (Cf. External App.)", href = 'https://esdlab.met.no/gcmeval/')),
                                 menuItem("Data Users", tabName = 'pu',startExpanded = TRUE,
                                          menuSubItem("Global Climate Models", tabName = "gcms"),
                                          menuSubItem("Regional Climate Models", tabName = "rcms"),
                                          menuSubItem("Models' Ranks (Cf. External App.)", href = 'https://esdlab.met.no/gcmeval/')
                                          #menuSubItem("Reanalysis", tabName = "rea")
                                          #menuSubItem("Weather Stations", tabName = "stations"),
                                          #menuSubItem("Satellite Data",  tabName = "sat"), # href = 'https://esdlab.met.no/gcmeval/'
                                          #menuSubItem("Storm tracks", href = 'http://157.249.177.25:3838/Storms/')
                                 ),
                                 menuItem("Sectoral Communication", tabName = 'sc',startExpanded = FALSE,
                                          menuSubItem("Standardized Precipitation Index", tabName = "spi"),
                                          menuSubItem("Water managmement", tabName = "hydro"),
                                          menuSubItem("Agriculture and Forestry", tabName = "agriculture"),
                                          menuSubItem("Tourism", tabName = "tourism"),
                                          menuSubItem("Insurance", tabName = "insurance"),
                                          menuSubItem("Energy", tabName = "energy"),
                                          menuSubItem("Health", tabName = "health"),
                                          menuSubItem("Transport", tabName = "transport"),
                                          menuSubItem("Infrastructure", tabName = "infrastructure"),
                                          menuSubItem("Disaster Risk Reduction", tabName = "disaster"),
                                          menuSubItem("Coastal Areas", tabName = "coastareas"),
                                          menuSubItem("Defense", tabName = "defense"),
                                          menuSubItem("Cities and Urban areas", tabName = "cities")),
                                 menuItem("Settings and filtering", tabName = "du", startExpanded = FALSE,
                                          sliderInput("dates7", "Years",min=1900, max=2099,
                                                      step = 5, value= c(1900,2099),
                                                      sep="",width = "100%"),
                                          bsPopover(id = 'dates7',title = 'Select the years to include in the simulations',
                                                    content = 'The time span for the simulaitons. You can expand or reduce the time span at your convenience with a frequecny of 5 years. This input is useful when computing the changes (past or future) with regards to the base period.',
                                                    placement = 'bottom',options = list(container = "body")),
                                          sliderInput("datesref", "Base period",min=1900, max=2099,
                                                      step = 5, value= c(1980,2010),
                                                      sep="",width = "100%"),
                                          textInput("threshold8",label = "Threshold", placeholder = "1",width = '100%'),
                                          p('Map settings'),
                                          selectInput("legend",label = "Legend",choices = c("Display","Hide"),
                                                      selected = "Display",width = '100%'),
                                          selectInput("minimap",label = "Mini Map",choices = c("Display","Hide"),
                                                      selected = "Hide",width = '100%')
                                 )
                               )
              ),
              dashboardBody(
                #include google analytics
                tags$link(href='https://fonts.googleapis.com/css?family=Karla',rel='stylesheet'),
                tags$head(tags$style(HTML('
					  .main-header .sidebar-toggle:before {
					          content: "MENU (Click to display or hide)";font-family:Karla;font-size:16px;}
					  /* logo */
                                          .skin-red .main-header .logo {
                                          background-color: #871010;
                                          }
                                          
                                          /* logo when hovered */
                                          .skin-red .main-header .logo:hover {
                                          background-color: #871010;
                                          }
                                          
                                          /* navbar (rest of the header) */
                                          .skin-red .main-header .navbar{
                                          background-color: #871010;
                                          }        
                                          
                                          /* other links in the sidebarmenu when hovered */
                                          .skin-red .main-sidebar .sidebar .sidebar-menu a:hover{
                                          background-color: #871010;
                                          }
                                          /* toggle button when hovered  */                    
                                          .skin-red .main-header .navbar .sidebar-toggle:hover{
                                          background-color: #871010;
					  margin-left:0px;
					  overflow: auto;
					  }

                '))),
                tags$body(tags$style(HTML('
                .content {
		max-width:1028px;
		margin: auto !important;
		font-family: Karla !important;
                font-size: 16px;
                text-align: left;
		line-height:22px;
		}
                '))),
                tags$head(tags$style(HTML('
               h1 {
                  font-family:Karla;
		  font-size:18px;
		  font-weight:bold;
               	  text-align:center;
		  padding: 0px;
		  margin-top:7px;
	       }
	       .main-header {
               font-family: Karla !important;
               font-size: 16px;
               text-align: center;}
               '))),
                tags$head(tags$style(HTML('
                .box {
                      font-family: Karla !important;
		      margin: 5px;	      
                }
                .box.box-solid.box-danger>.box-header {
                    background:#b21c1c;
                }
                .box.box-solid.box-danger{
                    border-bottom-color:#b21c1c;
                    border-left-color:#b21c1c;
                    border-right-color:#b21c1c;
                    border-top-color:#b21c1c;
                '))),
                tags$head(tags$style(HTML('
                .popover-title{
                              color: #FFE4E1;
                              font-size: 16px;
                              background-color:#b21c1c;
                              }
                .popover-content {
                  font-size: 12px;
                }
                '))),
                tags$head(HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-108282573-4'></script>
                               <script>
                               window.dataLayer = window.dataLayer || [];
                               function gtag(){dataLayer.push(arguments);}
                               gtag('js', new Date());
                               gtag('config', 'UA-108282573-4', { 'anonymize_ip': true });
                               </script>"
                )),
                tags$head(
                  tags$style(
                    HTML(".shiny-notification {
                         font-family:Karla;
			 font-weight: bold;
			 position:fixed;
                         top: calc(5.5%);
                         left: calc(40%);
                         right: calc(20%);
                         opacity : 1;}"
                    )
                  )
                ),
                #send information to google analytics
                #this includes the event name and the value it is set to
                #omit sending plotting information (i.e., events starting with .client)
                tags$script(HTML(
                  "$(document).on('shiny:inputchanged', function(event) {
                  if (event.name.substr(1,6) !== 'client') {
                  newname = event.name+' set to '+event.value;
                  gtag('event', newname, {'event_category': 'User interaction'});
                  }
                  });"
                )),
                
                tabItems(
                  tabItem(tabName = "browse", 
                          fluidPage(
                            tags$br(),tags$br(),
			    fluidRow(
                              box(status = 'danger',solidHeader = TRUE, width = '100%',
                                  'Here, you can',
                                  tags$ol(
                                    tags$li("check the GLOSSARY which contains a list of abbreviations and climate variable names including a short description"), 
                                    tags$li("FILTER or refine your selection from a list of various settings, e.g. selecting a specific GCM, RCM, ..."), 
                                    tags$li("explore available CLIMATE MODEL SIMULATIONS based on your selection and get a quick link to the data (redirected from http://climexpl.knmi.nl)")),                                    title = "Product Users | Explore the simulations",collapsible = TRUE,collapsed = FALSE
                              )
                            ),
                            fluidRow(
                              box(status = 'danger',solidHeader = FALSE, width = '100%',collapsible=FALSE,
                                  tags$div(HTML('<p style="color:#871010;">TIPS | Climate projections describe a range of possible climate outcomes.</p>
				   <details style="color:#871010;"><summary>Read More</summary> 
                                   Climate projections are based on different climate models with different set-ups. 
                                   How should I use this information? It is important to evaluate the climate models’ 
                                   ability to simulate changes, and one way to do so is to examine how they reproduce 
                                   the mean seasonal cycle in temperature and precipitation. We examine model output 
                                   collected from the Climate Model Intercomparison Project - Phase5 (CMIP5), 
                                   the Coordinated Regional Climate Downscaling Experiment over Europe (EURO-CORDEX), 
                                   and the Empirical-Statistical Downscaling project (ESD) at the Norwegian Meteorological 
                                   Institute to provide the best estimates of global/regional/local climate signal that in turn can be used in impact studies. Click on the dashboard to navigate between other items.</details>')))),
                            #fluidRow(
                            #   box(width = '100%',
                            #       column(12, infoBoxOutput('simulation',width = '100%')),
                            #       title = 'Read Me First',status = 'danger', solidHeader = TRUE, collapsible = TRUE, 
                            #       collapsed = FALSE)
                            # ),
                            fluidRow(
                              box(width = '100%',
                                  column(12,DT::dataTableOutput('glossary',width = '100%')),
                                  title = 'Glossary',status = 'danger', solidHeader = TRUE, collapsible = TRUE, 
                                  collapsed = TRUE)
                            ),
                            fluidRow(
                              box(width = '100%',
                                  column(12,
                                         selectInput("project",label = "Project",
                                                     choices = c('none',as.character(M$Project)),selected = 'none',width = '100%'),
                                         selectInput("exp",label = "Experiment",
                                                     choices = c('none',as.character(M$Experiment)), selected = 'none',width = '100%'),
                                         selectInput("gcm",label = "Global Climate Model",
                                                     choices = c('none',as.character(M$GCM)), selected = 'none',width = '100%'),
                                         selectInput("rcm",label = "Regional Climate Model",
                                                     choices = c('none',as.character(M$RCM)),selected = 'none',width = '100%'),
                                         selectInput("run",label = "RUN",
                                                     choices = c('none',as.character(M$RIP)),selected = 'none',width = '100%'),
                                         selectInput("var",label = "Climate Variable",
                                                     choices = c('none',as.character(M$VAR)),selected = 'none',width = '100%'),
                                         selectInput("dates",label = "Dates",
                                                     choices = c('none',as.character(M$Years)),selected = 'none',width = '100%'),
                                         selectInput("url",label = "URL",
                                                     choices = c('none',as.character(M$URL)), selected = 'none', width = '100%')),
                                  title = 'Filter by',status = 'danger', solidHeader = TRUE,collapsible = TRUE, 
                                  collapsed = TRUE)
                            ),
                            fluidRow(
                              box(width = '100%',
                                  column(12,
                                         DT::dataTableOutput("browser",width = '100%')),
                                  title = 'Climate Model Simulations',status = 'danger', solidHeader = TRUE,collapsible = TRUE, 
                                  collapsed = FALSE))
                          )
                  ),
                  tabItem(tabName = "bias", 
                          tags$br(),tags$br(),
			  box(status = 'danger',solidHeader = TRUE, width = '100%',
                              'Here, you can',
                              tags$ol(
                                tags$li("select a region to navigate through various predefined regions (EURO-CORDEX, PRUDENCE, European countries)"), 
                                tags$li("modify the default settings and select the output type (e.g. chart, boxplot) and values (e.g. bias, change)"), 
                                tags$li("evalutate biases in monthly mean air Temperature statistics"),
                                tags$li("evaluate biases in monthly precipitaiton totals statistics")
                              ),
                              title = "Product Users | Models' biases",collapsible = TRUE, collapsed = FALSE
                          ),
                          box(status = 'danger',solidHeader = FALSE,width = '100%',collapsible = TRUE,title = 'TIPS',
                              tags$div(HTML('<p style="color:#871010;">TIPS | One way to assess the skill of climate models is to examine the model biases in reproducing the seasonal cycle. 
                                      Here, you can navigate between (CMIP5, A) global and (EURO-CORDEX, B) regional climate models, modify the settings so that they fit your needs, and explore how the models reproduce the monthly mean air temperature and precipitation totals over a number of pre-defined regions. You can additionally click on the dashboard menu to navigate between other evaluations of climate model simulations.</p>'))
                          ),
                          tabsetPanel(type = "tabs",
                                      tabPanel("A | Global Climate Model Evaluation", p(), 
                                               fluidPage(
                                                 fluidRow(
                                                   box(width = '100%', solidHeader = TRUE, status = 'danger',
                                                       tags$figcaption('The regions used the AR5 Reference Regions which include 26 regions defined in SREX. 
                                                                       In addition to these regions, the Arctic, Antarctic, South Asia and South-East Asia) and 
                                                                       three global analysis domains: land only, sea only and all points. ',
                                                                       tags$a(href = 'http://www.ipcc-data.org/guidelines/pages/ar5_regions.html',"Read more")),
                                                       tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                                       column(12,
                                                              column(4,selectInput("gcm.region.pu", label = NULL, 
                                                                                   choices = region.names,
                                                                                   selected = "Global",width = '100%')),
                                                              column(8,helpText('You can navigate between the various predefined regions.'))
                                                       ),
                                                       leafletOutput('gcm.region.pu',width = '100%',height = '600'),
                                                       title = tags$p('1. Select a region : Explore and navigate through various regions (AR5 predefined regions)'), 
                                                       collapsible = TRUE, collapsed = TRUE)
                                                 ),
                                                 fluidRow(
                                                   box(width = '100%', solidHeader = TRUE, status = 'danger',
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("gcm.period.pu", label = "Period", 
                                                                                     choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                                                 "Far Future (2071-2100)"),
                                                                                     selected = "Present",width = '100%'),
                                                                       bsPopover(id = 'gcm.period.pu',title = 'test2',content = 'this is a test',placement = 'top')),
                                                                column(8,br(),helpText('You can navigate between one control and two (near and far) future time horizons.'))
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("gcm.chart.type.pu", label = "Chart Output", 
                                                                                     choices = c("Individual Simulations",
                                                                                                 "Ensemble of All Simulations",
                                                                                                 "Box Plots of All Simulations"),
                                                                                     selected = "Ensemble of All Simulations",width = '100%')),
                                                                column(8,br(),
                                                                       helpText('You can modify the layout of the chart to display individual simulations or the envelope-based on all simulations.'))
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("gcm.stat.pu", label = "Statistics", 
                                                                                     choices = c('Mean','Standard Deviation','Spatial Correlation'))),
                                                                column(8,br(),
                                                                       helpText('You can display values for various statistics such as the mean and the spatial standard deviation. The spatial correlation are only computed between historical simulations and the reference data for the present (1981-2010).'))
                                                         )
                                                       ),                                         
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("gcm.var.pu", label = "Variables", 
                                                                                     choices = c('Individual','Synchronised'),
                                                                                     selected = 'Synchronised',width = '100%')),
                                                                column(8,br(),helpText('You can filter the simulations and keep only identical simulations for all climate variables such as precipitation and temperature.',width = '100%'))
                                                         )
                                                       ),
                                                       title = tags$p('2. Settings & Outputs : Modify the default settings and select the output type and values.'), collapsible = TRUE, collapsed = TRUE)
                                                 ),
                                                 fluidRow(
                                                   box(
                                                     column(12,
                                                            plotlyOutput("gcm.sc.bias.tas.pu",width = '100%',height = '500'))
                                                     ,
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figcaption.gcm.tas.pu",width= '100%')
                                                              ))
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',solidHeader = TRUE,width = '100%',
                                                                  infoBoxOutput("figTips.gcm.tas.pu",width= '100%')
                                                              )
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figMoreTips.gcm.tas.pu",width= '100%')
                                                              )
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figRemember.gcm.tas.pu",width= '100%')
                                                              )
                                                       )
                                                     ),
                                                     title = '3. Bias in monthly mean air Temperature',
                                                     status = 'danger',solidHeader = TRUE,width = '100%',
                                                     collapsible = TRUE,collapsed = FALSE)
                                                 ),
                                                 fluidRow(
                                                   box(
                                                     column(12,plotlyOutput("gcm.sc.bias.pr.pu",height = '600'))
                                                     ,
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figcaption.gcm.pr.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figTips.gcm.pr.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figMoreTips.gcm.pr.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figRemember.gcm.pr.pu",width= '100%'))
                                                       )
                                                     ),
                                                     title = '4. Bias in monthly precipitation totals',
                                                     status = 'danger',solidHeader = TRUE,width = '100%',
                                                     collapsible = TRUE,collapsed = FALSE)
                                                 )
                                               )
                                      ),
                                      tabPanel("B | Regional Climate Model Evaluation", p(), 
                                               fluidPage(
                                                 fluidRow(
                                                   box(width = '100%', solidHeader = TRUE, status = 'danger',
                                                       #tags$figcaption('The EURO-CORDEX domain.',
                                                       #                tags$a(href = 'http://www.cordex.org/domains/cordex-region-euro-cordex/',"Read more")),
                                                       column(12,
                                                              column(4,selectInput("rcm.region.pu", label = NULL, 
                                                                                   choices = regions.all,
                                                                                   selected = "Europe",width = '100%')),
                                                              column(8,helpText('You can navigate between the various predefined regions such as Europe (EURO-CORDEX domain, PRUDENCE regions, and national regions'))
                                                       ),
                                                       leafletOutput('rcm.bias.region.pu',width = '100%',height = 500),
                                                       title = tags$p('1. Select a region : Explore and navigate through various regions (EURO-CORDEX, PRUDENCE, European countries)'),
                                                       # title = tags$p('1. Select a region : Explore and navigate through various predefined PRUDENCE regions from the list below. You can expand the window to display the spatial domain on a map.',
                                                       #                selectInput("rcm.region.pu", label = NULL,choices = regions.all,selected = "Europe",width = '100%')), 
                                                       collapsible = TRUE, collapsed = TRUE)
                                                 ),
                                                 fluidRow(
                                                   box(width = '100%', solidHeader = TRUE, status = 'danger',
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("rcm.period.pu", label = "Period", 
                                                                                     choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                                                 "Far Future (2071-2100)"),
                                                                                     selected = "Present",width = '100%')),
                                                                column(8,br(),helpText('You can navigate between one control and two (near and far) future time horizons.'))
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("rcm.chart.type.pu", label = "Chart Output", 
                                                                                     choices = c("Individual Simulations",
                                                                                                 "Ensemble of All Simulations",
                                                                                                 "Box Plots of All Simulations"),
                                                                                     selected = "Ensemble of All Simulations",width = '100%')),
                                                                column(8,br(),
                                                                       helpText('You can modify the layout of the chart to display individual simulations or the envelope-based on all simulations.'))
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("rcm.stat.pu", label = "Statistics", 
                                                                                     choices = c('Mean','Standard Deviation'))
                                                                ),
                                                                column(8,br(),
                                                                       helpText('You can display values for various statistics such as the mean and the spatial standard deviation. The spatial correlations are computed only between historical simulations and the reference data for the present (1981-2010).'))
                                                         )
                                                       ),                                         
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("rcm.var.pu", label = "Variables", 
                                                                                     choices = c('Individual','Synchronised'),
                                                                                     selected = 'Synchronised',width = '100%')),
                                                                column(8,br(),helpText('You can filter the simulations and keep only identical simulations for all climate variables such as precipitation and temperature.',width = '100%'))
                                                         )
                                                       ),
                                                       title = tags$p('2. Settings & Outputs : Modify the default settings and select the output type and values.'), collapsible = TRUE, collapsed = TRUE)
                                                 ),
                                                 fluidRow(
                                                   box(
                                                     
                                                     column(12,
                                                            plotlyOutput("rcm.sc.bias.tas.pu",height = '600'))
                                                     ,
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figcaption.rcm.tas.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figTips.rcm.tas.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figMoreTips.rcm.tas.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figRemember.rcm.tas.pu",width= '100%'))
                                                       )
                                                     ),
                                                     title = '3. Bias in monthly mean air Temperature',
                                                     status = 'danger',solidHeader = TRUE,width = '100%',
                                                     collapsible = TRUE,collapsed = FALSE)
                                                 ),
                                                 fluidRow(
                                                   box(
                                                     
                                                     column(12,
                                                            plotlyOutput("rcm.sc.bias.pr.pu",height = '500'))
                                                     ,
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figcaption.rcm.pr.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figTips.rcm.pr.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figMoreTips.rcm.pr.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figRemember.rcm.pr.pu",width= '100%'))
                                                       )
                                                     ),
                                                     title = '4. Bias in monthly precipitation totals',
                                                     status = 'danger',solidHeader = TRUE,width = '100%',
                                                     collapsible = TRUE,collapsed = TRUE)
                                                 )
                                               )
                                      )
                          )
                  ),
                  tabItem(tabName = "seasonalCycle",
                          fluidPage(
                            tags$br(),tags$br(),
			    fluidRow(box(status = 'danger',solidHeader = TRUE, width = '100%', 
                                         'Here, you can',
                                         tags$ol(
                                           tags$li("select a region to navigate through various predefined regions (EURO-CORDEX, PRUDENCE, European countries)"), 
                                           tags$li("modify the default settings and select the output type (e.g. chart, boxplot) and values (e.g. bias, change)"), 
                                           tags$li("evalutate the seasonal cycle of monthly mean air Temperature statistics"),
                                           tags$li("evaluate the seasonal cycle of monthly precipitaiton totals statistics")
                                         ),
                                         title = "Product Users | Seasonal Cycle",collapsible = TRUE,collapsed = FALSE
                            )),
                            fluidRow(box(status = 'danger',solidHeader = TRUE,width = '100%',
                                         tags$div(HTML('<p style="color:#871010;">TIPS | One way to assess the skill of climate models is to examine how they reproduce the seasonal cycle. 
                                    Here, you can navigate between (CMIP5, A) global and (Euro-CORDEX, B) regional climate models, modify the settings so that they fit your needs, and explore how the models reproduce the monthly mean air temperature and precipitation totals over a number of pre-defined regions. Click on the dashboard to navigate between other evaluation items.<p/>'))
                            ))),
                          tabsetPanel(type = "tabs",
                                      tabPanel("A | Global Climate Model Evaluation", p(),
                                               fluidPage(
                                                 fluidRow(
                                                   box(width = '100%', solidHeader = TRUE, status = 'danger',
                                                       tags$figcaption('The regions used the AR5 Reference Regions which include 26 regions defined in SREX.
                                                                     In addition to these regions, the Arctic, Antarctic, South Asia and South-East Asia) and
                                                                     three global analysis domains: land only, sea only and all points. ',
                                                                       tags$a(href = 'http://www.ipcc-data.org/guidelines/pages/ar5_regions.html',"Read more")),
                                                       tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                                       column(12,
                                                              column(4,selectInput("gcm.sc.region.pu", label = NULL,
                                                                                   choices = region.names,
                                                                                   selected = "Global",width = '100%')),
                                                              column(8,helpText('You can navigate between the various predefined regions.'))
                                                       ),
                                                       leafletOutput('gcm.sc.region.pu',width = '100%',height = 500),
                                                       title = tags$p('1. Select a region : Explore and navigate through various regions (AR5 predefined regions)'),
                                                       collapsible = TRUE, collapsed = TRUE)
                                                 ),
                                                 fluidRow(
                                                   box(width = '100%', solidHeader = TRUE, status = 'danger',
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("gcm.sc.period.pu", label = "Period",
                                                                                     choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                                                 "Far Future (2071-2100)"),
                                                                                     selected = "Present",width = '100%')),
                                                                column(8,br(),helpText('You can navigate between one control and two (near and far) future time horizons.'))
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("gcm.sc.chart.type.pu", label = "Chart Output",
                                                                                     choices = c("Individual Simulations",
                                                                                                 "Ensemble of All Simulations",
                                                                                                 "Box Plots of All Simulations"),
                                                                                     selected = "Ensemble of All Simulations",width = '100%')),
                                                                column(8,br(),
                                                                       helpText('You can modify the layout of the chart to display individual simulations or the envelope-based on all simulations.'))
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("gcm.sc.stat.pu", label = "Statistics",
                                                                                     choices = c('Mean','Standard Deviation','Spatial Correlation'))),
                                                                column(8,br(),
                                                                       helpText('You can display values for various statistics such as the mean and the spatial standard deviation. The spatial  correlations are computed only between historical simulations and the reference data for the present (1981-2010).'))
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("gcm.sc.var.pu", label = "Variables",
                                                                                     choices = c('Individual','Synchronised'),
                                                                                     selected = 'Synchronised',width = '100%')),
                                                                column(8,br(),helpText('You can filter the simulations and keep only identical simulations for all climate variables such as precipitation and temperature.',width = '100%'))
                                                         )
                                                       ),
                                                       title = tags$p('2. Settings & Outputs : Modify the default settings and select the output type and values.'), collapsible = TRUE, collapsed = TRUE)
                                                 ),
                                                 fluidRow(
                                                   box(
                                                     column(12,
                                                            plotlyOutput("gcm.sc.tas.pu",height = '500'))
                                                     ,
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figcaption.gcm.sc.tas.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figTips.gcm.sc.tas.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figMoreTips.gcm.sc.tas.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figRemember.gcm.sc.tas.pu",width= '100%'))
                                                       )
                                                     ),
                                                     title = '3. Seasonal Cycle of monthly mean air Temperature',
                                                     status = 'danger',solidHeader = TRUE,width = '100%',
                                                     collapsible = TRUE,collapsed = FALSE)
                                                 ),
                                                 fluidRow(
                                                   box(
                                                     column(12,
                                                            plotlyOutput("gcm.sc.pr.pu",height = '500'))
                                                     ,
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figcaption.gcm.sc.pr.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figTips.gcm.sc.pr.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figMoreTips.gcm.sc.pr.pu",width= '100%'))
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              box(status = 'danger',width = '100%',solidHeader = TRUE,
                                                                  infoBoxOutput("figRemember.gcm.sc.pr.pu",width= '100%'))
                                                       )
                                                     ),
                                                     title = '4. Seasonal Cycle of monthly precipitation totals',
                                                     status = 'danger',solidHeader = TRUE,width = '100%',
                                                     collapsible = TRUE,collaped = FALSE)
                                                 )
                                               )
                                      ),
                                      tabPanel("B | Regional Climate Model Evaluation", p(),
                                               fluidPage(
                                                 fluidRow(
                                                   box(width = '100%', solidHeader = TRUE, status = 'danger',
                                                       #tags$figcaption('The EURO-CORDEX domain.',
                                                       #                tags$a(href = 'http://www.cordex.org/domains/cordex-region-euro-cordex/',"Read more")),
                                                       column(12,
                                                              column(4,selectInput("rcm.sc.region.pu", label = NULL,
                                                                                   choices = regions.all,
                                                                                   selected = "Europe",width = '100%')),
                                                              column(8,helpText('You can navigate between the various predefined regions.'))
                                                       ),
                                                       leafletOutput('rcm.sc.region.pu',width = '100%',height = '500'),
                                                       title = tags$p('1. Select a region : Explore and navigate through various predefined regions (EURO-CORDEX, PRUDENCE, European countries)'),
                                                       collapsible = TRUE, collapsed = TRUE)
                                                 ),
                                                 fluidRow(
                                                   box(width = '100%', solidHeader = TRUE, status = 'danger',
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("rcm.sc.period.pu", label = "Period",
                                                                                     choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                                                 "Far Future (2071-2100)"),
                                                                                     selected = "Present",width = '100%')),
                                                                column(8,br(),helpText('You can navigate between one control and two (near and far) future time horizons.'))
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("rcm.sc.chart.type.pu", label = "Chart Output",
                                                                                     choices = c("Individual Simulations",
                                                                                                 "Ensemble of All Simulations",
                                                                                                 "Box Plots of All Simulations"),
                                                                                     selected = "Ensemble of All Simulations",width = '100%')),
                                                                column(8,br(),
                                                                       helpText('You can modify the layout of the chart to display individual simulations or the envelope-based on all simulations.'))
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("rcm.sc.stat.pu", label = "Statistics",
                                                                                     choices = c('Mean','Standard Deviation','Spatial Correlation'))),
                                                                column(8,br(),
                                                                       helpText('You can display values for various statistics such as the mean and the spatial standard deviation. The spatial  correlations are computed only between historical simulations and the reference data for the present (1981-2010).'))
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(12,
                                                                column(4,selectInput("rcm.sc.var.pu", label = "Variables",
                                                                                     choices = c('Individual','Synchronised'),
                                                                                     selected = 'Synchronised',width = '100%')),
                                                                column(8,br(),helpText('You can filter the simulations and keep only identical simulations for all climate variables such as precipitation and temperature.',width = '100%'))
                                                         )
                                                       ),
                                                       title = tags$p('2. Settings & Outputs : Modify the default settings and select the output type and values.'), collapsible = TRUE, collapsed = TRUE)
                                                 ),
                                                 fluidRow(
                                                   box(
                                                     column(12,plotlyOutput("rcm.sc.tas.pu",height = '500'))
                                                     ,
                                                     fluidRow(
                                                       column(12,
                                                              infoBoxOutput("figcaption.rcm.sc.tas.pu",width= '100%'))
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              infoBoxOutput("figTips.rcm.sc.tas.pu",width= '100%')
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              infoBoxOutput("figMoreTips.rcm.sc.tas.pu",width= '100%')
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              infoBoxOutput("figRemember.rcm.sc.tas.pu",width= '100%')
                                                       )
                                                     ),
                                                     title = '3. Seasonal Cycle of monthly mean air Temperature',
                                                     status = 'danger',solidHeader = TRUE,width = '100%',
                                                     collapsible = TRUE,collapsed = FALSE)
                                                 ),
                                                 fluidRow(
                                                   box(                                                   
                                                     column(12,
                                                            plotlyOutput("rcm.sc.pr.pu",width = '100%',height = '500'))
                                                     ,
                                                     fluidRow(
                                                       column(12,
                                                              infoBoxOutput("figcaption.rcm.sc.pr.pu",width= '100%'))
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              infoBoxOutput("figTips.rcm.sc.pr.pu",width= '100%')
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              infoBoxOutput("figMoreTips.rcm.sc.pr.pu",width= '100%')
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(12,
                                                              infoBoxOutput("figRemember.rcm.sc.pr.pu",width= '100%')
                                                       )
                                                     ),
                                                     title = '4. Seasonal Cycle of monthly precipitaiton totals',
                                                     status = 'danger',solidHeader = TRUE,width = '100%',
                                                     collapsible = TRUE,collapsed = TRUE)
                                                 )
                                               )
                                      )
                          )
                  ),
                  tabItem(tabName = "gcms",
		  	  tags$br(),tags$br(),
                          box(status = 'danger',solidHeader = TRUE, width = '100%',
                              tags$h4("Data Users | Global Climate Models")
                          ),
                          box(status = 'danger',solidHeader = TRUE,width = '100%',
                              tags$div(HTML('<p style="color:#871010;">TIPS | The global climate models constitute powerful tools for climate projection to provide the best representation of the projected climate signal over a region of interest. The climate simulations evaluated here are based on the Coordinated Regional Climate Downscaling Experiment over Europe (EURO-CORDEX) to produce the best estimates of regional/local climate signal that in turn can be used in impact studies. You can click on the dashboard to navigate between other items.</p>'))
                          ),
                          # tabsetPanel(id = 'gcms.tabs',type = "tabs",selected = "Seasonal Cycle",
                          #             tabPanel("Metadata", p(),
                          #                      fluidPage(
                          #                        tabsetPanel(id = 'gcm.meta.tabs',type = 'tabs', 
                          #                                    #tabPanel('Temperature',DT::dataTableOutput("gcm.meta.tas")),
                          #                                    #tabPanel('Precipitation',DT::dataTableOutput("gcm.meta.pr")),
                          #                                    tabPanel('All variables',DT::dataTableOutput("gcm.meta.all"))
                          #                                    
                          #                        ))
                          #             ),
                          #             tabPanel("Seasonal Cycle", p(),
                          fluidPage(
                            p(),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tags$figcaption('The regions used the AR5 Reference Regions which include 26 regions defined in SREX. 
                                       In addition to these regions, the Arctic, Antarctic, South Asia and South-East Asia) and 
                                       three global analysis domains: land only, sea only and all points. ',
                                                         tags$a(href = 'http://www.ipcc-data.org/guidelines/pages/ar5_regions.html',"Read more")),
                                         tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                         column(12,selectInput("gcm.region", label = NULL, 
                                                               choices = region.names,
                                                               selected = "Global",width = '100%')),
                                         leafletOutput('gcm.region',width = '100%',height = '500'),
                                         title = tags$p('1. Select a region : Explore and navigate through various regions (AR5 predefined regions)'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         fluidRow(
                                           column(4,selectInput("gcm.period", label = "Period", 
                                                                choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                            "Far Future (2071-2100)"),
                                                                selected = "Present",width = '100%')),
                                           column(8,br(),helpText('You can navigate between one control and two (near and far) future time horizons.'))),
                                         fluidRow(column(4, selectInput("gcm.chart.type", label = "Chart Output", 
                                                                        choices = c("Individual Simulations",
                                                                                    "Ensemble of All Simulations",
                                                                                    "Box Plots of All Simulations"),
                                                                        selected = "Ensemble of All Simulations",width = '100%')),
                                                  column(8,br(),helpText('You can modify the layout of the chart to display individual simulations or the envelope-based on all simulations.'))),
                                         fluidRow(column(4,  selectInput("gcm.sim.sc", label = "Simulations", 
                                                                         choices = c("All simulations","Selected simulations","Both (not yet implemented)"),
                                                                         selected = "All simulations",width = '100%')),
                                                  column(8,br(),helpText('You can filter the output to selected simulations in the meta data table or display all simulations (default).'))),
                                         fluidRow(column(4, selectInput("gcm.legend.sc", label = "Legend", 
                                                                        choices = c("Display","Hide"),
                                                                        selected = "Hide Legend",width = '100%')),
                                                  column(8,br(),helpText('You can display or hide the legend in the different charts'))),                                         
                                         fluidRow(column(4,  selectInput("gcm.groupBy", label = "Group By", choices = c('None','---',names(gcm.meta.tas)),
                                                                         selected = 'None',width = '100%')),
                                                  column(8,br(),helpText('You can group the simulations by values in the meta data table, for instance, by global climate model ID, i.e. all simulations sharing the same global climate model belong to the same group but different colors are applied for simulations within each group.'))),
                                         fluidRow(column(4,  selectInput("gcm.colorBy", label = "Color By", 
                                                                         choices = c('None','---','Group'),
                                                                         selected = 'None',width = '100%')),
                                                  column(8,br(),helpText('You can apply the same color within grouped simulations by values in the meta data table such as the global climate model ID. In this case, all simulations within each group will have same colored lines'))),
                                         fluidRow(column(4,  selectInput("gcm.outputValues", label = "Displayed values", 
                                                                         choices = c('Absolute','Anomaly','Bias','RMSE','Change'),
                                                                         selected = 'Absolute',width = '100%')),
                                                  column(8,
                                                         tags$style("#description {border: 2px solid #dd4b39;font-size: 18px;}"),
                                                         #textInput(inputId="description",label = '',
                                                         br(),helpText('You can transform the values into anomalies by subtracting the mean, compute the bias or the root mean square errors as deviations with regards to the reference data, or compute the climate change with regards to the base period 1981-2010'))),                                         
                                         fluidRow(column(4,  selectInput("gcm.stat", label = "Statistics", 
                                                                         choices = c('Mean','Standard Deviation','Spatial Correlation'))),
                                                  column(8,br(),
                                                         helpText('You can display values for various statistics such as the mean and the spatial standard deviation. The spatial  correlations are computed only between historical simulations and the reference data for the present (1981-2010).'))),                                         
                                         fluidRow(column(4,  selectInput("gcm.var", label = "Variables", 
                                                                         choices = c('Individual','Synchronised'),
                                                                         selected = 'Synchronised',width = '100%')),
                                                  column(8,br(),helpText('You can filter the simulations and keep only identical simulations for all climate variables such as precipitation and temperature.',width = '100%'))),
                                         title = tags$p('2. Settings & Outputs : Modify the default settings and select the output type and values.'), collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart",p(),
                                                    #box(tags$h4('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled air mean temperature by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5). You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation instead of the mean. Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'),width='100%',title = tags$figcaption('Info'),collapsible = TRUE, collapsed = TRUE,status = 'danger'),
                                                    plotlyOutput("gcm.sc.tas",height = '500'),p(),
                                                    column(12,infoBoxOutput("figcaption.gcm.sc.tas",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figTips.gcm.tas",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figMoreTips.gcm.tas",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figRemember.gcm.tas",width= '100%'))),
                                           tabPanel("Data",p(),column(12,infoBoxOutput("tabcaption",width= '100%')),
                                                    DT::dataTableOutput("gcm.sc.tas.data")),
                                           tabPanel('Metadata',DT::dataTableOutput("gcm.meta.tas"))),
                                         title = tags$p('3. Evaluate the seasonal cycle in simulated Mean Air Temperature'), 
                                         collapsible = TRUE, collapsed = FALSE))
                            ),fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    # tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                    #                 You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                    #                 You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation instead of the mean.
                                                    #                 Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'),
                                                    plotlyOutput("gcm.sc.pr",height = '500'),p(),
                                                    column(12,infoBoxOutput("figcaption.gcm.sc.pr",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figTips.gcm.pr",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figMoreTips.gcm.pr",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figRemember.gcm.pr",width= '100%'))),
                                           tabPanel("Data", DT::dataTableOutput("gcm.sc.pr.data")),
                                           tabPanel('Metadata',DT::dataTableOutput("gcm.meta.pr"))),
                                         title = tags$p('4. Evaluate the seasonal cycle in Simulated Monthly Precipitation totals'), 
                                         collapsible = TRUE, collapsed = TRUE))),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    # tags$figcaption('The interactive figure shows the scatter plot of annual means of simulated temperature and precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5). The orange star shows the multi-model ensemble means and the black star shows ERAINT values, respectively.
                                                    #                 You can modify the type of the displayed output from the "Settings & Outputs" parameters to select different future time period. The shaded rectangles show the range and 90% confidence interval from the ensemble model simulations, respectively.
                                                    #                 You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation instead of the mean. Please note that the spatial correlation only works for present (1981-2010) climate. 
                                                    #                 Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, select/deselect individual simulations, and download the plot as png.'),
                                                    plotlyOutput("gcm.scatter",height = '500'),p(),
                                                    column(12,infoBoxOutput("figcaption.gcm.scatter",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figTips.gcm.scatter",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figMoreTips.gcm.scatter",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figRemember.gcm.scatter",width= '100%'))),
                                           tabPanel("Data", DT::dataTableOutput("gcm.scatter.data"))),
                                         #tabPanel('Metadata',DT::dataTableOutput("gcm.meta.pr"))),
                                         title = tags$p('5. Scatter Plots of Simulated Climate Variables'), 
                                         collapsible = TRUE, collapsed = TRUE)))
                          )
                          #             ),
                          #             tabPanel("Scatter Plot", p(), 
                          #                      fluidPage(plotlyOutput("gcm.scatter",width = '100%',height = '500'),
                          #                                fluidRow(
                          #                                  column(3,selectInput("param7", label = "Element", 
                          #                                                       choices = c("Temperature","Wet-day freq.","Precip. intensity"),
                          #                                                       selected = "Temperature",width = '100%'))
                          #                                )
                          #                      )
                          #             )
                          #             #tabPanel("Distribution", p(), box(fluidPage(plotOutput("gcm.prob",width = '100%',height = '900')))),
                          #             #                     tabPanel("Taylor Diagram", p(), fluidPage(plotOutput("gcm.taylor"))),
                          #             #                     tabPanel("Scatter Plots", p() , 
                          #             #                              fluidPage(
                          #             #                                column(9,plotlyOutput("gcm.scatter",width = '100%',height = '900')),
                          #             #                                column(3,DT::dataTableOutput("tgcm"))),
                          #             #                              fluidRow(
                          #             #                                column(3,selectInput("season.cc",label = "Season",
                          #             #                                                     choices = c("Winter","Spring","Summer", "Autumn"),selected = "Winter",width = '100%')),
                          #             #                                column(3,selectInput("period.cc", label = "Future Periods", choices = c("Near Future (2021-2050)","Far Future (2071-2100)"),
                          #             #                                                     selected = "Near Future",width = "100%")),
                          #             #                                column(3,selectInput("rcp.cc", label = "RCP", choices = c("High","Intermediate","low"),
                          #             #                                                     selected = "Intermediate",width = "100%")),
                          #             #                                column(3,selectInput("ci", label = "Confidence Interval", choices = c("Display","Hide"),
                          #             #                                                     selected = "Hide",width = "100%"))))
                          # )
                  ),
                  tabItem(tabName = "rcms",
                          tags$br(),tags$br(),
			  box(status = 'danger',solidHeader = TRUE, width = '100%',
                              tags$h4('Data Users | Regional Climate Models')
                          ),
                          box(status = 'danger',solidHeader = TRUE,width = '100%',
                              tags$div(HTML('<p style="color:#871010;">TIPS | The regional climate model simulations constitute a better representation of regional climate outcomes than global climate outputs as they are run on higher spatial resolution, and include more local processes to provide the best representation of the climate signal over a region of interest. 
                    The climate simulations evaluated here are based on the Coordinated Regional Climate Downscaling Experiment over Europe (EURO-CORDEX) to produce the best estimates of regional/local climate signal that in turn can be used in impact studies. You can click on the dashboard to navigate between other items.</p>'))
                          ),
                          fluidPage(
                            p(),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         #tags$figcaption('The EURO-CORDEX domain',
                                         #                tags$a(href = 'http://cordex.org/domains/cordex-region-euro-cordex/',"Read more")),
                                         #tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                         column(12,selectInput("rcm.region", label = NULL, 
                                                               choices = regions.all,
                                                               selected = "Europe",width = '100%')),
                                         leafletOutput('rcm.region',width = '100%',height = 500),
                                         title = tags$p('1. Display the region (EURO-CORDEX, PRUDENCE, European countries)'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         fluidRow(
                                           column(4,selectInput("rcm.period", label = "Period", 
                                                                choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                            "Far Future (2071-2100)"),
                                                                selected = "Present",width = '100%')),
                                           column(8,br(),helpText('You can navigate between one control and two (near and far) future time horizons.'))
                                         ),
                                         fluidRow(
                                           column(4,selectInput("rcm.chart.type", label = "Chart Output", 
                                                                choices = c("Individual Simulations",
                                                                            "Ensemble of All Simulations",
                                                                            "Box Plots of All Simulations"),
                                                                selected = "Ensemble of All Simulations",width = '100%')),
                                           column(8,br(),helpText('You can modify the layout of the chart to display individual simulations or the envelope-based on all simulations.'))
                                         ),
                                         fluidRow(
                                           column(4,selectInput("rcm.sim.sc", label = "Simulations", 
                                                                choices = c("All simulations","Selected Simulations","Both (not yet implemented)"),
                                                                selected = "All simulations",width = '100%')),
                                           column(8,br(),helpText('You can filter the output to selected simulations in the meta data table or display all simulations (default).'))
                                         ),
                                         fluidRow(
                                           column(4,selectInput("rcm.legend.sc", label = "Legend", 
                                                                choices = c("Display","Hide"),
                                                                selected = "Hide Legend",width = '100%')),
                                           column(8,br(),helpText('You can display or hide the legend in the different charts'))),                             
                                         fluidRow(
                                           column(4,selectInput("rcm.groupBy", label = "Group By", choices = c('None','---',names(rcm.meta.tas)),
                                                                selected = 'None',width = '100%')),
                                           column(8,br(),helpText('You can group the simulations by values in the meta data table, for instance, by global climate model ID, i.e. all simulations sharing the same global climate model belong to the same group but different colors are applied for simulations within each group.'))
                                         ),
                                         fluidRow(
                                           column(4,selectInput("rcm.colorBy", label = "Color By", 
                                                                choices = c('None','---','Group'),
                                                                selected = 'None',width = '100%')),
                                           column(8,br(),helpText('You can apply the same color within groupped simulations by values in the meta data table such as the global climate model ID. In this case, all simulations within each group will have same colored lines'))
                                         ),
                                         fluidRow(
                                           column(4,selectInput("rcm.outputValues", label = "Displayed values", 
                                                                choices = c('Absolute','Anomaly','Bias','Change'),
                                                                selected = 'Absolute',width = '100%')),
                                           column(8,br(),helpText('You can apply the same color within groupped simulations by values in the meta data table such as the global climate model ID. In this case, all simulations within each group will have same colored lines'))),
                                         fluidRow(
                                           column(4,selectInput("rcm.stat", label = "Statistics", 
                                                                choices = c('Mean','Standard Deviation',
                                                                            'Spatial Correlation'))),
                                           column(8,br(),helpText('You can display values for various statistics such as the mean and the spatial standard deviation. The spatial  correlations are computed only between historical simulations and the reference data for the present (1981-2010).'))),  
                                         fluidRow(
                                           column(4,selectInput("rcm.var", label = "Variables", 
                                                                choices = c('Individual','Synchronised'),
                                                                selected = 'Synchronised',width = '100%')),
                                           column(8,br(),helpText('You can filter the simulations and keep only identical simulations for all climate variables such as precipitation and temperature.',width = '100%'))),
                                         title = tags$p('2. Settings & Outputs : Modify the default settings and select the output type and values.'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart",p(),
                                                    # tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled air mean temperature by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                    #                 You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                    #                 You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation (only works on present (1981-2010) climate)  instead of the mean.
                                                    #                 Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'),
                                                    plotlyOutput("rcm.sc.tas",height = '500'),p(),
                                                    column(12,infoBoxOutput("figcaption.rcm.sc.tas",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figTips.rcm.tas",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figMoreTips.rcm.tas",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figRemember.rcm.tas",width= '100%'))),
                                           tabPanel("Data",DT::dataTableOutput("rcm.sc.tas.data")),
                                           tabPanel('Metadata',DT::dataTableOutput("rcm.meta.tas"))),
                                         title = tags$p('3. Evaluate the seasonal cycle in simulated Mean Air Temperature'), 
                                         collapsible = TRUE, collapsed = FALSE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    # tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                    #                 You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                    #                 You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation (only works on present (1981-2010) climate)  instead of the mean.
                                                    #                 Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'),
                                                    plotlyOutput("rcm.sc.pr",height = '500'),p(),
                                                    column(12,infoBoxOutput("figcaption.rcm.sc.pr",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figTips.rcm.pr",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figMoreTips.rcm.pr",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figRemember.rcm.pr",width= '100%'))),
                                           tabPanel("Data", DT::dataTableOutput("rcm.sc.pr.data")),
                                           tabPanel('Metadata',DT::dataTableOutput("rcm.meta.pr"))),
                                         title = tags$p('4. Evaluate the seasonal cycle in Simulated Monthly Precipitation totals'), 
                                         collapsible = TRUE, collapsed = TRUE))),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    # tags$figcaption('The interactive figure shows the scatter plot of annual means of simulated temperature and precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5). The orange star shows the multi-model ensemble means and the black star shows ERAINT values, respectively.
                                                    #               You can modify the type of the displayed output from the "Settings & Outputs" parameters to select different future time period. The shaded rectangles show the range and 90% confidence interval from the ensemble model simulations, respectively.
                                                    #               You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlations (only works on present (1981-2010) climate) instead of the mean.
                                                    #               Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, select/deselect individual simulations, and download the plot as png.'),
                                                    plotlyOutput("rcm.scatter",height = '500'),p(),
                                                    column(12,infoBoxOutput("figcaption.rcm.scatter",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figTips.rcm.scatter",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figMoreTips.rcm.scatter",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figRemember.rcm.scatter",width= '100%'))),
                                           tabPanel("Data", DT::dataTableOutput("rcm.scatter.data"))),
                                         #tabPanel('Metadata',DT::dataTableOutput("gcm.meta.pr"))),
                                         title = tags$p('5. Scatter Plots of Simulated Climate Variables'), 
                                         collapsible = TRUE, collapsed = TRUE)))
                          )
                  ),
                  tabItem(tabName = "score5", 
                          tags$br(),tags$br(),
			  box(status = 'danger',solidHeader = TRUE, width = '100%',
                              'Here, you can',
                              tags$ol(
                                tags$li("select a region to navigate through various predefined regions (EURO-CORDEX, PRUDENCE, European countries)"), 
                                tags$li("modify the default settings and select the output type (e.g. chart, boxplot) and values (e.g. bias, change)"), 
                                tags$li("evalutate future changes in monthly mean air Temperature statistics"),
                                tags$li("evaluate future changes in monthly precipitaiton totals statistics")),
                              title = 'Product Users | Changes in Climate',collapsible = TRUE,collapsed = FALSE
                          ),
                          box(status = 'danger',solidHeader = TRUE,width = '100%',
                              tags$div(HTML('<p style="color:#871010;">TIPS | Changes in Climate can be obtained using various climate models such as global climate models, regional climate models, and empirical statistical climate models which constitute powerful tools to provide the best representation of the projected climate signal over a region of interest. 
                    The climate simulations evaluated here are based on the CMIP5 global climate models, these are in turn used to force regional climate models (Coordinated Regional Climate Downscaling Experiment) and Empirical-Statistical Models (ESD) to produce the best estimates of regional/local climate signal that in turn can be used in impact studies. You can click on the dashboard to navigate between other items.</p>'))
                          ),
                          fluidPage(
                            fluidRow(
                              box(collapsible = TRUE,collapsed = FALSE,
                                  column(12,
                                         tabsetPanel(id = 'cc.tabs',type = "tabs", selected = 'Temperature',
                                                     tabPanel("Settings", p(), 
                                                              fluidRow(
                                                                column(4,selectInput("gcm.cc.region", label = 'Region', 
                                                                                     choices = region.names,
                                                                                     selected = "Global",width = '100%')),
                                                                column(8,br(),helpText('You can navigate between various AR5 predefined regions.'))
                                                              ),
                                                              fluidRow(
                                                                column(4,selectInput("gcm.cc.period", label = "Period", 
                                                                                     choices = c("Near Future (2021-2050)",
                                                                                                 "Far Future (2071-2100)"),
                                                                                     selected = "Near Future (2021-2050)",width = '100%')),
                                                                column(8,br(),helpText('You can navigate between one control and two (near and far) future time horizons.'))
                                                              ),
                                                              fluidRow(
                                                                column(4,selectInput("gcm.cc.chart.type", label = "Chart Output", 
                                                                                     choices = c("Individual Simulations",
                                                                                                 "Ensemble of All Simulations",
                                                                                                 "Box Plots of All Simulations"),
                                                                                     selected = "Ensemble of All Simulations",width = '100%')),
                                                                column(8,br(),helpText('You can modify the layout of the chart to display individual simulations or the envelope-based on all simulations.'))
                                                              ),
                                                              fluidRow(
                                                                column(4,selectInput("gcm.cc.stat", label = "Statistics", 
                                                                                     choices = c('Mean','Standard Deviation',
                                                                                                 'Spatial Correlation'))),
                                                                column(8,br(),helpText('You can display values for various statistics such as the mean and the spatial standard deviation. The spatial  correlations are computed only between historical simulations and the reference data for the present (1981-2010).'))),  
                                                              fluidRow(
                                                                column(4,selectInput("gcm.cc.var", label = "Variables", 
                                                                                     choices = c('Individual','Synchronised'),
                                                                                     selected = 'Synchronised',width = '100%')),
                                                                column(8,br(),helpText('You can filter the simulations and keep only identical simulations for all climate variables such as precipitation and temperature.',width = '100%')))   
                                                     ),
                                                     tabPanel("Temperature", p(),
                                                              plotlyOutput("gcm.cc.tas.pu",width='100%',height = '500'),
                                                              column(12,infoBoxOutput("figcaption.gcm.tas.cc",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figTips.gcm.tas.cc",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figMoreTips.gcm.tas.cc",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figRemember.gcm.tas.cc",width= '100%'))
                                                     ),
                                                     tabPanel("Precipitation", p(),
                                                              plotlyOutput("gcm.cc.pr.pu",height = '500'),
                                                              column(12,infoBoxOutput("figcaption.gcm.pr.cc",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figTips.gcm.pr.cc",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figMoreTips.gcm.pr.cc",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figRemember.gcm.pr.cc",width= '100%'))
                                                     ),
                                                     tabPanel("Precipitation vs Temperature", p(),
                                                              plotlyOutput("gcm.cc.scatter.pu",height = '500px'),
                                                              column(12,infoBoxOutput("figcaption.gcm.cc.scatter",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figTips.gcm.cc.scatter",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figMoreTips.gcm.cc.scatter",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figRemember.gcm.cc",width= '100%'))
                                                     )
                                                     #tabPanel("Scatter", p(),
                                                     #         plotlyOutput("gcm.cc.scatter.pu",height = '500')
                                                     #)
                                         )
                                  ),
                                  title = 'CMIP5 Global Climate Model Simulations',status = 'danger',solidHeader = TRUE,width = '100%')
                            ),
                            fluidRow(
                              box(collapsible = TRUE,collapsed = FALSE,
                                  column(12,
                                         tabsetPanel(id = 'rcm.cc.tabs',type = "tabs", selected = 'Temperature',
                                                     tabPanel("Settings", p(), 
                                                              fluidRow(
                                                                column(4,selectInput("rcm.cc.region", label = 'Region', 
                                                                                     choices = regions.all,
                                                                                     selected = "Europe",width = '100%')),
                                                                column(8,br(),helpText('You can navigate between various PRUDENCE predefined regions (! not yet implemented).'))
                                                              ),
                                                              fluidRow(
                                                                column(4,selectInput("rcm.cc.period", label = "Period", 
                                                                                     choices = c("Near Future (2021-2050)",
                                                                                                 "Far Future (2071-2100)"),
                                                                                     selected = "Near Future (2021-2050)",width = '100%')),
                                                                column(8,br(),helpText('You can navigate between one control and two (near and far) future time horizons.'))
                                                              ),
                                                              fluidRow(
                                                                column(4,selectInput("rcm.cc.chart.type", label = "Chart Output", 
                                                                                     choices = c("Individual Simulations",
                                                                                                 "Ensemble of All Simulations",
                                                                                                 "Box Plots of All Simulations"),
                                                                                     selected = "Ensemble of All Simulations",width = '100%')),
                                                                column(8,br(),helpText('You can modify the layout of the chart to display individual simulations or the envelope-based on all simulations.'))
                                                              ),
                                                              fluidRow(
                                                                column(4,selectInput("rcm.cc.stat", label = "Statistics", 
                                                                                     choices = c('Mean','Standard Deviation',
                                                                                                 'Spatial Correlation'))),
                                                                column(8,br(),helpText('You can display values for various statistics such as the mean and the spatial standard deviation. The spatial  correlations are computed only between historical simulations and the reference data for the present (1981-2010).'))),  
                                                              fluidRow(
                                                                column(4,selectInput("rcm.cc.var", label = "Variables", 
                                                                                     choices = c('Individual','Synchronised'),
                                                                                     selected = 'Synchronised',width = '100%')),
                                                                column(8,br(),helpText('You can filter the simulations and keep only identical simulations for all climate variables such as precipitation and temperature.',width = '100%')))   
                                                     ),
                                                     tabPanel("Temperature", p(), 
                                                              plotlyOutput("rcm.cc.tas.pu",height = '500'),
                                                              column(12,infoBoxOutput("figcaption.rcm.tas.cc",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figTips.rcm.tas.cc",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figMoreTips.rcm.tas.cc",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figRemember.rcm.tas.cc",width= '100%'))
                                                     ),
                                                     tabPanel("Precipitation", p(),
                                                              plotlyOutput("rcm.cc.pr.pu",height = '500'),
                                                              column(12,infoBoxOutput("figcaption.rcm.pr.cc",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figTips.rcm.pr.cc",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figMoreTips.rcm.pr.cc",width= '100%')),p(),
                                                              column(12,infoBoxOutput("figRemember.rcm.pr.cc",width= '100%'))
                                                     ),
                                                     tabPanel("Precipitation vs Temperature", p(),
                                                              plotlyOutput("rcm.cc.scatter.pu",height = '500'),
                                                              box(collapsible = TRUE, collapsed=TRUE, width = '100%', status = 'warning',title = 'Figure Details',
                                                                  column(12,infoBoxOutput("figcaption.rcm.cc.scatter",width= '100%')),p(),
                                                                  column(12,infoBoxOutput("figTips.rcm.cc.scatter",width= '100%')),p(),
                                                                  column(12,infoBoxOutput("figMoreTips.rcm.cc.scatter",width= '100%')),p(),
                                                                  column(12,infoBoxOutput("figRemember.rcm.cc.scatter",width= '100%'))
                                                              )
                                                     )
                                         )
                                  ),
                                  title = 'EURO-CORDEX Regional Climate model Simulations',status = 'danger',solidHeader = TRUE,width = '100%')
                            )
                          )
                  ),
                  tabItem(tabName = 'spi',
                          fluidPage(
                            p(),
                            fluidRow(
                              box(status = 'danger',solidHeader = TRUE, width = '100%',
                                  'Here, you can',
                                  tags$ol(
                                    tags$li("Modify the default settings and select the GCM/RCM simulation"), 
                                    tags$li("Evalutate statistics of the Standardized Precipitation Index")),
                                  title = 'Sectoral Communication | Standardized Precipitation Index',collapsible = TRUE,collapsed = FALSE
                              )
                            ),
                            fluidRow(
                              box(width = '100%', solidHeader = TRUE, status = 'danger',
                                  selectInput("spi.period", label = "Period", 
                                              choices = c("1981-2010 (Present)",
                                                          "2021-2050 (Near Future)",
                                                          "2071-2100 (Far future)"),
                                              selected = "1981-2010 (Present)",width = '100%'),
                                  selectInput("spi.sim", label = "Simulation", 
                                              choices = rcm.names,
                                              selected = rcm.names[1],width = '100%'),
                                  selectInput("spi.group", label = "SPI Category", 
                                              choices = c('ED - Extremely Dry',
                                                          'MD - Moderately Dry',
                                                          'SD - Severe Dry',
                                                          'NN - Normal',
                                                          'MW - Moderately Wet',
                                                          'VW - Very wet',
                                                          'EW - Extremely Wet'),
                                              selected = 'ED - Extremely Dry',width = '100%'),
                                  selectInput("spi.freq", label = "Frequency (months)", 
                                              choices = c('1','3','6','12'),
                                              selected = '12',width = '100%'),
                                  selectInput("spi.stat", label = "Statistic", 
                                              choices = c('nEvents','nMonths','meanEventLength'),
                                              selected = 'nEvents'),
                              title = '1. Settings & Outputs : Modify the default settings and select the output type and values.', 
                              collapsible = TRUE, collapsed = TRUE)
                          ),
                          fluidRow(
                                   box(width = '100%', solidHeader = TRUE, status = 'danger',
                                       'Your settings are the following:',
                                       tags$hr(),
                                       textOutput('spi.settings'),
                                       tags$hr(),
                                       leafletOutput("map.spi",height = '900'),
                                       title = tags$p('2. Evaluate the Standardized Precipitaiton Index (SPI) over Europe'),
                                       collapsible = TRUE, collapsed = FALSE))
                  )
                ),
                tabItem(tabName = "hydro",
                        fluidPage(
                          p(),
                          fluidRow(
                            column(12,
                                   box(width = '100%', solidHeader = TRUE, status = 'danger',
                                       tags$figcaption('The EURO-CORDEX domain',
                                                         tags$a(href = 'http://cordex.org/domains/cordex-region-euro-cordex/',"Read more")),
                                         tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                         column(12,selectInput("rcm.region", label = NULL, 
                                                               choices = regions.all,
                                                               selected = "Europe",width = '100%')),
                                         #leafletOutput('rcm.region',width = '100%',height = 500),
                                         title = tags$p('1. Select a region (e.g. EURO-CORDEX definition)'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         selectInput("rcm.period", label = "Period", 
                                                     choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                 "Far Future (2071-2100)"),
                                                     selected = "Present",width = '100%'),
                                         selectInput("rcm.chart.type", label = "Chart Output", 
                                                     choices = c("Individual Simulations",
                                                                 "Ensemble of All Simulations",
                                                                 "Box Plots of All Simulations"),
                                                     selected = "Ensemble of All Simulations",width = '100%'),
                                         selectInput("rcm.sim.sc", label = "Simulations", 
                                                     choices = c("All simulations","Selected Simulations","Both (not yet implemented)"),
                                                     selected = "All simulations",width = '100%'),
                                         selectInput("rcm.legend.sc", label = "Legend", 
                                                     choices = c("Display","Hide"),
                                                     selected = "Hide Legend",width = '100%'),
                                         selectInput("rcm.groupBy", label = "Group By", choices = c('None','---',names(rcm.meta.tas)),
                                                     selected = 'None',width = '100%'),
                                         selectInput("rcm.colorBy", label = "Color By", 
                                                     choices = c('None','---','Group'),
                                                     selected = 'None',width = '100%'),
                                         selectInput("rcm.outputValues", label = "Displayed values", 
                                                     choices = c('Absolute','Anomaly','Bias','Change'),
                                                     selected = 'Absolute',width = '100%'),
                                         selectInput("rcm.stat", label = "Statistics", 
                                                     choices = c('Mean','Standard Deviation',
                                                                 'Correlation')),
                                         selectInput("rcm.var", label = "Variables", 
                                                     choices = c('Individual','Synchronised'),
                                                     selected = 'Synchronised',width = '100%'),
                                         title = tags$p('2. Settings & Outputs : Modify the default settings and select the output type and values.'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Projected Precipitaiton",p(),
                                                    # tags$figcaption('The interactive figure shows the projected precipitation simulated by the selected set of simulations assuming the intermediate emission scenario (RCP4.5).
                                                    #                 You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                    #                 You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                    #                 Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'),
                                                    plotlyOutput("hydro.sc.pr",height = '900'))
                                         ),
                                         title = tags$p('3. Water Resources'),
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Projected temperature",p(),
                                                    # tags$figcaption('The interactive figure shows the projected temperature simulated by the selected set of simulations assuming the intermediate emission scenario (RCP4.5).
                                                    #                 You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                    #                 You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                    #                 Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'),
                                                    plotlyOutput("hydro.sc.tas",height = '900'))
                                         ),
                                         title = tags$p('4. Drought'), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps of flood risk and curves showing return values.", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('4. River Runoff'), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            )
                          )
                          
                  ),
                  tabItem(tabName = "agriculture",
                          fluidPage(
                            p(),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         #tags$figcaption('The EURO-CORDEX domain',
                                         #                tags$a(href = 'http://cordex.org/domains/cordex-region-euro-cordex/',"Read more")),
                                         #tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                         column(12,selectInput("rcm.region", label = NULL, 
                                                               choices = regions.all,
                                                               selected = "Europe",width = '100%')),
                                         #leafletOutput('rcm.region',width = '100%',height = 500),
                                         title = tags$p('1. Select your region of interest'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         selectInput("rcm.period", label = "Period", 
                                                     choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                 "Far Future (2071-2100)"),
                                                     selected = "Present",width = '100%'),
                                         selectInput("rcm.chart.type", label = "Chart Output", 
                                                     choices = c("Individual Simulations",
                                                                 "Ensemble of All Simulations",
                                                                 "Box Plots of All Simulations"),
                                                     selected = "Ensemble of All Simulations",width = '100%'),
                                         selectInput("rcm.sim.sc", label = "Simulations", 
                                                     choices = c("All simulations","Selected Simulations","Both (not yet implemented)"),
                                                     selected = "All simulations",width = '100%'),
                                         selectInput("rcm.legend.sc", label = "Legend", 
                                                     choices = c("Display","Hide"),
                                                     selected = "Hide Legend",width = '100%'),
                                         selectInput("rcm.groupBy", label = "Group By", choices = c('None','---',names(rcm.meta.tas)),
                                                     selected = 'None',width = '100%'),
                                         selectInput("rcm.colorBy", label = "Color By", 
                                                     choices = c('None','---','Group'),
                                                     selected = 'None',width = '100%'),
                                         selectInput("rcm.outputValues", label = "Displayed values", 
                                                     choices = c('Absolute','Anomaly','Bias','Change'),
                                                     selected = 'Absolute',width = '100%'),
                                         selectInput("rcm.stat", label = "Statistics", 
                                                     choices = c('Mean','Standard Deviation',
                                                                 'Correlation')),
                                         selectInput("rcm.var", label = "Variables", 
                                                     choices = c('Individual','Synchronised'),
                                                     selected = 'Synchronised',width = '100%'),
                                         title = tags$p('2. Settings & Outputs : Modify the default settings and select the output type and values.'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart",p(),
                                                    tags$figcaption('The interactive figure shows the Intensity-Duration-Frequency curve simulated by the selected set of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))),
                                         title = tags$p('3. Growing season'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps", p(), 
=======
                                           tabPanel("Maps of flood risk and curves showing return values.", p(), 
>>>>>>> 86776234eb307d683fb1c1c7c7ea51dccd63b6f0
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('6. Drought'), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps of flood risk and curves showing return values.", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('7. Hail'), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps of flood risk and curves showing return values.", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('6. Drought'), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps of flood risk and curves showing return values.", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('7. Hail'), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps of flood risk and curves showing return values.", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
=======
>>>>>>> 86776234eb307d683fb1c1c7c7ea51dccd63b6f0
                                         title = tags$p('8. Flood'), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps of flood risk and curves showing return values.", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('9. Sunlight'), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            )
                          )
                  ),
                  tabItem(tabName = "tourism",
                          fluidPage(
                            p(),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         #tags$figcaption('The EURO-CORDEX domain',
                                         #                tags$a(href = 'http://cordex.org/domains/cordex-region-euro-cordex/',"Read more")),
                                         #tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                         column(12,selectInput("rcm.region", label = NULL, 
                                                               choices = regions.all,
                                                               selected = "Europe",width = '100%')),
                                         #leafletOutput('rcm.region',width = '100%',height = 500),
                                         title = tags$p('1. Select your region of interest'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         selectInput("rcm.period", label = "Period", 
                                                     choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                 "Far Future (2071-2100)"),
                                                     selected = "Present",width = '100%'),
                                         selectInput("rcm.chart.type", label = "Chart Output", 
                                                     choices = c("Individual Simulations",
                                                                 "Ensemble of All Simulations",
                                                                 "Box Plots of All Simulations"),
                                                     selected = "Ensemble of All Simulations",width = '100%'),
                                         selectInput("rcm.sim.sc", label = "Simulations", 
                                                     choices = c("All simulations","Selected Simulations","Both (not yet implemented)"),
                                                     selected = "All simulations",width = '100%'),
                                         selectInput("rcm.legend.sc", label = "Legend", 
                                                     choices = c("Display","Hide"),
                                                     selected = "Hide Legend",width = '100%'),
                                         selectInput("rcm.groupBy", label = "Group By", choices = c('None','---',names(rcm.meta.tas)),
                                                     selected = 'None',width = '100%'),
                                         selectInput("rcm.colorBy", label = "Color By", 
                                                     choices = c('None','---','Group'),
                                                     selected = 'None',width = '100%'),
                                         selectInput("rcm.outputValues", label = "Displayed values", 
                                                     choices = c('Absolute','Anomaly','Bias','Change'),
                                                     selected = 'Absolute',width = '100%'),
                                         selectInput("rcm.stat", label = "Statistics", 
                                                     choices = c('Mean','Standard Deviation',
                                                                 'Correlation')),
                                         selectInput("rcm.var", label = "Variables", 
                                                     choices = c('Individual','Synchronised'),
                                                     selected = 'Synchronised',width = '100%'),
                                         title = tags$p('2. Settings & Outputs : Modify the default settings and select the output type and values.'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart",p(),
                                                    tags$figcaption('The interactive figure shows the Intensity-Duration-Frequency curve simulated by the selected set of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))),
                                         title = tags$p('3. Temperature'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('4. Precipitation '), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps of flood risk and curves showing return values.", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('5. Snow'), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps of flood risk and curves showing return values.", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('6. Wind'), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            )
                          )
                  ),
                  tabItem(tabName = "insurance",
                          fluidPage(
                            p(),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         #tags$figcaption('The EURO-CORDEX domain',
                                         #                tags$a(href = 'http://cordex.org/domains/cordex-region-euro-cordex/',"Read more")),
                                         #tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                         column(12,selectInput("rcm.region", label = NULL, 
                                                               choices = regions.all,
                                                               selected = "Europe",width = '100%')),
                                         #leafletOutput('rcm.region',width = '100%',height = 500),
                                         title = tags$p('1. Select your region of interest'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         selectInput("rcm.period", label = "Period", 
                                                     choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                 "Far Future (2071-2100)"),
                                                     selected = "Present",width = '100%'),
                                         selectInput("rcm.chart.type", label = "Chart Output", 
                                                     choices = c("Individual Simulations",
                                                                 "Ensemble of All Simulations",
                                                                 "Box Plots of All Simulations"),
                                                     selected = "Ensemble of All Simulations",width = '100%'),
                                         selectInput("rcm.sim.sc", label = "Simulations", 
                                                     choices = c("All simulations","Selected Simulations","Both (not yet implemented)"),
                                                     selected = "All simulations",width = '100%'),
                                         selectInput("rcm.legend.sc", label = "Legend", 
                                                     choices = c("Display","Hide"),
                                                     selected = "Hide Legend",width = '100%'),
                                         selectInput("rcm.groupBy", label = "Group By", choices = c('None','---',names(rcm.meta.tas)),
                                                     selected = 'None',width = '100%'),
                                         selectInput("rcm.colorBy", label = "Color By", 
                                                     choices = c('None','---','Group'),
                                                     selected = 'None',width = '100%'),
                                         selectInput("rcm.outputValues", label = "Displayed values", 
                                                     choices = c('Absolute','Anomaly','Bias','Change'),
                                                     selected = 'Absolute',width = '100%'),
                                         selectInput("rcm.stat", label = "Statistics", 
                                                     choices = c('Mean','Standard Deviation',
                                                                 'Correlation')),
                                         selectInput("rcm.var", label = "Variables", 
                                                     choices = c('Individual','Synchronised'),
                                                     selected = 'Synchronised',width = '100%'),
                                         title = tags$p('2. Settings & Outputs : Modify the default settings and select the output type and values.'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart",p(),
                                                    tags$figcaption('The interactive figure shows the Intensity-Duration-Frequency curve simulated by the selected set of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))),
                                         title = tags$p('3. Floods'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('4. Storms '), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            )
                            
                            
                          )
                  ),
                  tabItem(tabName = "health",
                          fluidPage(
                            p(),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tags$figcaption('Meteorological elements such as temperature, wind, precipitation, and humidity may have some health issues (Epstein and Ferber, 2011), affecting conditions such as heat waves, pollution, pollen, flooding, storm surge, wind, snow, ice, or droughts. Heat waves pose a health hazards for an aging population in southern Europe, whereas higher temperatures in general can increase the spread of diseases and their vectors (eg. ticks and lyme disease). Changing temperatures also affect spread pollen affecting people with allergies, and temperature inversions can trap pollution (warmer winters may potentially reduce the frequency of inversions). Flooding and storm surge can lead to drowning, while strong winds can result in dangerous situations with flying debris. The presence of snow and ice can also lead to accidents, the former through shifting snow (causing cardiac arrest in older people) or avalanches, while icy condition can result in higher number of broken limbs. Droughts can lead to dangerous conditions with higher risks of wildfires.',
                                                         'A typical user from the health sector may include planners within health authorities (ministry of health) and hospitals. To prepare outlooks for future demands, they need to look at various factors, and climate change may not necessarily be the most important one. Furthermore, the time horizon for their planning is usually shorter than a decade, unless there is a need to start a research project to develop new treatments or build new hospitals. Reliable seasonal to decadal predictions can benefit annual budgeting and planning, typically the time frame for society.',
                                                         p(), tags$a(href = 'http://cordex.org/domains/cordex-region-euro-cordex/', p(), "Read more")),
                                         tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                         column(12,checkboxGroupInput(inputId = "typical.user", label = 'Typical user' ,
                                                                      choices = c('Health authorities','Hospitals'),
                                                                      selected = 'Health authorities',width = '100%')),
                                         #leafletOutput('rcm.region',width = '100%',height = 500),
                                         title = tags$p("About KPI's for the Health Sector"), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         selectInput("rcm.period", label = "Period", 
                                                     choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                 "Far Future (2071-2100)"),
                                                     selected = "Present",width = '100%'),
                                         selectInput("rcm.chart.type", label = "Chart Output", 
                                                     choices = c("Individual Simulations",
                                                                 "Ensemble of All Simulations",
                                                                 "Box Plots of All Simulations"),
                                                     selected = "Ensemble of All Simulations",width = '100%'),
                                         selectInput("rcm.sim.sc", label = "Simulations", 
                                                     choices = c("All simulations","Selected Simulations","Both (not yet implemented)"),
                                                     selected = "All simulations",width = '100%'),
                                         selectInput("rcm.legend.sc", label = "Legend", 
                                                     choices = c("Display","Hide"),
                                                     selected = "Hide Legend",width = '100%'),
                                         selectInput("rcm.groupBy", label = "Group By", choices = c('None','---',names(rcm.meta.tas)),
                                                     selected = 'None',width = '100%'),
                                         selectInput("rcm.colorBy", label = "Color By", 
                                                     choices = c('None','---','Group'),
                                                     selected = 'None',width = '100%'),
                                         selectInput("rcm.outputValues", label = "Displayed values", 
                                                     choices = c('Absolute','Anomaly','Bias','Change'),
                                                     selected = 'Absolute',width = '100%'),
                                         selectInput("rcm.stat", label = "Statistics", 
                                                     choices = c('Mean','Standard Deviation',
                                                                 'Spatial Correlation')),
                                         selectInput("rcm.var", label = "Variables", 
                                                     choices = c('Individual','Synchronised'),
                                                     selected = 'Synchronised',width = '100%'),
                                         title = tags$p('1. Settings & Outputs : Modify the default settings and select the output type and values.'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart",p(),
                                                    tags$figcaption('The interactive figure shows the Intensity-Duration-Frequency curve simulated by the selected set of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation (only works on present (1981-2010) climate) instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))),
                                         title = tags$p('2. Heat waves and temperature changes'), 
                                         collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation (only works on present (1981-2010) climate) instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation (only works on present (1981-2010) climate)  instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('3. Pollution '), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation (only works on present (1981-2010) climate)  instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation (only works on present (1981-2010) climate)  instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('4. Floods and Droughts '), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation (only works on present (1981-2010) climate)  instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                  You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                  You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation (only works on present (1981-2010) climate)  instead of the mean.
                                                  Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('5. Storms and Storm Surge '), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                                    You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                                    You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation (only works on present (1981-2010) climate)  instead of the mean.
                                                                    Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                                    You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                                    You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and spatial correlation (only works on present (1981-2010) climate)  instead of the mean.
                                                                    Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('6. Snow and Ice '), 
                                         collapsible = TRUE, collapsed = TRUE)
                              )
                            )
                            
                            
                          )
                  )
                  
                )
              )
)    
