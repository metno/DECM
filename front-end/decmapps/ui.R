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
                                          menuSubItem("Changes in Climate", tabName = "score5"),
                                          menuSubItem("Standardized Precipitation Index over Europe", tabName = "spi"),
                                          menuSubItem("Models' Spread (Cf. External App.)", href = 'https://esdlab.met.no/gcmeval/')),
                                 menuItem("Data Users", tabName = 'pu',startExpanded = TRUE,
                                          menuSubItem("Global Climate Models", tabName = "gcms"),
                                          menuSubItem("Regional Climate Models", tabName = "rcms"),
                                          menuSubItem("Models' Ranks (Cf. External App.)", href = 'https://esdlab.met.no/gcmeval/')
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
                                  collapsible = TRUE, collapsed = TRUE)
                            ),
                            fluidRow(
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
                                  title = tags$p('2. Settings & Outputs : Modify the default settings and select the output type and values.'), collapsible = TRUE, collapsed = TRUE)
                            ),
                            fluidRow(
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
                                  collapsible = TRUE, collapsed = FALSE)
                            ),
                            fluidRow(
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
                                  collapsible = TRUE, collapsed = TRUE)),
                            fluidRow(
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
                                  collapsible = TRUE, collapsed = TRUE))
                          )
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
                                    tags$li("Evaluate statistics of the Standardized Precipitation Index (SPI) over the European domain"),
				    tags$li("Evaluate statistics of the Standardized Precipitation and Evaporation Index (SPEI) over the European domain")),
                                  title = 'Sectoral Communication | Standardized Precipitation Index',collapsible = TRUE,collapsed = FALSE
                              )
                            ),
			    fluidRow(
                              box(status = 'danger',solidHeader = FALSE, width = '100%',collapsible=FALSE,
                                  tags$div(HTML('<p style="color:#871010;">TIPS | Standardized Precipitation Index (SPI) is used as an illustration of a derived product in the sector-specific examples</p>
                                   <details style="color:#871010;"><summary>Read More</summary>
(e.g. Hayes et al. 1999). SPI characterizes the deficit (or abundance) of water at different temporal scales with respect to normal conditions in a reference period. SPI is flexible, easy to interpret and simple to calculate, as it uses only monthly precipitation as input. The main limitations of SPI is its incapability to take other meteorological factors which affect the water balance such as evapotranspiration into account when estimating water availability. Due to this deficiency, SPI has also been criticised for being insensitive to temperature changes. Notwithstanding these limitation, SPI is widely used in drought monitoring at different temporal scales and its use is also supported by the World Meteorological Organization (WMO).
SPI is calculated from monthly precipitation time series as follows. First, a parametric (typically gamma or Pearson III -type) distribution is fitted to the monthly data. In the example data, gamma distribution has been used. To take precipitation seasonality into account, separate distributions are fitted at each month. Using the fitted parameters, cumulative probabilities are calculated for each accumulated precipitation value. In the second step, the obtained cumulative distribution values are normalised such that they follow Gaussian distribution, which enables the comparison between different locations. In other words, SPI is simply the distance from the (zero) median measured in normalised standard deviations. 

When raw monthly values are used, the SPI lacks “memory” in the sense that previous precipitation conditions are not taken into account in the calculation of SPI. As different hydrological aspects (soil moisture vs.  runoff) respond at different time scales to the lack or surplus of water, monthly SPI is often unsuitable for drought characterization. Depending on the application, SPI is usually calculated after accumulating precipitation over certain (moving) time window (3-month, 6-month etc.). Here, SPI has been estimated using 1, 3, 6 and 12-month time window.
the main criticism against using SPI in climatic studies is that it does not take changes in temperature and its effect to water availability (e.g. through evapotranspiration) directly into account when assessing drought conditions. Due to this it has been argued that SPI might not be suitable for estimating the effect of climate change to drought occurrence. To take this limitation better into account, Vicente-Serrano et al. (2010) developed an alternative index, SPEI, which does not suffer from this limitation. SPEI is estimated from water deficit (or surplus) measured as the difference between monthly precipitation and potential evapotranspiration. SPEI is not limited to any particular formulation of potential evapotranspiration and different formulas can be used based on the available input data. In the following calculations, the formulation presented by Hargreaves (1994), which is based on monthly minimum and maximum temperature, is used. In the original paper, log-logistic distribution was used to model the water deficit and the same statistical model is also used here. As in the case of SPI, the obtained cumulative probabilities are finally transformed to follow normal distribution, i.e., the interpretation of SPEI is similar to SPI.

<h1> Calculation of SPI and SPEI statistics </h1>

The calculation of both SPI and SPEI has been made with the SPEI package available for R. First, the described distributions are fitted to data in the reference period (1981-2010). The obtained distribution parameters are then used to derive SPI and SPEI values for each land grid box in the European region. The obtained time series are then used to evaluate changes in the occurrence and spatial extent of drought (as measured by SPI and SPEI) with respect to 1981-2010, assuming that future precipitation and precipitation deficit come from the same distribution. The following statistics are calculated from these time series to facilitate the visualisation of changes in SPI and SPEI for different GCM-RCMs and drought categories:

nMonths = Number of months belonging to a specific drought category 
nEvents = Number of events belonging to a certain drought category
meanEventLength = Mean length of events belonging to a certain category

These statistics are calculated for each of the drought categories described above and in addition also to different combinations of categories (e.g. moderately dry + severely dry 0 extremely dry (MD+SD+ED)).

The category naming follows Table 2 and the statistics have been calculated for 1-, 3-, 6- and 12-month time windows, using grid points at which the land fraction is greater than 50%.

<h1> Identified issues with SPI and SPEI </h1>

Both SPI and SPEI have been estimated using only one distribution for each (gamma for SPI and log-normal for SPEI) and might not be optimal for all aggregation scales, different times of the year and geographical locations. In addition, the unbiased probability-weighted moments used to estimate the distribution for log-normal distribution seem to lead a slight underestimation of the spread of the distribution (Beguería et al. 2014). Both of these issues can be inferred from the reference period values as differing numbers of months belonging to a specific category than what would be expect after normalisation to normal distribution. In addition, the estimation of cumulative probabilities using gamma distribution in SPI gives infinite values in arid regions (mostly in Africa) when 1-month and 3-month aggregation is used.

<h1> References </h1>

<ul>
<li> Hayes, M.J., Svoboda, M.D., Wilhite, D.A. and Vanyarkho, O.V., 1999. Monitoring the 1996 drought using the standardized precipitation index. Bulletin of the American meteorological society, 80(3), pp.429-438. </li>

<li> Vicente-Serrano, S.M., Beguería, S. and López-Moreno, J.I., 2010. A multiscalar drought index sensitive to global warming: the standardized precipitation evapotranspiration index. Journal of climate, 23(7), pp.1696-1718. </li>

<li> Hargreaves, G.H., 1994. Defining and using reference evapotranspiration. Journal of Irrigation and Drainage Engineering, 120(6), pp.1132-1139. </li>

<li> Beguería, S., Vicente‐Serrano, S.M., Reig, F. and Latorre, B., 2014. Standardized precipitation evapotranspiration index (SPEI) revisited: parameter fitting, evapotranspiration models, tools, datasets and drought monitoring. International Journal of Climatology, 34(10), pp.3001-3023. </li> 
</ul> </details>')))),


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
                                  title = tags$p('2. Evaluate the SPI - Standardized Precipitaiton Index - over Europe'),
                                  collapsible = TRUE, collapsed = FALSE)),
                            fluidRow(
                              box(width = '100%', solidHeader = TRUE, status = 'danger',
                                  'Your settings are the following:',
                                  tags$hr(),
                                  textOutput('spei.settings'),
                                  tags$hr(),
                                  leafletOutput("map.spei",height = '900'),
                                  title = tags$p('3. Evaluate the SPEI - Standardized Precipitaiton and Evaporation Index - over Europe'),
                                  collapsible = TRUE, collapsed = TRUE))
                          )
                  )
                  
                  
                )
              )
              
)

