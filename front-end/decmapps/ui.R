# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution

library(shinydashboard)
data("metaextract")
M <- data.frame(list(Project=meta$project_id,Experiment=meta$experiment_id,GCM=meta$gcm,
                     RIP=meta$gcm_rip, RCM=meta$rcm, VAR=meta$var, Unit=meta$unit, Resolution=paste(meta$resolution,"deg"),
                     Domain=paste(gsub(","," - ",meta$lon),"E"," / ",paste(gsub(","," - ",meta$lat)),"N",sep=""), 
                     Years=gsub(",","-",gsub("-[0-9]{2}","",meta$dates)), URL=meta$url))

dashboardPage(skin = 'red',
              dashboardHeader(title = 'Data Evaluation for Climate Models (DECM prototype)', titleWidth = '600px', 
                              #set height for header
                              tags$li(class = "dropdown",
                                      tags$style(".main-header {height: 60px}"),
                                      tags$style(".main-header .logo {height: 60px}")
                              ),
                              # tags$li(a(href = 'http://www.copernicus.eu/',
                              #           img(src = 'https://climatedatasite.net/wp-content/uploads/2018/02/copernicus-logo.png',
                              #               title = "Copernicus", height = "30px"),
                              #           title = "Copernicus website"),
                              #         class = "dropdown"),
                              # tags$li(a(href = 'https://climate.copernicus.eu/',
                              #           img(src = 'https://climatedatasite.net/wp-content/uploads/2018/02/c3s-logo.png',
                              #               title = "Climate Change Service", height = "30px"),
                              #           title = "Climate Change Service"),
                              #         class = "dropdown"),
                              tags$li(a(href = 'https://climate.copernicus.eu/data-evaluation-climate-models',
                                        img(src = 'https://climatedatasite.net/wp-content/uploads/2018/02/banner_c3s.png',
                                            title = "DECM website", height = "40px"),
                                        style = "padding-top:10px; padding-bottom:10px;"),
                                      class = "dropdown"),
                              dropdownMenu(
                                type = "messages", 
                                badgeStatus =  "success",
                                messageItem(from = 'Welcome to DECM Prototype tool', message = "Climate Data Web Site", icon = icon("file"),
                                            href = "https://climatedatasite.net/"),
                                messageItem(from = 'Go to the shiny app', icon = icon("file"), message = 'Click here!',
                                            href = "https://esdlab.met.no/decmapps/"),
                                messageItem(from = 'Shopping Box', icon = icon("shopping-cart", lib = "glyphicon"),
                                            message = "No model selected ")
                              ),
                              dropdownMenuOutput('messageMenu')),
              # ,
              # dropdownMenu(type = "messages", .list = msgs),
              # dropdownMenu(type = "tasks",.list = tasks)),
              dashboardSidebar(collapsed = TRUE,
                               sidebarMenu(
                                 menuItem("Product Users", tabName = 'pu',startExpanded = TRUE,
                                          menuSubItem("Explore the Simulations", tabName = "browse"),
                                          menuSubItem("Seasonal Cycle (not finished)", tabName = "seasonalCycle"),
                                          menuSubItem("Models' Biases (not finished)", tabName = "bias"),
                                          menuSubItem("Models' Spread or Uncertainty (not finished)", href = 'https://esdlab.met.no/gcmeval/'),
                                          #menuSubItem("test iframe", tabName = "score1"),
                                          #menuSubItem("Individual Model", tabName = "score4"),
                                          menuSubItem("Changes in Climate", tabName = "score5")),
                                 #menuSubItem("Storm tracks", href = 'http://157.249.177.25:3838/Storms/'),
                                 #menuSubItem("Individual Location", tabName = "score7"),
                                 #menuSubItem("Comparator", tabName = "score8")),
                                 menuItem("Data Users", tabName = 'pu',startExpanded = TRUE,
                                          menuSubItem("Global Climate Models", tabName = "gcms",selected = TRUE),
                                          menuSubItem("Regional Climate Models", tabName = "rcms"),
                                          menuSubItem("Models' Ranks", href = 'https://esdlab.met.no/gcmeval/')
                                          #menuSubItem("Reanalysis", tabName = "rea")
                                          #menuSubItem("Weather Stations", tabName = "stations"),
                                          #menuSubItem("Satellite Data",  tabName = "sat"), # href = 'https://esdlab.met.no/gcmeval/'
                                          #menuSubItem("Storm tracks", href = 'http://157.249.177.25:3838/Storms/')
                                 ),
                                 menuItem("Sectoral Communication", tabName = 'sc',startExpanded = FALSE,
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
                tags$head(HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-108282573-4'></script>
                               <script>
                               window.dataLayer = window.dataLayer || [];
                               function gtag(){dataLayer.push(arguments);}
                               gtag('js', new Date());
                               gtag('config', 'UA-108282573-4');
                               </script>"
                )),
                
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
                            fluidRow(
                              box(width = '100%',
                                  column(12, infoBoxOutput('simulation',width = '100%')),
                                  title = 'Read Me First',status = 'danger', solidHeader = TRUE, collapsible = TRUE, 
                                  collapsed = FALSE)
                            ),
                            fluidRow(
                              box(width = '100%',
                                  column(12,DT::dataTableOutput('glossary',width = '100%')),
                                  title = 'Glossary',status = 'danger', solidHeader = TRUE, collapsible = TRUE, 
                                  collapsed = TRUE)
                            ),
                            fluidRow(
                              box(width = '100%',
                                  column(12,
                                         selectInput("project",label = "Global Climate Model",
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
                  tabItem(tabName = "gcms",
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
                                         leafletOutput('gcm.region',width = '100%',height = 500),
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
                                                                                    #"Ensemble of Selected Simulations",
                                                                                    #"Both - Ensemble & Individual Simulations",
                                                                                    "Box Plots of All Simulations",
                                                                                    "Box Plots of Selected Simulations"),
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
                                                  column(8,br(),helpText('You can apply the same color within groupped simulations by values in the meta data table such as the global climate model ID. In this case, all simulations within each group will have same colored lines'))),
                                         fluidRow(column(4,  selectInput("gcm.outputValues", label = "Displayed values", 
                                                                         choices = c('Absolute','Anomaly','Bias','RMSE','Change'),
                                                                         selected = 'Absolute',width = '100%')),
                                                  column(8,
                                                         tags$style("#description {border: 2px solid #dd4b39;font-size: 18px;}"),
                                                         #textInput(inputId="description",label = '',
                                                         br(),helpText('You can transform the values into anomalies by substracting the mean, compute the bias or the root mean square errors as devitations with regards to the reference data, or compute the climate change with regards to the base period 1981-2010'))),                                         
                                         fluidRow(column(4,  selectInput("gcm.stat", label = "Statistics", 
                                                                         choices = c('Mean','Standard Deviation','Correlation'))),
                                                  column(8,br(),
                                                         helpText('You can display values for various statistics such as the mean, standard deviation or spatial correlation computed between simulations and the reference data.'))),                                         
                                         fluidRow(column(4,  selectInput("gcm.var", label = "Variables", 
                                                                         choices = c('Individual','Synchronised'),
                                                                         selected = 'Synchronised',width = '100%')),
                                                  column(8,br(),helpText('You can filter the simulations and keep only identical simulations for all climate variables such as preciptiation and temperaure.',width = '100%'))),
                                         title = tags$p('2. Settings & Outputs : Modify the default settings and select the output type and values.'), collapsible = TRUE, collapsed = TRUE))
                            ),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart",p(),
                                                    #box(tags$h4('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled air mean temperature by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5). You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean. Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'),width='100%',title = tags$figcaption('Info'),collapsible = TRUE, collapsed = TRUE,status = 'danger'),
                                                    plotlyOutput("gcm.sc.tas",width = '100%',height = '600'),p(),
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
                                                    #                 You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                    #                 Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'),
                                                    plotlyOutput("gcm.sc.pr",width = '100%',height = '600'),p(),
                                                    column(12,infoBoxOutput("figcaption.gcm.sc.pr",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figTips.gcm.pr",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figMoreTips.gcm.pr",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figRemember.gcm.pr",width= '100%'))),
                                           tabPanel("Data", DT::dataTableOutput("gcm.sc.pr.data")),
                                           tabPanel('Metadata',DT::dataTableOutput("gcm.meta.pr"))),
                                         title = tags$p('4. Evaluate the seasonal cycle in Simulated Monthly Precipitation totals'), 
                                         collapsible = TRUE, collapsed = FALSE))),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    # tags$figcaption('The interactive figure shows the scatter plot of annual means of simulated temperature and precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5). The orange star shows the multi-model ensemble means and the black star shows ERAINT values, respectively.
                                                    #                 You can modify the type of the displayed output from the "Settings & Outputs" parameters to select different future time period. The shaded rectangles show the range and 90% confidence interval from the ensemble model simulations, respectively.
                                                    #                 You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                    #                 Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, select/deselect individual simulations, and download the plot as png.'),
                                                    plotlyOutput("gcm.scatter",width = '100%',height = '600'),p(),
                                                    column(12,infoBoxOutput("figcaption.gcm.scatter",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figTips.gcm.scatter",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figMoreTips.gcm.scatter",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figRemember.gcm.scatter",width= '100%'))),
                                           tabPanel("Data", DT::dataTableOutput("gcm.scatter.data"))),
                                         #tabPanel('Metadata',DT::dataTableOutput("gcm.meta.pr"))),
                                         title = tags$p('5. Scatter Plots of Simulated Climate Variables'), 
                                         collapsible = TRUE, collapsed = FALSE)))
                          )
                          #             ),
                          #             tabPanel("Scatter Plot", p(), 
                          #                      fluidPage(plotlyOutput("gcm.scatter",width = '100%',height = '600'),
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
                          # tabsetPanel(id = 'rcms.tabs',type = "tabs",selected = "Seasonal Cycle",
                          #             tabPanel("Metadata", p(),
                          #                      fluidPage(
                          #                        tabsetPanel(id = 'rcm.meta.tabs',type = 'tabs', 
                          #                                    #tabPanel('Temperature',DT::dataTableOutput("rcm.meta.tas")),
                          #                                    #tabPanel('Precipitation',DT::dataTableOutput("rcm.meta.pr")),
                          #                                    tabPanel('All variables',DT::dataTableOutput("rcm.meta.all"))
                          #                                    
                          #                        ))
                          #             ),
                          #             tabPanel("Seasonal Cycle", p(),
                          fluidPage(
                            p(),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tags$figcaption('The EURO-CORDEX domain',
                                                         tags$a(href = 'http://cordex.org/domains/cordex-region-euro-cordex/',"Read more")),
                                         tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                         column(12,selectInput("rcm.region", label = NULL, 
                                                               choices = 'Europe',
                                                               selected = "Europe",width = '100%')),
                                         leafletOutput('rcm.region',width = '100%',height = 500),
                                         title = tags$p('1. Display the region (EURO-CORDEX definition)'), 
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
                                                                            "Ensemble of All Simulations","Ensemble of Selected Simulations",
                                                                            "Box Plots of All Simulations","Box Plots of Selected Simulations"),
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
                                                                            'Correlation'))),
                                           column(8,br(),helpText('You can display values for various statistics such as the mean, standard deviation or spatial correlation computed between simulations and the reference data.'))),  
                                         fluidRow(
                                           column(4,selectInput("rcm.var", label = "Variables", 
                                                                choices = c('Individual','Synchronised'),
                                                                selected = 'Synchronised',width = '100%')),
                                           column(8,br(),helpText('You can filter the simulations and keep only identical simulations for all climate variables such as preciptiation and temperaure.',width = '100%'))),
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
                                                    #                 You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                    #                 Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'),
                                                    plotlyOutput("rcm.sc.tas",width = '100%',height = '600'),p(),
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
                                                    #                 You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                    #                 Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'),
                                                    plotlyOutput("rcm.sc.pr",width = '100%',height = '600'),p(),
                                                    column(12,infoBoxOutput("figcaption.rcm.sc.pr",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figTips.rcm.pr",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figMoreTips.rcm.pr",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figRemember.rcm.pr",width= '100%'))),
                                           tabPanel("Data", DT::dataTableOutput("rcm.sc.pr.data")),
                                           tabPanel('Metadata',DT::dataTableOutput("rcm.meta.pr"))),
                                         title = tags$p('4. Evaluate the seasonal cycle in Simulated Monthly Precipitation totals'), 
                                         collapsible = TRUE, collapsed = FALSE))),
                            fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    # tags$figcaption('The interactive figure shows the scatter plot of annual means of simulated temperature and precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5). The orange star shows the multi-model ensemble means and the black star shows ERAINT values, respectively.
                                                    #               You can modify the type of the displayed output from the "Settings & Outputs" parameters to select different future time period. The shaded rectangles show the range and 90% confidence interval from the ensemble model simulations, respectively.
                                                    #               You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                    #               Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, select/deselect individual simulations, and download the plot as png.'),
                                                    plotlyOutput("rcm.scatter",width = '100%',height = '600'),p(),
                                                    column(12,infoBoxOutput("figcaption.rcm.scatter",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figTips.rcm.scatter",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figMoreTips.rcm.scatter",width= '100%')),p(),
                                                    column(12,infoBoxOutput("figRemember.rcm.scatter",width= '100%'))),
                                           tabPanel("Data", DT::dataTableOutput("rcm.scatter.data"))),
                                         #tabPanel('Metadata',DT::dataTableOutput("gcm.meta.pr"))),
                                         title = tags$p('5. Scatter Plots of Simulated Climate Variables'), 
                                         collapsible = TRUE, collapsed = FALSE)))
                          )
                  ),
                  tabItem(tabName = "stations",
                          tabsetPanel(id = 'sta.tabs',type = "tabs",
                                      tabPanel("Metadata", p(),DT::dataTableOutput("station.meta")),
                                      tabPanel("Map", p(), leafletOutput("station.map",width = '100%',height = '900')),
                                      #                              fluidRow(
                                      #                                column(3,selectInput("rcp7",label = "Scenario", choices = c("Intermediate emissions (RCP4.5)",
                                      #                                                                                            "Low emissions (RCP2.6)", 
                                      #                                                                                            "High emissions (RCP8.5)"),
                                      #                                                     selected = "Intermediate emissions (RCP4.5)",width = '100%')),
                                      #                                column(3,sliderInput("lon7",label = "Longitudes",
                                      #                                                     min = 0, max = 30, value = c(0, 30),width = '100%')),
                                      #                                column(3, sliderInput("lat7",label = "Latitudes",
                                      #                                                      min = 55, max = 72, value = c(55, 72))),
                                      #                                column(2,selectInput(inputId = 'im',label = "Model",
                                      #                                                     choices = c('Ens. Mean','------',gcmnames.26),
                                      #                                                     selected = 'Ens. Mean',width = '100%')))),
                                      tabPanel("Chart", p(), plotlyOutput("station.ts",width = '100%',height = '900')),
                                      tabPanel("Data", p(), DT::dataTableOutput('station.data'))
                          )
                  ),
                  tabItem(tabName = "score5", 
                          fluidPage(
                            fluidRow(
                              box(collapsible = TRUE,collapsed = FALSE,
                                  column(12,'text'),
                                  title = 'GCM simulations',status = 'danger',solidHeader = TRUE,width = '100%')
                            ),
                            fluidRow(
                              box(collapsible = TRUE,collapsed = FALSE,
                                  column(12,'text'),
                                  title = 'RCM simulations',status = 'danger',solidHeader = TRUE,width = '100%')
                            ),
                            fluidRow(
                              box(collapsible = TRUE,collapsed = TRUE,
                                  column(12,
                                         tabsetPanel(id = 'score5.tabs',type = "tabs",
                                                     tabPanel("Map", p(), leafletOutput("map.cc",width = '100%',height = '900'),
                                                              fluidRow(
                                                                column(3,selectInput("rcp7",label = "Scenario", choices = c("Intermediate emissions (RCP4.5)",
                                                                                                                            "Low emissions (RCP2.6)", 
                                                                                                                            "High emissions (RCP8.5)"),
                                                                                     selected = "Intermediate emissions (RCP4.5)",width = '100%')),
                                                                column(3,sliderInput("lon7",label = "Longitudes",
                                                                                     min = 0, max = 30, value = c(0, 30),width = '100%')),
                                                                column(3, sliderInput("lat7",label = "Latitudes",
                                                                                      min = 55, max = 72, value = c(55, 72))),
                                                                column(2,selectInput(inputId = 'im',label = "Model",
                                                                                     choices = c('Ens. Mean','------',gcmnames.26),
                                                                                     selected = 'Ens. Mean',width = '100%')))),
                                                     tabPanel("Plot", p(), fluidPage(plotlyOutput("plot.cc",width = '100%',height = '800')),
                                                              fluidRow(
                                                                column(3,selectInput("param7", label = "Element", choices = c("Temperature", # "Precip. sum",
                                                                                                                              "Wet-day freq.","Precip. intensity"),
                                                                                     selected = "Temperature",width = '100%')),
                                                                column(3,selectInput("loc7",label = "Location", choices = t2m.locs, 
                                                                                     selected = t2m.locs[1],width = '100%')),
                                                                # column(2,checkboxGroupInput(inputId = 'selim',label = "Display Model",
                                                                #                             choices = gcmnames.45,
                                                                #                             selected = NULL,width = '100%')),
                                                                column(3,selectInput("season7",label = "Season",
                                                                                     choices = c("Annual (All seasons)","Winter (DJF)","Spring (MAM)",
                                                                                                 "Summer (JJA)", "Autumn (SON)"), 
                                                                                     selected = "Winter (DJF)",width = '100%')),
                                                                column(2, tags$b('Smoother') ,checkboxInput("loess", "Fit a loess function", TRUE))),
                                                              fluidRow(
                                                                column(12, DT::dataTableOutput('gcm.table')))),
                                                     tabPanel("Summary", p(),verbatimTextOutput("summary.cc")),
                                                     tabPanel("Table", p(),DT::dataTableOutput("table.cc")),
                                                     tabPanel("Distribution", p(), box(fluidPage(plotOutput("prob.cc",width = '100%',height = '900')))),
                                                     tabPanel("Taylor Diagram", p(), fluidPage(plotOutput("taylor.cc"))),
                                                     tabPanel("Scatter Plots", p() , 
                                                              fluidPage(
                                                                column(9,plotlyOutput("scatter.cc",width = '100%',height = '900')),
                                                                column(3,DT::dataTableOutput("tgcm"))),
                                                              #column(3,checkboxGroupInput(inputId = 'im.cc',label = "Model",
                                                              #                             choices =  c('Ens. Mean','------',levels(factor(model.45)),
                                                              #                            selected = 'Ens. Mean',width = '100%'))),
                                                              fluidRow(
                                                                column(3,selectInput("season.cc",label = "Season",
                                                                                     choices = c("Winter","Spring","Summer", "Autumn"),selected = "Winter",width = '100%')),
                                                                column(3,selectInput("period.cc", label = "Future Periods", choices = c("Near Future (2021-2050)","Far Future (2071-2100)"),
                                                                                     selected = "Near Future",width = "100%")),
                                                                column(3,selectInput("rcp.cc", label = "RCP", choices = c("High","Intermediate","low"),
                                                                                     selected = "Intermediate",width = "100%")),
                                                                column(3,selectInput("ci", label = "Confidence Interval", choices = c("Display","Hide"),
                                                                                     selected = "Hide",width = "100%"))))
                                         )),title = 'ESD simulations on station level',status = 'danger',solidHeader = TRUE,width = '100%')
                            )
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
                                                               choices = 'Europe',
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
                                                                 "Ensemble of All Simulations","Ensemble of Selected Simulations",
                                                                 "Box Plots of All Simulations","Box Plots of Selected Simulations"),
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
                                                    plotlyOutput("hydro.sc.pr",width = '100%',height = '900'))
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
                                                    plotlyOutput("hydro.sc.tas",width = '100%',height = '900'))
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
                                         tags$figcaption('The EURO-CORDEX domain',
                                                         tags$a(href = 'http://cordex.org/domains/cordex-region-euro-cordex/',"Read more")),
                                         tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                         column(12,selectInput("rcm.region", label = NULL, 
                                                               choices = 'Europe',
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
                                                                 "Ensemble of All Simulations","Ensemble of Selected Simulations",
                                                                 "Box Plots of All Simulations","Box Plots of Selected Simulations"),
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
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                                    You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                                    You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                                    Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.'))
                                         ),
                                         title = tags$p('4. Frost '), 
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
                                         title = tags$p('5. Rain during harvest'), 
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
                                         tags$figcaption('The EURO-CORDEX domain',
                                                         tags$a(href = 'http://cordex.org/domains/cordex-region-euro-cordex/',"Read more")),
                                         tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                         column(12,selectInput("rcm.region", label = NULL, 
                                                               choices = 'Europe',
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
                                                                 "Ensemble of All Simulations","Ensemble of Selected Simulations",
                                                                 "Box Plots of All Simulations","Box Plots of Selected Simulations"),
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
                                         tags$figcaption('The EURO-CORDEX domain',
                                                         tags$a(href = 'http://cordex.org/domains/cordex-region-euro-cordex/',"Read more")),
                                         tags$a(href = 'https://figshare.com/s/7b678c0f92c43f8e0aeb',"Demo video"),
                                         column(12,selectInput("rcm.region", label = NULL, 
                                                               choices = 'Europe',
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
                                                                 "Ensemble of All Simulations","Ensemble of Selected Simulations",
                                                                 "Box Plots of All Simulations","Box Plots of Selected Simulations"),
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
                                         column(12,checkboxGroupInput(inputId = "rcm.region", label = 'Typical user' ,
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
                                                                 "Ensemble of All Simulations","Ensemble of Selected Simulations",
                                                                 "Box Plots of All Simulations","Box Plots of Selected Simulations"),
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
                                                                    You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
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
                                                                    You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                                    Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                                    You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                                    You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
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
                                                                    You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                                    Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                                    You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                                    You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
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
                                                                    You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                                    Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                                    You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                                    You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
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
                                                                            You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                                            Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. You can also check and download both the data and meta data tabs for furhter details about the simulations.')),
                                           tabPanel("Maps", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                                    You can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                                    You can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
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

