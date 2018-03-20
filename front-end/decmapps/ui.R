# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution

library(shinydashboard)


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
                                 menuItem("KPIs for Product Users", tabName = 'pu',startExpanded = TRUE,
                                          menuSubItem("Climate Models", href = 'https://esdlab.met.no/modelexplorer/'),
                                          menuSubItem("Biases", href = 'https://esdlab.met.no/gcmeval/'),
                                          menuSubItem("Spread or Uncertainty", href = 'https://esdlab.met.no/gcmeval/'),
                                          menuSubItem("Mean Annual Cycle", tabName = "score3"),
                                          menuSubItem("test iframe", tabName = "score1"),
                                          menuSubItem("Individual Model", tabName = "score4"),
                                          menuSubItem("Climate Change", tabName = "score5"),
                                          menuSubItem("Storm tracks", href = 'http://157.249.177.25:3838/Storms/'),
                                          menuSubItem("Individual Location", tabName = "score7"),
                                          menuSubItem("Comparator", tabName = "score8")),
                                 menuItem("KPIs for Data Users", tabName = 'pu',startExpanded = TRUE,
                                          menuSubItem("Global Climate Models", tabName = "gcms",selected = TRUE),
                                          menuSubItem("Regioanl Climate Models", tabName = "rcms"),
                                          menuSubItem("Reanalysis", tabName = "rea"),
                                          menuSubItem("Weather Stations", tabName = "stations"),
                                          menuSubItem("Satellite Data",  tabName = "sat"), # href = 'https://esdlab.met.no/gcmeval/'
                                          menuSubItem("Storm tracks", href = 'http://157.249.177.25:3838/Storms/')),
                                 menuItem("KPIs for Sectoral Communication", tabName = 'sc',startExpanded = TRUE,
                                          menuSubItem("Hydrology", tabName = "hydro"),
                                          menuSubItem("Energy", tabName = "score32"),
                                          menuSubItem("Agriculture", tabName = "score33"),
                                          menuSubItem("Forestry", tabName = "score34"),
                                          menuSubItem("Climate Change", tabName = "score35"),
                                          menuSubItem("Storm tracks", tabName = "score36")),
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
                  tabItem(tabName = "score1", htmlOutput("frame")),
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
                                         selectInput("gcm.period", label = "Period", 
                                                     choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                 "Far Future (2071-2100)"),
                                                     selected = "Present",width = '100%'),
                                         selectInput("gcm.chart.type", label = "Chart Output", 
                                                     choices = c("Individual Simulations",
                                                                 "Ensemble of All Simulations","Ensemble of Selected Simulations",
                                                                 "Box Plots of All Simulations","Box Plots of Selected Simulations"),
                                                     selected = "Ensemble of All Simulations",width = '100%'),
                                         selectInput("gcm.sim.sc", label = "Simulations", 
                                                     choices = c("All simulations","Selected simulations","Both (not yet implemented)"),
                                                     selected = "All simulations",width = '100%'),
                                         selectInput("gcm.legend.sc", label = "Legend", 
                                                     choices = c("Display","Hide"),
                                                     selected = "Hide Legend",width = '100%'),
                                         selectInput("gcm.groupBy", label = "Group By", choices = c('None','---',names(gcm.meta.tas)),
                                                     selected = 'None',width = '100%'),
                                         selectInput("gcm.colorBy", label = "Color By", 
                                                     choices = c('None','---','Group'),
                                                     selected = 'None',width = '100%'),
                                         selectInput("gcm.outputValues", label = "Displayed values", 
                                                     choices = c('Absolute','Anomaly','Bias','Change'),
                                                     selected = 'Absolute',width = '100%'),
                                         selectInput("gcm.stat", label = "Statistics", 
                                                     choices = c('Mean','Standard Deviation',
                                                                 'Correlation')),
                                         selectInput("gcm.var", label = "Variables", 
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
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled air mean temperature by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                                                 The user can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                                                 The user can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                                                 Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. The user can also check and download both the data and meta data tabs for furhter details about the simulations.'),
                                                    plotlyOutput("gcm.sc.tas",width = '100%',height = '600')),
                                           tabPanel("Data",DT::dataTableOutput("gcm.sc.tas.data")),
                                           tabPanel('Metadata',DT::dataTableOutput("gcm.meta.tas"))),
                                         title = tags$p('3. Evaluate the seasonal cycle in simulated Mean Air Temperature'), 
                                         collapsible = TRUE, collapsed = FALSE))
                            ),fluidRow(
                              column(12,
                                     box(width = '100%', solidHeader = TRUE, status = 'danger',
                                         tabsetPanel(
                                           tabPanel("Chart", p(), 
                                                    tags$figcaption('The interactive figure shows the seasonal cycle of pseudo-observed (dashed) and modeled precipitation by the multi-model ensemble of simulations assuming the intermediate emission scenario (RCP4.5).
                                                                                 The user can modify the type of the output from the "Settings & Outputs" tab box into, for example, individual simulations, envelope of the ensemble model simulations, box plots of both, transform the values into anomalies, group the models by attributes, etc. 
                                                                                 The user can additionally double click on specific climate models from the legend (once displayed) or the meta data table to isolate one or a group of simulations or modified the displyed statistic to, for example, spatial standard deviation and correlations instead of the mean.
                                                                                 Other options are also included such as zoom in/out, show closest data by pointing with the mouse on the simulations, compare data between simulations, and download the plot as png by taking a snapshot. The user can also check and download both the data and meta data tabs for furhter details about the simulations.'),
                                                    plotlyOutput("gcm.sc.pr",width = '100%',height = '600')),
                                           tabPanel("Data", DT::dataTableOutput("gcm.sc.pr.data")),
                                           tabPanel('Metadata',DT::dataTableOutput("gcm.meta.pr"))),
                                         title = tags$p('4. Evaluate the seasonal cycle in Simulated Monthly Precipitation totals'), 
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
                          tabsetPanel(id = 'rcms.tabs',type = "tabs",
                                      tabPanel("Metadata", p(),
                                               fluidPage(
                                                 tabsetPanel(id = 'rcm.meta.tabs',type = 'tabs', 
                                                             tabPanel('Temperature',DT::dataTableOutput("rcm.meta.tas")),
                                                             tabPanel('Precipitation',DT::dataTableOutput("rcm.meta.pr")),
                                                             tabPanel('All variables',DT::dataTableOutput("rcm.meta.all"))
                                                             
                                                 ))
                                      ),
                                      tabPanel("Seasonal Cycle", p(), 
                                               fluidPage(
                                                 fluidRow(
                                                   column(2,selectInput("rcm.region", label = "Region", 
                                                                        choices = 'Europe',
                                                                        selected = "Europe",width = '100%')),
                                                   column(2,selectInput("rcm.period", label = "Period", 
                                                                        choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                                    "Far Future (2071-2100)"),
                                                                        selected = "Present",width = '100%')),
                                                   column(2,selectInput("rcm.chart.type", label = "Chart Output", 
                                                                        choices = c("Individual Simulations",
                                                                                    "Ensemble of All Simulations","Ensemble of Selected Simulations",
                                                                                    "Box Plots of All Simulations","Box Plots of Selected Simulations"),
                                                                        selected = "Ensemble",width = '100%')),
                                                   column(2,selectInput("rcm.legend.sc", label = "Chart Legend", 
                                                                        choices = c("All simulations",
                                                                                    "Selected simulations",
                                                                                    "None"),
                                                                        selected = "All simulations",width = '100%')),
                                                   column(2,selectInput("rcm.groupBy", label = "Group By", 
                                                                        choices = c('None','---',names(rcm.meta.tas)),
                                                                        selected = 'None',width = '100%')),
                                                   column(2,selectInput("rcm.colorBy", label = "Color By", 
                                                                        choices = c('None','---','Group'),
                                                                        selected = 'None',width = '100%')),
                                                   column(2,selectInput("rcm.outputValues", label = "Displayed values", 
                                                                        choices = c('Absolute','Anomaly'),
                                                                        selected = 'Absolute',width = '100%')),
                                                   column(2,selectInput("rcm.stat", label = "Statistics", 
                                                                        choices = c('Mean','Standard Deviation',
                                                                                    'Correlation'),
                                                                        selected = 'Mean',width = '100%'))
                                                 ),
                                                 fluidRow(
                                                   column(12,
                                                          box(width = '100%', solidHeader = TRUE, status = 'info',
                                                              leafletOutput('rcm.region',width = '100%',height = 850),
                                                              title = tags$html('Show Region'), 
                                                              collapsible = TRUE, collapsed = TRUE))
                                                 ),
                                                 fluidRow(
                                                   column(6,
                                                          tabsetPanel(
                                                            tabPanel("Chart",p(),tags$h4('Seasonal cycle of simulated and observed (black) temperature.'),
                                                                     plotlyOutput("rcm.sc.tas",width = '100%',height = '800')),
                                                            tabPanel("Data",
                                                                     DT::dataTableOutput("rcm.sc.tas.data")))
                                                   ),
                                                   column(6,
                                                          tabsetPanel(
                                                            tabPanel("Chart", p(), tags$h4('Seasonal cycle of simulated and observed (black) precipitation.'),
                                                                     plotlyOutput("rcm.sc.pr",width = '100%',height = '800')),
                                                            tabPanel("Data", DT::dataTableOutput("rcm.sc.pr.data")))
                                                   ))
                                               )
                                      ),
                                      tabPanel("Scatter Plot", p(), 
                                               fluidPage(plotlyOutput("rcm.scatter",width = '100%',height = '700'),
                                                         fluidRow(
                                                           column(3,selectInput("rcm.param", label = "Element", 
                                                                                choices = c("Temperature","Wet-day freq.","Precip. intensity"),
                                                                                selected = "Temperature",width = '100%'))
                                                         )
                                               )
                                      )
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
                          )),
                  tabItem(tabName = "hydro", box("du-b"))
                )
              )
)


