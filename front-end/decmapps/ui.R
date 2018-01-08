# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution

library(shinydashboard)

dashboardPage(skin = 'red',
              dashboardHeader(title = 'Data Evaluation for Climate Models (DECM)', titleWidth = '600px',
                              dropdownMenu(
                                type = "messages", 
                                badgeStatus =  "success",
                                messageItem(from = 'Welcome to DECM Prototype tool', message = "Climate Data Web Site", icon = icon("file"),
                                            href = "https://climatedatasite.net/"),
                                messageItem(from = 'Shiny', icon = icon("file"), message = 'More help!',
                                            href = "https://rstudio.github.io/shinydashboard/"),
                                messageItem(from = 'server', icon = icon("shopping-cart", lib = "glyphicon"),
                                            message = "No model selected ")
                              ),
                              dropdownMenuOutput('messageMenu')),
              # ,
              # dropdownMenu(type = "messages", .list = msgs),
              # dropdownMenu(type = "tasks",.list = tasks)),
              dashboardSidebar(collapsed = FALSE,
                               sidebarMenu(
                                 menuItem("KPIs for Product Users", tabName = 'pu',startExpanded = TRUE,
                                          menuSubItem("Climate Models", tabName = "score1"), #  href = 'https://esdlab.met.no/modelexplorer/'
                                          menuSubItem("Biases", tabName = "score1"),
                                          menuSubItem("Spread or Uncertainty", tabName = "score2"),
                                          menuSubItem("Mean Annual Cycle", tabName = "score3"),
                                          menuSubItem("Individual Model", tabName = "score4"),
                                          menuSubItem("Climate Change", tabName = "score5"),
                                          menuSubItem("Storm tracks", tabName = "score6"),
                                          menuSubItem("Individual Location", tabName = "score7"),
                                          menuSubItem("Comparator", tabName = "score8")),
                                 menuItem("KPIs for Data Users", tabName = 'pu',startExpanded = TRUE,
                                          menuSubItem("Global Climate Models", tabName = "gcms",selected = TRUE),
                                          menuSubItem("Regioanl Climate Models", tabName = "rcms"),
                                          menuSubItem("Reanalysis", tabName = "rea"),
                                          menuSubItem("Weather Stations", tabName = "stations"),
                                          menuSubItem("Satellite Data",  tabName = "sat"), # href = 'https://esdlab.met.no/gcmeval/'
                                          menuSubItem("Storm tracks", tabName = "score26")),
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
                  tabItem(tabName = "gcms",
                          tabsetPanel(id = 'gcms.tabs',type = "tabs",
                                      tabPanel("Metadata", p(),
                                            tabsetPanel(id = 'meta.tabs',type = 'tabs', 
						tabPanel('Temperature',DT::dataTableOutput("gcm.meta.tas")),
						tabPanel('Precipitation',DT::dataTableOutput("gcm.meta.pr"))

					    )
				      ),
                                      #                     tabPanel("Map", p(), leafletOutput("gcm.map",width = '100%',height = '900'),
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
                                      
                                      # tabPanel("Region", p(), 
                                      #          selectInput("region",label = "SREX Region",choices = region.names),
                                      #          leafletOutput("region",width = '100%',height = '700')
                                      #          
                                      # ),
                                      tabPanel("Seasonal Cycle", p(), 
                                               fluidPage(
                                                 fluidRow(
                                                   column(2,selectInput("region", label = "Region", 
                                                                        choices = region.names,
                                                                        selected = "Global",width = '100%')),
                                                   column(2,selectInput("period", label = "Period", 
                                                                        choices = c("Present (1981-2010)","Near Future (2021-2050)",
                                                                                    "Far Future (2071-2100)"),
                                                                        selected = "Present",width = '100%')),
                                                   column(2,selectInput("type", label = "Chart Output", 
                                                                        choices = c("Individual Models",
                                                                                    "Ensemble of All Models","Ensemble of Selected Models",
                                                                                    "Box Plots of All Models","Box Plots of Selected Models"),
                                                                        selected = "Ensemble",width = '100%')),
                                                   column(2,selectInput("legend.sc", label = "Chart Legend", 
                                                                        choices = c("Display All",
                                                                                    "Display Selected Models Only",
                                                                                    "Hide All"),
                                                                        selected = "Display Models",width = '100%')),
                                                   column(2,selectInput("groupBy", label = "Group By", 
                                                                        choices = c('None','---',names(gcm.meta.tas)),
                                                                        selected = 'None',width = '100%'))
                                                 ),
                                                 fluidRow(
                                                   column(12,
                                                          box(width = '100%', solidHeader = TRUE, status = 'info',
                                                              leafletOutput('region',width = '100%',height = 850),
                                                              title = tags$html('Show Region'), 
                                                              collapsible = TRUE, collapsed = TRUE))
                                                 ),
                                                 fluidRow(
                                                   column(6,
                                                          tabsetPanel(
                                                            tabPanel("Chart",p(),tags$h5('Seasonal cycle of simulated and observed (black) temperature.'),
                                                                     plotlyOutput("gcm.sc.tas",width = '100%',height = '800')),
                                                            tabPanel("Data",
                                                                     DT::dataTableOutput("gcm.sc.tas.data")))
                                                   ),
                                                   column(6,
                                                          tabsetPanel(
                                                            tabPanel("Chart", p(), tags$h5('Seasonal cycle of simulated and observed (black) precipitation.'),
                                                                     plotlyOutput("gcm.sc.pr",width = '100%',height = '800')),
                                                            tabPanel("Data", DT::dataTableOutput("gcm.sc.pr.data")))
                                                   ))
                                               )
                                      ),
                                      tabPanel("Scatter Plot", p(), 
                                               fluidPage(plotlyOutput("gcm.scatter",width = '100%',height = '700'),
                                                         fluidRow(
                                                           column(3,selectInput("param7", label = "Element", 
                                                                                choices = c("Temperature","Wet-day freq.","Precip. intensity"),
                                                                                selected = "Temperature",width = '100%'))
                                                         )
                                               )
                                      )
                                      #tabPanel("Distribution", p(), box(fluidPage(plotOutput("gcm.prob",width = '100%',height = '900')))),
                                      #                     tabPanel("Taylor Diagram", p(), fluidPage(plotOutput("gcm.taylor"))),
                                      #                     tabPanel("Scatter Plots", p() , 
                                      #                              fluidPage(
                                      #                                column(9,plotlyOutput("gcm.scatter",width = '100%',height = '900')),
                                      #                                column(3,DT::dataTableOutput("tgcm"))),
                                      #                              fluidRow(
                                      #                                column(3,selectInput("season.cc",label = "Season",
                                      #                                                     choices = c("Winter","Spring","Summer", "Autumn"),selected = "Winter",width = '100%')),
                                      #                                column(3,selectInput("period.cc", label = "Future Periods", choices = c("Near Future (2021-2050)","Far Future (2071-2100)"),
                                      #                                                     selected = "Near Future",width = "100%")),
                                      #                                column(3,selectInput("rcp.cc", label = "RCP", choices = c("High","Intermediate","low"),
                                      #                                                     selected = "Intermediate",width = "100%")),
                                      #                                column(3,selectInput("ci", label = "Confidence Interval", choices = c("Display","Hide"),
                                      #                                                     selected = "Hide",width = "100%"))))
                          )
                  ),
                  tabItem(tabName = "rcms",
                          tabsetPanel(id = 'rcms.tabs',type = "tabs",
                                      tabPanel("Metadata", p() , DT::dataTableOutput("rcm.meta"))
                                      #                     tabPanel("Map", p(), leafletOutput("rcm.map",width = '100%',height = '900'),
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
                                      #                     tabPanel("Chart", p(), fluidPage(plotlyOutput("rcm.chart",width = '100%',height = '900')),
                                      #                              fluidRow(
                                      #                                column(3,selectInput("param7", label = "Element", choices = c("Temperature", # "Precip. sum",
                                      #                                                                                              "Wet-day freq.","Precip. intensity"),
                                      #                                                     selected = "Temperature",width = '100%')),
                                      #                                column(3,selectInput("loc7",label = "Location", choices = t2m.locs, 
                                      #                                                     selected = t2m.locs[1],width = '100%')),
                                      #                                column(2,checkboxGroupInput(inputId = 'selim',label = "Display Model",
                                      #                                                            choices = gcmnames.45,
                                      #                                                            selected = NULL,width = '100%')),
                                      #                                column(3,selectInput("season7",label = "Season",
                                      #                                                     choices = c("Annual (All seasons)","Winter (DJF)","Spring (MAM)",
                                      #                                                                 "Summer (JJA)", "Autumn (SON)"), 
                                      #                                                     selected = "Winter (DJF)",width = '100%')),
                                      #                                column(2, tags$b('Smoother') ,checkboxInput("loess", "Fit a loess function", FALSE))),
                                      #                              fluidRow(
                                      #                                column(12, dataTableOutput('table.table')))),
                                      #                     tabPanel("Distribution", p(), box(fluidPage(plotOutput("rcm.dist",width = '100%',height = '900')))),
                                      #                     tabPanel("Taylor Diagram", p(), fluidPage(plotOutput("rcm.taylor"))),
                                      #                     tabPanel("Scatter Plots", p() , 
                                      #                              fluidPage(
                                      #                                column(9,plotlyOutput("rcm.scatter",width = '100%',height = '900')),
                                      #                                column(3,DT::dataTableOutput("tgcm"))),
                                      #                              fluidRow(
                                      #                                column(3,selectInput("season.cc",label = "Season",
                                      #                                                     choices = c("Winter","Spring","Summer", "Autumn"),selected = "Winter",width = '100%')),
                                      #                                column(3,selectInput("period.cc", label = "Future Periods", choices = c("Near Future (2021-2050)","Far Future (2071-2100)"),
                                      #                                                     selected = "Near Future",width = "100%")),
                                      #                                column(3,selectInput("rcp.cc", label = "RCP", choices = c("High","Intermediate","low"),
                                      #                                                     selected = "Intermediate",width = "100%")),
                                      #                                column(3,selectInput("ci", label = "Confidence Interval", choices = c("Display","Hide"),
                                      #                                                     selected = "Hide",width = "100%"))))
                          )),
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
                                      tabPanel("Plot", p(), fluidPage(plotlyOutput("plot.cc",width = '100%',height = '900')),
                                               fluidRow(
                                                 column(3,selectInput("param7", label = "Element", choices = c("Temperature", # "Precip. sum",
                                                                                                               "Wet-day freq.","Precip. intensity"),
                                                                      selected = "Temperature",width = '100%')),
                                                 column(3,selectInput("loc7",label = "Location", choices = t2m.locs, 
                                                                      selected = t2m.locs[1],width = '100%')),
                                                 column(2,checkboxGroupInput(inputId = 'selim',label = "Display Model",
                                                                             choices = gcmnames.45,
                                                                             selected = NULL,width = '100%')),
                                                 column(3,selectInput("season7",label = "Season",
                                                                      choices = c("Annual (All seasons)","Winter (DJF)","Spring (MAM)",
                                                                                  "Summer (JJA)", "Autumn (SON)"), 
                                                                      selected = "Winter (DJF)",width = '100%')),
                                                 column(2, tags$b('Smoother') ,checkboxInput("loess", "Fit a loess function", FALSE))),
                                               fluidRow(
                                                 column(12, dataTableOutput('gcm.table')))),
                                      tabPanel("Summary", p(),verbatimTextOutput("summary.cc")),
                                      tabPanel("Table", p(),tableOutput("table.cc")),
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

