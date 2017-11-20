# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution

library(shinydashboard)

dashboardPage(skin = 'red',
              dashboardHeader(title = 'Data Evaluation for Climate Models (DECM)', titleWidth = '600px'),
              dashboardSidebar(collapsed = FALSE,
                               sidebarMenu(
                                 menuItem("KPIs for Product Users", tabName = 'pu',startExpanded = TRUE,
                                          menuSubItem("Biases", tabName = "score1"),
                                          menuSubItem("Spread or Uncertainty", tabName = "score2"),
                                          menuSubItem("Mean Annual Cycle", tabName = "score3"),
                                          menuSubItem("Individual Model", tabName = "score4"),
                                          menuSubItem("Climate Change", tabName = "score5"),
                                          menuSubItem("Storm tracks", tabName = "score6"),
                                          menuSubItem("Individual Location", tabName = "score7"),
                                          menuSubItem("Comparator", tabName = "score8")
                                 ),
                                 menuItem("KPIs for Data Users", tabName = 'pu',startExpanded = TRUE,
                                          menuSubItem("Data", tabName = "score1"),
                                          menuSubItem("Spread or Uncertainty", tabName = "score2"),
                                          menuSubItem("Mean Annual Cycle", tabName = "score3"),
                                          menuSubItem("Individual Model", tabName = "score4"),
                                          menuSubItem("Climate Change", tabName = "score5"),
                                          menuSubItem("Storm tracks", tabName = "score6")
                                 ),menuItem("KPIs for Sectoral Communication", tabName = 'pu',startExpanded = TRUE,
                                            menuSubItem("Hydrology", tabName = "score1"),
                                            menuSubItem("Energy", tabName = "score2"),
                                            menuSubItem("Agriculture", tabName = "score3"),
                                            menuSubItem("Forestry", tabName = "score4"),
                                            menuSubItem("Climate Change", tabName = "score5"),
                                            menuSubItem("Storm tracks", tabName = "score6")
                                 ),
                                 
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
                                                 column(12, dataTableOutput('table.table')))),
                                      tabPanel("Summary", p(),verbatimTextOutput("summary.cc")),
                                      tabPanel("Table", p(),tableOutput("table.cc")),
                                      tabPanel("Distribution", p(), box(fluidPage(plotOutput("prob.cc",width = '100%',height = '900')))),
                                      tabPanel("Taylor Diagram", p(), fluidPage(plotOutput("taylor.cc"))),
                                      tabPanel("Scatter Plots", p() , 
                                               fluidPage(plotlyOutput("scatter.cc",width = '100%',height = '900')),
                                               fluidRow(
                                                 column(6,selectInput("period", label = "Future Periods", choices = c("2021-2050","2070-2100"),
                                                                      selected = "2021-2050",width = "100%")),
                                                 column(6,selectInput("region", label = "Regions", choices = c("ALA"),
                                                                      selected = "ALA",width = "100%"))))
                          )),
                  tabItem(tabName = "score6", box("du-b"))
                )
              )
)

