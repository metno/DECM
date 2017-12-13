
library(shiny)
library(shinydashboard)
if (!require("DT")) install.packages('DT')
library(DT)
library(DECM)

#data("statistics.cmip.tas.2071-2100")
data("metaextract")
im <- meta$project_id=="CMIP5" & meta$var=="tas"
gcmnames <- paste(seq(sum(im)),": ",meta$gcm[im],".",meta$gcm_rip[im],sep="")

dashboardPage(skin = "blue",
            dashboardHeader(title = "Evaluation of CMIP5 climate model simulations",
                            titleWidth = 600,
                            dropdownMenuOutput("messageMenu")),
            dashboardSidebar(
                tags$h4("Settings Panel "),
                checkboxGroupInput("gcms",
                  label = "Climate models (GCMs)",
                  choices = gcmnames,
                  selected = gcmnames[sample(1:107,5)])),# for testing purposes: random choice of 5 models
            dashboardBody(
              #tags$script(HTML("$('body').addClass('fixed');")),#fix sidebar and header (no scrolling)
              fluidPage(theme = "bootstrap.css",
                  fluidRow(column(12, box(title = "Disclaimer", status = 'info', collapsible = TRUE, collapsed = FALSE, width = '100%',
                                         tags$body("This is a prototype and should not be used as a basis for decision making."))
                           ),
                           column(6,
                              box(title = "Focus region",status = 'primary', width='60%',
                                  collapsible = TRUE, collapsed = FALSE,
                                         selectInput("region", label = "", width='75%', 
                                              choices = c("Global",
                                                          "Alaska/N.W. Canada [ALA:1]","Amazon [AMZ:7]",
                                                          "Central America/Mexico [CAM:6]","small islands regions Caribbean",
                                                          "Central Asia [CAS:20]","Central Europe [CEU:12]",
                                                          "Canada/Greenland/Iceland [CGI:2]","Central North America [CNA:4]",
                                                          "East Africa [EAF:16]","East Asia [EAS:22]",
                                                          "East North America [ENA:5]","South Europe/Mediterranean [MED:13]",
                                                          "North Asia [NAS:18]","North Australia [NAU:25]",
                                                          "North-East Brazil [NEB:8]","North Europe [NEU:11]",
                                                          "Southern Africa [SAF:17]","Sahara [SAH:14]",
                                                          "South Asia [SAS:23]","South Australia/New Zealand [SAU:26]",
                                                          "Southeast Asia [SEA:24]","Southeastern South America [SSA:10]",
                                                          "Tibetan Plateau [TIB:21]","West Africa [WAF:15]",
                                                          "West Asia [WAS:19]","West North America [WNA:3]",
                                                          "West Coast South America [WSA:9]","Antarctica",
                                                          "Arctic","Pacific Islands region[2]",
                                                          "Southern Topical Pacific","Pacific Islands region[3]",
                                                          "West Indian Ocean")),
                                  plotOutput("map", width = '100%', height = 153))
                           ),
                           column(6,
                                  box(title = "Time frame and scenario",status = 'primary', width='60%',
                                      collapsible = TRUE, collapsed = FALSE,
                                      selectInput("season",
                                                  label = "Season",
                                                  choices = c("Annual mean","Winter","Spring","Summer","Autumn"),
                                                  selected = "Annual mean"),
                                      selectInput("period",
                                                  label = "Future time period",
                                                  choices = c("Far future (2071-2100)",
                                                              "Near future (2021-2050)"),
                                                  selected = "Far future (2071-2100)"),
                                      selectInput("rcp",
                                                  label = "Emission scenario",
                                                  choices = c("RCP 4.5"),
                                                  selected = "RCP 4.5")
                                      )
                           ),
                           column(12, 
                              box(title = 'Scatterplot of the regional mean climate change', 
                                  width = '100%' , status = 'primary', # textOutput("title1")
                                  collapsible = TRUE, collapsed = FALSE,
                                  plotOutput("dtdpr", width = '100%', height = 550),
                                  column(width=6,sliderInput("tlim", "temperature range", width='90%',
                                              min = -20, max = 20, value = c(-4,4), step=0.5)),
                                  column(width=6,sliderInput("plim", "precipitation range", width='90%',
                                              min = -2, max = 2, value = c(-0.3,0.3), step=0.2))
                              )
                           ),
                           column(12,
                                  box(title = 'Calculate weighted climate change spread',
                                      width = '100%' , status = 'primary', # textOutput("title1")
                                      collapsible = TRUE, collapsed = TRUE,
                                      flowLayout(
                                        selectInput("regionw1", label = "1st focus region",
                                                    choices = c("Global",
                                                                "Alaska/N.W. Canada [ALA:1]","Amazon [AMZ:7]",
                                                                "Central America/Mexico [CAM:6]","small islands regions Caribbean",
                                                                "Central Asia [CAS:20]","Central Europe [CEU:12]",
                                                                "Canada/Greenland/Iceland [CGI:2]","Central North America [CNA:4]",
                                                                "East Africa [EAF:16]","East Asia [EAS:22]",
                                                                "East North America [ENA:5]","South Europe/Mediterranean [MED:13]",
                                                                "North Asia [NAS:18]","North Australia [NAU:25]",
                                                                "North-East Brazil [NEB:8]","North Europe [NEU:11]",
                                                                "Southern Africa [SAF:17]","Sahara [SAH:14]",
                                                                "South Asia [SAS:23]","South Australia/New Zealand [SAU:26]",
                                                                "Southeast Asia [SEA:24]","Southeastern South America [SSA:10]",
                                                                "Tibetan Plateau [TIB:21]","West Africa [WAF:15]",
                                                                "West Asia [WAS:19]","West North America [WNA:3]",
                                                                "West Coast South America [WSA:9]","Antarctica",
                                                                "Arctic","Pacific Islands region[2]",
                                                                "Southern Topical Pacific","Pacific Islands region[3]",
                                                                "West Indian Ocean")),
                                        selectInput("regionw2", label = "2nd focus region",
                                                    choices = c("---","Global",
                                                                "Alaska/N.W. Canada [ALA:1]","Amazon [AMZ:7]",
                                                                "Central America/Mexico [CAM:6]","small islands regions Caribbean",
                                                                "Central Asia [CAS:20]","Central Europe [CEU:12]",
                                                                "Canada/Greenland/Iceland [CGI:2]","Central North America [CNA:4]",
                                                                "East Africa [EAF:16]","East Asia [EAS:22]",
                                                                "East North America [ENA:5]","South Europe/Mediterranean [MED:13]",
                                                                "North Asia [NAS:18]","North Australia [NAU:25]",
                                                                "North-East Brazil [NEB:8]","North Europe [NEU:11]",
                                                                "Southern Africa [SAF:17]","Sahara [SAH:14]",
                                                                "South Asia [SAS:23]","South Australia/New Zealand [SAU:26]",
                                                                "Southeast Asia [SEA:24]","Southeastern South America [SSA:10]",
                                                                "Tibetan Plateau [TIB:21]","West Africa [WAF:15]",
                                                                "West Asia [WAS:19]","West North America [WNA:3]",
                                                                "West Coast South America [WSA:9]","Antarctica",
                                                                "Arctic","Pacific Islands region[2]",
                                                                "Southern Topical Pacific","Pacific Islands region[3]",
                                                                "West Indian Ocean")),
                                        selectInput("regionw3", label = "3rd focus region",
                                                    choices = c("---","Global",
                                                                "Alaska/N.W. Canada [ALA:1]","Amazon [AMZ:7]",
                                                                "Central America/Mexico [CAM:6]","small islands regions Caribbean",
                                                                "Central Asia [CAS:20]","Central Europe [CEU:12]",
                                                                "Canada/Greenland/Iceland [CGI:2]","Central North America [CNA:4]",
                                                                "East Africa [EAF:16]","East Asia [EAS:22]",
                                                                "East North America [ENA:5]","South Europe/Mediterranean [MED:13]",
                                                                "North Asia [NAS:18]","North Australia [NAU:25]",
                                                                "North-East Brazil [NEB:8]","North Europe [NEU:11]",
                                                                "Southern Africa [SAF:17]","Sahara [SAH:14]",
                                                                "South Asia [SAS:23]","South Australia/New Zealand [SAU:26]",
                                                                "Southeast Asia [SEA:24]","Southeastern South America [SSA:10]",
                                                                "Tibetan Plateau [TIB:21]","West Africa [WAF:15]",
                                                                "West Asia [WAS:19]","West North America [WNA:3]",
                                                                "West Coast South America [WSA:9]","Antarctica",
                                                                "Arctic","Pacific Islands region[2]",
                                                                "Southern Topical Pacific","Pacific Islands region[3]",
                                                                "West Indian Ocean")),
                                        selectInput("regionw4", label = "4th focus region",
                                                    choices = c("---","Global",
                                                                "Alaska/N.W. Canada [ALA:1]","Amazon [AMZ:7]",
                                                                "Central America/Mexico [CAM:6]","small islands regions Caribbean",
                                                                "Central Asia [CAS:20]","Central Europe [CEU:12]",
                                                                "Canada/Greenland/Iceland [CGI:2]","Central North America [CNA:4]",
                                                                "East Africa [EAF:16]","East Asia [EAS:22]",
                                                                "East North America [ENA:5]","South Europe/Mediterranean [MED:13]",
                                                                "North Asia [NAS:18]","North Australia [NAU:25]",
                                                                "North-East Brazil [NEB:8]","North Europe [NEU:11]",
                                                                "Southern Africa [SAF:17]","Sahara [SAH:14]",
                                                                "South Asia [SAS:23]","South Australia/New Zealand [SAU:26]",
                                                                "Southeast Asia [SEA:24]","Southeastern South America [SSA:10]",
                                                                "Tibetan Plateau [TIB:21]","West Africa [WAF:15]",
                                                                "West Asia [WAS:19]","West North America [WNA:3]",
                                                                "West Coast South America [WSA:9]","Antarctica",
                                                                "Arctic","Pacific Islands region[2]",
                                                                "Southern Topical Pacific","Pacific Islands region[3]",
                                                                "West Indian Ocean"))
                                      ),
                                      helpText("Enter weights for selected focus regions, seasons and temperature/precipitation to calculate a weighted average of the climate change spread in your model selection compared to the whole ensemble."),
                                      
                                      splitLayout(
                                        numericInput('wreg1', 'Region 1', 1,min=0, step=0.25),
                                        numericInput('wreg2', 'Region 2', 0,min=0, step=0.25),
                                        numericInput('wreg3', 'Region 3', 0,min=0, step=0.25),
                                        numericInput('wreg4', 'Region 4', 0,min=0, step=0.25)
                                      ),
                                      splitLayout(
                                        numericInput('wann', 'Annual', 1,min=0, step=0.25),
                                        numericInput('wdjf', 'Winter', 0,min=0, step=0.25),
                                        numericInput('wmam', 'Spring', 0,min=0, step=0.25),
                                        numericInput('wjja', 'Summer', 0,min=0, step=0.25),
                                        numericInput('wson', 'Autumn', 0,min=0, step=0.25)
                                      ),
                                      splitLayout(
                                        numericInput('wdt', 'Temperature', 1,min=0, step=0.25),
                                        numericInput('wdp', 'Precipitation', 1,min=0, step=0.25)
                                      ),
                                      htmlOutput("SpreadText")
                                  )
                           ),
                           column(12,
                              box(title = 'Mean annual temperature cycle', width = '100%', 
                                  status = 'primary', # textOutput("title1")
                                  collapsible = TRUE, collapsed = TRUE,
                                  #selectInput("ref",
                                  #            label = "Compare to reference",
                                  #            choices = c("Compare to reference",
                                  #                        "See future annual cycle"),
                                  #            selected = "Far future (2071-2100)"),
                                  plotOutput("Tcycle", width = '100%')
                              )
                           ),
                           column(12,
                                  box(title = 'Mean annual precipitation cycle', width = '100%', 
                                      status = 'primary', # textOutput("title1")
                                      collapsible = TRUE, collapsed = TRUE, 
                                      plotOutput("Pcycle", width = '100%')
                                  )
                           ),
                           column(12,
                                  box(title = 'Taylor diagram for spatial correlations', width = '100%',
                                      status = 'primary', # textOutput("title1")
                                      collapsible = TRUE, collapsed = TRUE,
                                      selectInput("varidTaylor",
                                                         label = "Variable",
                                                         choices = c("Temperature","Precipitation"),
                                                         selected = "Temperature"),
                                      plotOutput("taylor", width = '100%', height = 480)
                                  )
                           ),
                           column(12, 
                                  box(title = 'Table of climate model performance statistics', width = '100%' , status = 'primary',
                                      collapsible = TRUE, collapsed = TRUE,
                                      checkboxGroupInput("varid",
                                                         label = "Variable",
                                                         choices = c("Temperature","Precipitation"),
                                                         selected = "Temperature"),
                                      DT::dataTableOutput("table", width = '100%')
                                  )
                           ),
                           column(12,
                              box(title = 'Info', width='100%', status = 'primary',
                                h4("Method"),
                                "Put some reference to the methods here.",
                                h4("Source code"),
                                "The source code for this app is available at GitHub ",a("(http://github.com/metno/DECM/).",
                                                                             href="https://github.com/metno/DECM/"), 
                                "This code is partially based on the R-package 'esd' which is also freely available",
                                a("(http://github.com/metno/esd/).",href="https://github.com/metno/esd"))
                           )
                  )
                          
                )
              )

)
