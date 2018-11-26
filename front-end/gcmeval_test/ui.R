library(shiny)
library(shinyjs)

## Load metadata & subset common (pr and tas) GCMs
data("metaextract")
im <- meta$project_id=="CMIP5" & meta$var=="tas"
im2 <- meta$project_id=="CMIP5" & meta$var=="pr"
synch <- match(paste(meta$gcm[im2],meta$gcm_rip[im2]),paste(meta$gcm[im],meta$gcm_rip[im]))
gcmnames <- paste(seq(length(synch)),": ",meta$gcm[synch],".",meta$gcm_rip[synch],sep="")

regionlist <- c("Global",
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
                "West Indian Ocean")

##Create page
dashboardPage(skin = HTML("blue"),
              dashboardHeader(title = "Weighted CMIP5 model ranking and climate change spread", titleWidth = '600px', 
                              #set height for header
                              #dropdownMenuOutput("messageMenu"),
                              tags$li(class = "dropdown",
                                      tags$style(".main-header {height: 60px}"),
                                      tags$style(".main-header .logo {height: 60px}")
                              ),
                              tags$li(a(href = 'https://climate.copernicus.eu/data-evaluation-climate-models',
                                        img(src = 'https://climatedatasite.net/wp-content/uploads/2018/02/banner_c3s.png',
                                            title = "DECM website", height = "40px"),
                                        style = "padding-top:10px; padding-bottom:10px;"),
                                      class = "dropdown")
              ),
              dashboardSidebar(collapsed=FALSE, width='200px',
                               column(12,tags$h4("Ensemble selection")),
                column(12,actionButton("randomize", label="Random", width='110px')),
                column(12,actionButton("best", label="Best", width='110px')),
                column(12,numericInput("ngcm", label = "Ensemble size", 
                             value=11, min=1, max=length(gcmst), width='140px')),
                column(12,checkboxInput("gcm.repeat","Only one simulation from each GCM", value=FALSE)),
                column(12,checkboxGroupInput("gcms",
                                   label = "Climate models (GCMs)",
                                   choices = gcmnames,
                                   selected = gcmnames[1:11]))),
              dashboardBody(
                fluidPage(theme = "bootstrap.css",
                          fluidRow(column(12, box(collapsible = FALSE, collapsed = FALSE, width = '100%',
                                                  tags$img(src="banner_c3s.png",width="100%"))
                          ),
                          column(12, box(title = "GCMeval: A tool for climate model ensemble evaluation", status = 'info', collapsible = FALSE, collapsed = FALSE, width = '100%',
                                         htmlOutput("IntroText"),
                                         htmlOutput("DisclaimerText"))
                          ),
                          column(12,
                            box(title = "Spread of the regional mean climate change", 
                              status = 'primary', width='100%',
                              collapsible = TRUE, collapsed = FALSE,
                              column(12,
                                htmlOutput("ScatterText"),
                                br(),
                                br()
                              ),
                              column(12,
                                useShinyjs(),
                                extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-A', 'null'); }",
                                              functions = c("resetClick")),
                                plotlyOutput("dtdpr", width = '100%', height = 550),
                                br(),
                                br()
                                #verbatimTextOutput("clickevent")
                              ),
                              column(12,
                                column(4,
                                  selectInput("region", label = "Focus region", 
                                    choices = regionlist, selected="North Europe [NEU:11]"),
                                  plotOutput("map", width = '100%', height = 153)
                                ),
                                column(4,
                                  selectInput("season",
                                              label = "Season",
                                              choices = c("Annual mean","Winter","Spring","Summer","Autumn"),
                                              selected = "Annual mean"),
                                  selectInput("period",
                                              label = "Time horizon",
                                              choices = c("Far future (2071-2100)",
                                                          "Near future (2021-2050)"),
                                              selected = "Far future (2071-2100)"),
                                  selectInput("rcp",
                                              label = "Emission scenario",
                                              choices = c("RCP 4.5"),
                                              selected = "RCP 4.5")
                                ),
                                column(4,
                                    #htmlOutput("SpreadTextWeighted"),
                                    htmlOutput("SpreadText"),
                                    checkboxInput("weighted", 
                                                  label=HTML("<font size=-1><i> show weighted mean statistics </i></font>"), 
                                                  value=FALSE),
                                    br()
                                  )
                                )
                            )
                          ),
                          column(12, 
                            box(title = 'Model skill evaluation', 
                              width = '100%' , status = 'primary',
                              collapsible = TRUE, collapsed = FALSE,
                              column(12,
                                htmlOutput("RankingText"),
                                br()
                              ),
                              column(4,
                                     htmlOutput("MetricText"),
                                     br()
                              ),
                              column(8,
                                htmlOutput("ModelsText"),
                                br()
                              ),
                              column(12,
                                box(title = "Focus regions",
                                  width = '100%' , status = 'primary',
                                  collapsible = TRUE, collapsed = FALSE,
                                  column(6,
                                    selectInput("regionwm1", label = "Primary focus region",
                                      choices = regionlist, selected = "North Europe [NEU:11]"),
                                    plotOutput("mapm1", width = '100%', height = 153),
                                    helpText("How important is the model performance in this region? (The corresponding weights are 0, 1 and 2.)"),
                                    flowLayout(
                                      selectInput("wmreg1", label = NA,
                                        choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 2))
                                  ),
                                  column(6,
                                    selectInput("regionwm2", label = "Secondary focus region",
                                                choices = regionlist, selected = "Central Europe [CEU:12]"),
                                    plotOutput("mapm2", width = '100%', height = 153),
                                    helpText("How important is the model performance in this region?"),
                                    flowLayout(
                                      selectInput("wmreg2", label = NA,
                                        choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 1))
                                  )
                                )
                              ),
                              column(12, 
                                box(title = 'Variables', 
                                  width = '100%' , status = 'primary',
                                  collapsible = TRUE, collapsed = FALSE,
                                  helpText("How important is the performance of the single variables?"),
                                  flowLayout(
                                    selectInput("wmdt", label = "Temperature",
                                      choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 1),
                                    selectInput("wmdp", label = "Precipitation",
                                      choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 1)))
                              ),
                              column(12, 
                                box(title = 'Seasons', 
                                  width = '100%' , status = 'primary',
                                  collapsible = TRUE, collapsed = FALSE,
                                  helpText("How important is the annual and seasonal performance?"),
                                  flowLayout(
                                    selectInput("wmann", label = "Annual",
                                      choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 1),
                                    selectInput("wmdjf", label = "Winter",
                                      choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 2),
                                    selectInput("wmmam", label = "Spring",
                                      choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 2),
                                    selectInput("wmjja", label = "Summer",
                                      choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 2),
                                    selectInput("wmson", label = "Autumn",
                                      choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 2)))
                              ),
                              column(12, 
                                box(title = 'Skill scores', 
                                  width = '100%' , status = 'primary',
                                  collapsible = TRUE, collapsed = FALSE,
                                  helpText("Which skill scores (w.r.t. the reference data) are important?"),
                                  flowLayout(
                                    selectInput("wmbias", label = "Bias",
                                      choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 2),
                                    selectInput("wmsc", label = "Spatial correlation",
                                      choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 1),
                                    selectInput("wmsd", label = "Spatial variability",
                                      choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 1),
                                    selectInput("wmcmpi", label = HTML("CMPI<font size=-1><sup>2</sup></font>"),
                                      choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 1)),
                                    helpText(HTML(paste("<font size=-1><sup>2</sup></font>The combined model performance index summarizes the ",
                                                        "root mean square differences for multiple variables (following ",
                                                        "Gleckler et al. 2008: Performance metrics for climate models, ",
                                                        "J. Geophys. Res., 113, D06104, doi:10.1029/2007JD008972). ",
                                                        "Here, it is normalised with respect to the median rmse within the full ensemble."))))
                              ),
                              column(12,
                                box(title = 'Info', width='100%', status = 'primary',
                                    collapsible=TRUE, collapsed=TRUE,
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
                    )
)

