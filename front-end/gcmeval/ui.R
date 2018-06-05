
library(shiny)
library(shinydashboard)
if (!require("DT")) install.packages('DT')
library(DT)
library(DECM)

## Load metadata & subset common (pr and tas) GCMs
#data("metaextract")
load("../../back-end/data/metaextract.rda")
im <- meta$project_id=="CMIP5" & meta$var=="tas"
im2 <- meta$project_id=="CMIP5" & meta$var=="pr"
synch <- match(paste(meta$gcm[im2],meta$gcm_rip[im2]),paste(meta$gcm[im],meta$gcm_rip[im]))

gcmnames <- paste(seq(length(synch)),": ",meta$gcm[synch],".",meta$gcm_rip[synch],sep="")

##Create page
dashboardPage(skin = "blue",
              dashboardHeader(title = "Weighted CMIP5 model ranking and climate change spread",
                              titleWidth = 600,
                              dropdownMenuOutput("messageMenu")),
              dashboardSidebar(
                tags$h4("Settings Panel "),
                checkboxGroupInput("gcms",
                                   label = "Climate models (GCMs)",
                                   choices = gcmnames,
                                   # selected = gcmnames[sample(1:107,11)])),# for testing purposes: random choice of 11 models
                                   selected = gcmnames[c(21,34:37,42,81,97)])),
dashboardBody(
                #tags$script(HTML("$('body').addClass('fixed');")),#fix sidebar and header (no scrolling)
                fluidPage(theme = "bootstrap.css",
                          fluidRow(column(12, box(collapsible = FALSE, collapsed = FALSE, width = '100%',
                                                  tags$img(src="banner_c3s.png",width="100%"))
                          ),
                          column(12, box(title = "Disclaimer", status = 'info', collapsible = TRUE, collapsed = FALSE, width = '100%',
                                         tags$body("This is a prototype and should not be used as a basis for decision making. The GCM names might not correspond to the real GCM data."))
                          ),
                          column(6,
                                 box(title = "1st focus region",status = 'primary', width='60%',
                                     collapsible = TRUE, collapsed = FALSE,
                                     selectInput("regionwm1", label = "Select your primary focus region",
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
                                                             "West Indian Ocean"),selected = "North Europe [NEU:11]"),
                                     plotOutput("mapm1", width = '100%', height = 153),
                                     helpText("How important is the model performance in this region? (The corresponding weights are 0, 1 and 2.)"),
                                     flowLayout(
                                       selectInput("wmreg1", label = NA,
                                                   choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 2)))
                          ),
                          column(6,
                                 box(title = "2nd focus region",status = 'primary', width='60%',
                                     collapsible = TRUE, collapsed = FALSE,
                                     selectInput("regionwm2", label = "Select your secondary focus region",
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
                                                             "West Indian Ocean"),selected = "Central Europe [CEU:12]"),
                                     plotOutput("mapm2", width = '100%', height = 153),
                                     helpText("How important is the model performance in this region?"),
                                     flowLayout(
                                       selectInput("wmreg2", label = NA,
                                                   choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 1)))
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
                                 box(title = 'Skill skores', 
                                     width = '100%' , status = 'primary',
                                     collapsible = TRUE, collapsed = FALSE,
                                     helpText("Which skill skores (w.r.t. the reference data) are important?"),
                                     flowLayout(
                                       selectInput("wmbias", label = "Bias",
                                                   choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 2),
                                       selectInput("wmsc", label = "Spatial correlation",
                                                   choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 1),
                                       selectInput("wmsd", label = "Spatial variability",
                                                   choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 1),
                                       selectInput("wmcmpi", label = "CMPI¹",
                                                   choices = c("Not important (0)" = 0,"Important (1)" = 1, "Very important (2)" =2),selected = 1)),
                                     helpText("¹The combined model performance index summarizes the root mean square differences for multiple variables (following Gleckler et al. 2008: Performance metrics for climate models, J. Geophys. Res., 113, D06104, doi:10.1029/2007JD008972). Here, it is normalised with respect to the median rmse within the full ensemble."))
                          ),
                          column(12, 
                                 box(title = 'Weighted performance and projected climate change spread', 
                                     width = '100%' , status = 'primary',
                                     collapsible = TRUE, collapsed = FALSE,
                          htmlOutput("MetricText"))
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
                                                             "West Indian Ocean"),selected="North Europe [NEU:11]"),
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
                                     width = '100%' , status = 'primary',
                                     collapsible = TRUE, collapsed = FALSE,
                                     plotOutput("dtdpr", width = '100%', height = 550),
                                     column(width=6,sliderInput("tlim", "temperature range", width='90%',
                                                                min = -20, max = 20, value = c(-4,6), step=0.5)),
                                     column(width=6,sliderInput("plim", "precipitation range", width='90%',
                                                                min = -2, max = 2, value = c(-0.3,0.6), step=0.1))
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
