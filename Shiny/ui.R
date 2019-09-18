library(shiny)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(htmlwidgets)
library(htmltools)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(ggvis)
library(dygraphs)

# Define UI for application

shinyUI(
    fluidPage(titlePanel(
        fluidRow(
            column(5, div(class = " titleContainer",tags$h2("Exoplanet Explorer", align = 'left'))),
            tags$style("h2{
                         color: #178acc;}"),
            column(1, '', windowTitle = 'Exoplanet Archive'),
            column(6, tags$a( href = "https://exoplanetarchive.ipac.caltech.edu",tags$img(src = "NASA.png",height = '80px', width = '180px', align = 'right'),target="_blank")),

            tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=Squada+One&display=swap"),
            tags$style("h2{font-family: 'Squada One', serif;}")
                 )),
  
        div(id="navAlign",
        
           
        navbarPage(theme = shinytheme('slate'),
                   
                  
    # Application title
    title = "", fluid = T, collapsible = T,


###################################################################################################################
#                                                DESCRIPTION TAB                                                  #                  
###################################################################################################################
    
    tabPanel("Description",class = "img3",
             tags$head(tags$script(HTML('
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '))),
             tags$style(".navbar-default {
                         background: #4979ab;}"),
            sidebarLayout(position = 'right', 
                sidebarPanel(
                    tags$style(".navbar-default {
                         color: #cc3f3f;}"),
                    h1("NASA Exoplanet Archive"),
                    hr(),
                    p("The Nasa Exoplanet Archive is a service of the NASA Exoplanet Science Institute."),
                    br(),
                    p("The Confirmed Planets table contains lists of parameters for each planet discovered by the Kepler and K2 missions, coupled with the efforts of 50+ observatories Worldwide. 
                     The parameters in question were obtained by means of calculations and/or extracted from multiple references."),
                    br(),
                    p("This open source web app allows users to explore and interact with data from the Exoplanet Archive as well as from a few other sources.")
                ),
                mainPanel(
                    tabsetPanel(type = c("tabs"),
                        tabPanel("Identifying Exoplanets",
                                 
                                 tags$h5("Useful Neural Networks architecture for problems such as vetting transiting planet candidates"),
                                 
                                 tags$style("h5{color: #4979ab;
                                 font-size: 15px;
                                 font-style: bold;
                                 }"),
                                 
                                 hr(),
                                 p("The objective is to assess the likelihood that a given candidate is indeed a planet by means of training a deep convolutional neural
                                   network. The model is designed in such a way that it becomes able to discriminate between a transiting exoplanet and a false positive such as 
                                   eclipsing binaries, instrumental artifacts and stellar variability (Shallue & Vanderburg 2018).")),
                        tabPanel("Data",
                                 tags$h5("1. Confirmed Planets Table"),
                                 br(),
                                 wellPanel(style = "padding: 7px; width: 750px", p("The National Aeronautics and Space Administration (NASA) classifies objects as planetary, provided that they meet the following criteria:",
                                   tags$ul(
                                    tags$li("The mass of the object is equal to or less than 30 Jupiter masses;"),
                                    tags$li("The object is not free-floating;"),
                                    tags$li("Additional steps have been performed in order to exclude the probability that a confirmed planet is a false positive;")))),
                                 br(),
                                 tags$h5("2. Kepler Threshold-Crossing Events Table (TCE)"),
                                
                                 br(),
                                 wellPanel(style = "padding: 7px; width: 750px",
                                           p("This table will be used to create a training dataset to identify signals of transiting planets in Kepler Light Curves.",
                                             br(),
                                           "According to the NASA Exoplanet Archive, a 'Threshold-Crossing Event (TCE) is a sequence of transit-like features in the flux time series of a given target that resembles the signature of a 
                                   transiting planet to a sufficient degree that the target is passed on for further analysis'."),
                                           p("In other words, a TCE shows a transit-like pattern in a Kepler Light Curve, and provides information about the event such as:",
                                             tags$ul(
                                               tags$li("The interval between consecutive planetary transits;"),
                                               tags$li("The transit epoch corresponding to the center of the first detected transit in Barycentric Julian Day (BJD);"),
                                               tags$li("The impact parameter - The sky-projected distance between the center of the stellar disc and the center of the planet disc at conjunction, normalized by the stellar radius."),
                                               tags$li("The duration of the observed transits;"),
                                               tags$li("The fraction of stellar flux lost at the minimum of the planetary transit;"),
                                               tags$li("The transit depth normalized by the mean uncertainty in the flux during the transits;"),
                                               tags$li("'The Autovetter' set label, which results from the use of a random forest model to classify TCE's into 3 categories - namely,
                                                       'Planet Candidate' (PC), 'Astrophysical False Positive (AFP)', and 'Non-Transiting Phenomena (NTP)'. The classification algotithm 
                                                       of the 'Autovetter Project (McCauliff et al. 2015) discriminates events using features obtained from Kepler statistics."),
                                               tags$hr(style="border-color: white;"),
                                               p(tags$strong("The Threshold-Crossing Event Table is available here."), onclick = "fakeClick('Threshold Crossing Events')")
                                             ))
                                           
                                 )),
                        tabPanel("Light Curves",
                                 br(),
                                 tabsetPanel(type = c("tabs"),
                                             tabPanel("Full Light Curve", wellPanel(
                                           h4("Light Curve Example - Target ID 12735740 (Kepler-86 b)"),
                                           tags$hr(style = 'border-color: white;'),
                                           p("The arrows in the below light curve example indicate transit events which have been observed for Target ID #12735740"),
                                           tags$img(src = "12735740.png", height = '400px', width = '700px'), 
                                           
                                 p(br(), "The image above corresponds to the full light-curve of Star Kepler-86, over the entire Kepler mission. The arrows show a transiting planet, Kepler-86 b."), width = '100%', align = 'center',
                                 style = 'padding: 10px; width: 720px')),
                                 tabPanel("Global View",
                                          wellPanel(
                                            h4("Global View of Kepler-86's light curve:"),
                                            tags$hr(style = 'border-color:white;'),
                                            tags$img(src = '12735740_global_view.png', height = '400px', width = '700px'), 
                                            style = 'padding:10px; width:720px;', align = 'center'
                                          )))),
                        tabPanel("Exploratory Graphs"),
                        tabPanel("Observatories")
                        
                    )
                )
            )
             
    ),
    
    
    
###################################################################################################################
##                                                      DATA                                                     ##
###################################################################################################################
    
    navbarMenu("Data", 
               tabPanel("Confirmed Planets", class = "my_img",
             div(style = "padding: 20px !important; "),
             fluidPage(sidebarLayout(
               sidebarPanel(tags$style(".well {background-color:#375a7f;}"),
                            shinyWidgets::prettyCheckboxGroup("show_vars", "Columns to Display", animation = "pulse",
                                                              status = 'primary', names(planets), selected = names(planets[c("Planet Hostname", "Planet Name", "Discovery Method",
                                                                                                                             "Orbital Period (days)", "Orbit Semi-Major Axis (AU)",
                                                                                                                             "Eccentricity", "Planet Mass or M*sin(i) [Jupiter mass]",
                                                                                                                             "Planet Radius (Jupiter radii)")]))),
               
               mainPanel(
                 tags$style(HTML('table.dataTable tr:nth-child(even) {background-color: #375a7f !important;}')),
                 tags$style(HTML('table.dataTable tr:nth-child(odd) {background-color: #4979ab !important;}')),
                 tags$style(HTML('table.dataTable th {background-color: #3a3f44 !important;}')),
                 div(style = "display: inline-block;vertical-align:top;", downloadButton("download","Download as csv")),
                 div(style ="display: inline-block;vertical-align:top; width: 450px", HTML("<br>")),
                 div(style = "display: inline-block;vertical-align:top;", wellPanel(p(tags$a("Column definitions can be found at the NASA Exoplanet Archive", 
                                                                                             href = "https://exoplanetarchive.ipac.caltech.edu/docs/API_exoplanet_columns.html", target = "_blank")))),
                 DT::dataTableOutput("planets", width = '100%'))))),
             
             tabPanel("Threshold Crossing Events", class = 'img4', 
                      div(style = "padding: 20px !important;"),
                      fluidPage(sidebarLayout(
                        sidebarPanel(tags$style(".well {background-color: #375a7f};"),
             shinyWidgets::prettyCheckboxGroup("selected_vars", "Columns to Display", animation = 'jelly', 
                                               status = 'info', choices = names(select(tce, loc_rowid, `Kepler ID` = kepid, `Planet Number` = tce_plnt_num, `Orbital Period (days)` = tce_period,
                                                                                       `Transit Epoch (BJD)` = tce_time0bk, `Impact Parameter` = tce_impact, `Transit Duration (Days)` = tce_duration,
                                                                                       `Transit Depth (ppm)` = tce_depth, `Transit Signal to Noise (SNR)` = tce_model_snr, `Autovetter Training Set Label` = av_training_set)),
                                               
                                               selected = names(select(tce, -loc_rowid, `Kepler ID` = kepid, `Planet Number` = tce_plnt_num, `Orbital Period (days)` = tce_period,
                                                                       `Transit Epoch (BJD)` = tce_time0bk, `Impact Parameter` = tce_impact, `Transit Duration (Days)` = tce_duration,
                                                                       `Transit Depth (ppm)` = tce_depth, `Transit Signal to Noise (SNR)` = tce_model_snr, `Autovetter Training Set Label` = av_training_set)))),
             mainPanel(DT::dataTableOutput("tce")))),
             column(4,wellPanel(p("Documentation"))))),



###################################################################################################################
##                                          EXPLORATORY GRAPHS                                                   ## 
###################################################################################################################

navbarMenu("Exploratory Graphs",
           tabPanel("Correlation",
                    div(style = "padding: 20px !important;"),
                    fluidRow(
                      column(6,
                            wellPanel(
                              div(style = "display: inline-block;vertical-align:top;", shinyWidgets::dropdownButton(
                               tags$h3("Graph Options"),
                               
                               selectInput('xcolumn', "X-Axis", choices = names(planets %>% dplyr::select(which(sapply(.,class) == 'numeric'), -`Planet Name`, -`Right Ascension (sexagesimal)`, -`Declination (sexagesimal)`, -loc_rowid, -`Planet Hostname`)), 
                                           selected = "Planet Radius (Jupiter radii)"),
                               
                               prettyCheckbox(inputId = "scale_x", label = "Logarithmic Scale", icon = icon("check"), animation = 'pulse', value = T),
                               
                               hr(),
                               
                               selectInput('ycolumn', 'Y-Axis', choices = names(planets %>% dplyr::select(which(sapply(.,class) == 'numeric'), -`Planet Name`, -`Right Ascension (sexagesimal)`,
                                                                                                   -`Declination (sexagesimal)`, -loc_rowid, -`Planet Hostname`)), 
                                           selected = "Planet Mass or M*sin(i) [Jupiter mass]"),
                               
                               prettyCheckbox(inputId = "scale_y", label = "Logarithmic Scale", icon = icon("check"), animation = 'pulse', value = T),
                               
                               hr(),
                               
                               selectInput('color', 'Color', choices = c('None', names(planets %>% dplyr::select(which(sapply(.,class) == 'numeric'),
                                                                                                          `Discovery Method`,-`Planet Name`, 
                                                                                                          -`Right Ascension (sexagesimal)`, -`Declination (sexagesimal)`, 
                                                                                                          -loc_rowid, -`Planet Hostname`))),
                                           selected = 'Discovery Method'),
                               
                               hr(),
                               
                               materialSwitch(inputId = "smooth", label = "Linear Regression", status = 'primary', value = T),
                               materialSwitch(inputId = 'loess', label = "Lowess Smoothing", status = 'primary'),
                               
                               status = 'danger', icon = icon('gear'),
                               tooltip = tooltipOptions(title = 'Global Options'))),
                              div(style = "display: inline-block;vertical-align:top;width: 250px; align = 'center", HTML('<br>')),
                              div(style = "display: inline-block;vertical-align:top;", h4("Correlation Plot")),
                              
          
                              tags$hr(style="border-color: white;"),
                             
                             plotOutput("dotplot", click = "plot_click1",dblclick = "plot_dblclick", brush = brushOpts(id = "plot_brush1"))),
                             
                             br(),
                             
                             actionBttn("exclude_toggle", "Exclude Brushed Points", style = 'float'), actionBttn("reset_plot", "Reset", style = 'float')),
                      
                      column(6, 
                             wellPanel(div(style = "display: inline-block;vertical-align:top;", shinyWidgets::dropdownButton(
                        tags$h3("BoxPlot Options"),
                        selectInput('x_col', "X-Axis", choices = names(planets %>% dplyr::select(which(sapply(.,class) %in% c('integer', 'character', 'factor')), -`Planet Name`, -`Right Ascension (sexagesimal)`, -`Declination (sexagesimal)`, -loc_rowid, -`Planet Hostname`)), 
                                    selected = "Discovery Method"),status = 'danger', icon = icon('gear'),
                        tooltip = tooltipOptions(title = 'Global Options'))),
                        div(style = "display: inline-block;vertical-align:top;width: 250px; align = 'center", HTML('<br>')),
                        div(style = "display: inline-block;vertical-align:top;", h4("Distribution - Box Plot")),
                        tags$hr(style="border-color: white;"),
                        plotOutput('boxplots',brush = brushOpts(id = "plot_brush2"), hover = hoverOpts(id = "boxplot_hover"))))),
                    tags$hr(style="border-color: white;"),
                    
                    fluidRow(
                      
                      column(6, 
                             wellPanel(
                               div(class = "option-group",
                                   h4("Near-Point Options"),
                                   hr(),
                                   flowLayout(
                                     sliderInput("max_distance", "Max distance (pixels)",
                                                 min=1, max=20, value=5, step=1),
                                     sliderInput("max_points", "Max number of rows to select",
                                                 min=1, max = 50, value=5, step=1)
                                   ),
                                   DT::dataTableOutput("hoverinfo"))),
                             br(),
                             wellPanel(DT::dataTableOutput("brushinfo"))),
                      column(6, 
                             wellPanel(h4("Hover Points"),
                                       hr(),
                                       DT::dataTableOutput("box_near_point_info")),
                             br(),
                             wellPanel(DT::dataTableOutput("box_brushinfo"))))),
           
           tabPanel("Histograms", 
                    tags$style(".navbar-default { margin : 20 !important; }"),
                    div(style = "padding: 20px !important; "),
                    class = "my_img",  
                   wellPanel(shinyWidgets::dropdownButton(
                      tags$h3("List of Inputs"),
                      selectInput('xcol', "X-Variable", choices = names(planets %>% dplyr::select(which(sapply(.,class) %in% c('integer', 'character', 'factor', 'numeric')), -`Planet Name`, -`Right Ascension (sexagesimal)`, -`Declination (sexagesimal)`, -loc_rowid, -`Planet Hostname`, `Planet Radius (Jupiter radii)`)), 
                                  selected = "Discovery Method"),status = 'danger', icon = icon('gear'),
                      tooltip = tooltipOptions(title = 'Global Options')), hr(), plotlyOutput("histograms")), hr(), 
                   wellPanel(shinyWidgets::dropdownButton(
                     tags$h3("List of Inputs"),
                     selectInput('col_x', "X-Variable", choices = names(planets %>% dplyr::select(which(sapply(.,class) %in% c('integer', 'character', 'factor', 'numeric')), -`Planet Name`, -`Right Ascension (sexagesimal)`, -`Declination (sexagesimal)`, -loc_rowid, -`Planet Hostname`, `Planet Radius (Jupiter radii)`)), 
                                 selected = "Planet Radius (Jupiter radii)"),
                     prettyCheckbox(inputId = "log_x", label = "Logarithmic Scale", icon = icon("check"), animation = 'pulse', value = T),status = 'danger', icon = icon('gear'),
                     tooltip = tooltipOptions(title = 'Global Options')), hr(),
                   plotlyOutput("dens_plot"))
                    
           )),


###################################################################################################################
##                                                  ASTROMAP                                                    ##
###################################################################################################################

# This tab contains the AstroMap.

tabPanel("AstroMap - Confirmed Planets", tags$style("body{background-color: black;}"),
         tags$style(".navbar-default { margin : 20 !important; }"),
         div(style = "padding: 20px !important; "),
         
            fluidPage(
                sidebarLayout(
                sidebarPanel(DT::dataTableOutput('tbl')),
                
                mainPanel(plotOutput("point", brush = brushOpts(id = "plot_brush", resetOnNew = F)), 
                                     br(),
                          tags$hr(style="border-color: white;"),
                                     plotOutput('celestial', width = '1070px', brush = brushOpts(id = 'plot_brush')))))),


###################################################################################################################
#                                           LIGHT CURVES                                                          #
###################################################################################################################

tabPanel("Light Curves",
         tags$style(".navbar-default { margin : 20 !important; }"),
         div(style = "padding: 20px !important;"),
         tags$style("body{background-color: black;}"),
        fluidRow(
          column(6, wellPanel(selectizeGroupUI(id = 'my_filters',
                                               params = list(
                                                 kepid = list(inputId = 'kepid', title = "Kepler ID: "),
                                                 tce_plnt_num = list(inputId = 'tce_plnt_num', title = "Plnt Num: ")
                                               )),

          DT::dataTableOutput("cond_table")), 
          hr(), 
          tags$img(src = "transitgif2.gif", align = 'center', width = '100%')),
          column(6, 
                 wellPanel(actionButton("plot_", "Click to Plot"),
                         plotOutput("full_lightcurve")),
               hr(),
               wellPanel(plotOutput('test')))

       ),
        
        fluidRow(column(6), 
                 column(6, plotOutput("cond_plot")))),



   
###################################################################################################################



tabPanel("Dimensionality Reduction and K-means Clustering"),


###################################################################################################################
#                                                 OBSERVATORIES                                                   #
###################################################################################################################



tabPanel("Observatories", class = 'my_img2',
         div(style = "padding: 20px !important;"),
         div(style = 'margin-bottom: 10px;'),
         
         sidebarLayout(
             sidebarPanel(sliderInput(
                inputId = "pl_discovered",label = "Number of Discovered Planets", min = min(facility_coordinates$`Discovered Planets`),
                max = 400, step = NULL, value = c(min(facility_coordinates$`Discovered Planets`), max(facility_coordinates$`Discovered Planets`))),
                shinyWidgets::prettyCheckbox("kepler", label = "Include Kepler Mission (+2,000 planets discovered)", value = T, animation = 'jelly', status = 'info'),
                tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/f/f0/Animation_of_Kepler_trajectory.gif", align = 'center', width = '100%')
             ),
             
             mainPanel(leafletOutput("facilities", width = "100%"), 
                       br(),
                       dataTableOutput("obs_table")))),

###################################################################################################################

tabPanel("Sources"),
tabPanel("Contact"),


###################################################################################################################

tags$head(
    tags$style(HTML(".my_img{background-image: url(cosmos2.png);}")),
    tags$style(HTML(".my_img{margin-top: -20px;}")),
    tags$style(HTML(".my_img{margin_bottom: -18px;}")),
    tags$style(HTML(".my_img{margin-right: -20px;} .my_img{margin-left: -20px;} .my_img{padding-left: 10px;} .my_img{padding-right: 10px; !impotant;height: 100%;}")),
    
    tags$style(HTML(".my_img2{background-image: url(https://www.jgbennett.org/wp-content/uploads/2015/11/Image-world-5.jpg);}
                    .my_img2{margin-top: -20px;}")),
    
    
    tags$style(HTML(".img4{background-image: url(earth.jpg);}")),    
    tags$style(HTML(".img4{margin-top: -20px;}")),
    tags$style(HTML(".img4{margin_bottom: -18px;}")),
    tags$style(HTML(".img4{margin-right: -14px;} .img4{margin-left: -14px;} .img4{padding-left: 10px;} .img4{padding-right: 10px !important; height: 720px;}")),
    

tags$style(HTML(".img3{background-image: url(https://upload.wikimedia.org/wikipedia/commons/c/cc/Kepler-452b_artist_concept_-_animated_GIF.gif);}
                          .img3{margin-top: 20px; height: 720px;}  ")),

tags$style(HTML(".img{background-image: url(dims.jpeg);}")),    
          tags$style(HTML(".img{margin-top: -20px;}")),
          tags$style(HTML(".img{margin-bottom: -18px;}")),
          tags$style(HTML(".img{margin-right: -5px;} .img{margin-left: 5px;} .img{padding-left: 5px;} .img{padding-right: -5px !important; height: 100%;}")))


))))
