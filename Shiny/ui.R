library(shiny)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(htmlwidgets)
library(htmltools)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(queryBuilder)
library(jsonlite)
library(shinyjs)
library(shinyBS)



# Define UI for application

shinyUI(

    fluidPage(tags$style(HTML(".sidebar {
                      height: 90vh; overflow-y: auto;
                    }")),
      tags$style(HTML(".tabbable > .nav > li[class=active]    > a {background-color: #258f5a; color:white}
                      ")),
  
      titlePanel(
        fluidRow(
            column(5, div(class = "titleContainer", tags$a(absolutePanel(tags$h2("Exoplanet Explorer", align = 'center'),style = 'padding: 0px; border:3px solid; border-color:#4979ab;background-color:black; width:225px', width = '200px', right = 'auto',
                                                                          left = '25px'), onclick = "fakeClick('Description')"), style = 'height:120px;')),
            tags$style("h2{
                         color: #178acc;}"),
            column(1, '', windowTitle = 'Exoplanet Explorer'),
            column(6, tags$a(tags$img(src = "earth.gif",height = '100px', width = '185px', align = 'right'), onclick = "fakeClick('Description')")),

            tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=Saira+Stencil+One&display=swap"),
            tags$style("h2{font-family: 'Saira Stencil One', cursive;}")
                 )),
  
        div(id="navAlign",
        
           
        navbarPage(theme = shinytheme('slate'),
                   
                  
    # Application title
    title = "", fluid = T, collapsible = T,


###################################################################################################################
#                                                DESCRIPTION TAB                                                  #                  
###################################################################################################################
    
    tabPanel("Description", icon = icon('info'),class = "img3",
             tags$style(".navbar-default { margin : 10px !important; }"),
             div(style = "padding: 0px !important;"),
             
             # JS callback function to create links to tabs.
             
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
            
             tags$head(tags$script(HTML("
        // Enable navigation prompt
        window.onbeforeunload = function() {
            return 'Your changes will be lost!';
        };
    "))),
             tags$style(".navbar {
                         background: #4979ab; width:100%; border:2px solid; border-color:#436e9c}
                       "),
            sidebarLayout(position = 'right', 
                sidebarPanel(
                    h2("Open Exoplanet Catalogue", style = 'color:white;'),
                    hr(style = 'border-color:white; border:2px solid;'),
                    p(h3("Number of Confirmed Exoplanets: ",tags$strong(nrow(planets)), style = 'color:white;')),
                    p(h3("Number of Host Stars: ", tags$strong(n_distinct(planets$`Planet Hostname`)), style = 'color:white;')),
                    hr(style = 'border-color:white;'),
                    selectInput("var1", choices = names(select(planets, -`Discovery Method`, -`Planet Name`, -`Right Ascension (sexagesimal)`, -`Declination (sexagesimal)`)), selected = 'Orbit Semi-Major Axis (AU)',label = h3('Summary Statistics', style = 'color:white;')),
                    wellPanel(verbatimTextOutput('planetary_summary'), height = 200, style = 'background-color:#258f5a;'),
                    tags$hr(style = 'border-color: white;'),
                    p(h2("Acknowledgement", style = "color:white;"),"The research for this web app has made use of the NASA Exoplanet Archive, which is operated by the California Institute of Technology, under contract with the National Aeronautics and Space Administration under the Exoplanet Exploration Program."),
                    br(),
                    p(
                      "This web application also makes use of data from the first public release of the WASP data (Butters et al. 2010) as provided by the WASP consortium and services at the NASA Exoplanet Archive."),
                    br(),
                    p("KELT data are made available to the community through the Exoplanet Archive on behalf of the KELT project team.")
                   
                    
                ),
                mainPanel(
                    tabsetPanel(type = c("tabs"),
                  tabPanel("Background", 
                           br(),
                           
                           column(12, 
                                  wellPanel(
                                    p(
                  "At the present day, the progress we've made in the way we understand science, coupled with advances in technological means, have accelerated the global momentum towards space exploration and the search for extra-solar worlds.
                                           This quest could become the premise of a new story, which would contain a propensity for innovative thinking on a societal scale. 
                                           Hunting planets is an entertaining, paradigm shifting process, the uniqueness of which would be made even more special, should signs of 'Earthly' life be found in a 
                  planetary system foreign to our own."), style = 'padding:1px;')),
                  br(),
                
                  column(8,wellPanel(p("The launch of the Kepler Space Telescope has revolutionized space exploration by means of photometric techniques to observe hundreds of thousands of stars. The Kepler mission (May 2009 through May 2013), which aimed to discover
                  Earth-sized planets located within the habitable zone of Solar-like systems has led to the discovery of thousands of transiting exoplanets, in addition to numerous planet candidates."), style = 'padding: 5px;')),
                  column(8, wellPanel(tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/f/f0/Animation_of_Kepler_trajectory.gif", align = 'center', width = '600px', height = '400px'), style = 'padding:10px; width:620px;')),
           
                    column(8,wellPanel(p("This open source web app allows users to explore and interact with data from the Exoplanet Archive as well as from a few additional sources."), style = 'padding:5px;'))),
                        tabPanel("Data",
                                 br(),
                                 p("The interactive tables display data stored in the", (tags$a(tags$strong("Exoplanet Archive"), href = "https://exoplanetarchive.ipac.caltech.edu/", target = "_blank")),
                                                        "in a format that can be searched, filtered, sorted, plotted, and downloaded.
                                                        A query builder is also available for conditional and complementary filtering."),
                                 tabsetPanel(type = c("tabs"),
                                        
                                             tabPanel("Confirmed Planets",
                                                      br(),
                                 tags$a(tags$strong(tags$h5("1. Confirmed Planets Table")), onclick = "fakeClick('Confirmed Planets')"),
                                 br(),
                                 p("This table contains a set of confirmed exoplanets."),
                                 br(),
                                 wellPanel(style = "padding: 7px; width: 750px", p("The National Aeronautics and Space Administration (NASA) classifies objects as planetary, provided that they meet the following criteria:",
                                   tags$ul(
                                    tags$li("The mass of the object is equal to or less than 30 Jupiter masses;"),
                                    tags$li("The object is not free-floating;"),
                                    tags$li("Additional steps have been performed in order to exclude the probability that a confirmed planet is a false positive;")))),
                                 p("The Confirmed Planets table contains lists of parameters for each planet discovered by the Kepler and K2 missions, coupled with the efforts of 50+ observatories Worldwide. 
                     The parameters in question were obtained by means of calculations and/or extracted from multiple references.")),
                                 tabPanel("Threshold-Crossing Events",
                                 br(),
                                 tags$a(tags$strong(tags$h5("2. Kepler Threshold-Crossing Events Table (TCE)")), onclick = "fakeClick('Threshold Crossing Events')"),
                                
                                 br(),
                                 p("This table contains parameters for all targets observed by Kepler as Threshold Crossing Events (TCEs)."),
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
                                                       of the 'Autovetter Project (McCauliff et al. 2015) discriminates events using features obtained from Kepler statistics."))))),
                                 tabPanel("Query Builder",
                                          br(),
                                          wellPanel(tags$a(tags$strong(tags$h5("Quick guide for using the Query Builder")), onclick = "fakeClick('Query Builder')"),
                                                    tags$video(src = 'query_builder_how_to.mov',  autoplay = T, controls = T, width = '750px', height = '600px'), width = '100%',
                                                    style = 'padding:0px;width: 770px; background-color:black;')))),
                        
                        tabPanel("Light Curves",
                                 br(),
                                 tabsetPanel(type = c("tabs"),
                                             tabPanel("Intro",
                                                      br(),
                                                      wellPanel(tags$img(src= "transit.gif", width = '700px', height = '400px'), style = 'padding:10px; width:720px; background-color:orange;', align = 'center'),
                                                      h6("Illustration of a light curve produced by a transiting planet.")),
                                             tabPanel("Full Light Curve", 
                                                      br(),
                                                        wellPanel(
                                           h4("Light Curve Example - Target ID 12735740 (Kepler-86 b)"),
                                           tags$hr(style = 'border-color: white;'),
                                           p("The arrows in the below light curve example indicate transit events which have been observed for Target ID #12735740"),
                                           tags$img(src = "12735740.png", height = '400px', width = '700px'), 
                                           
                                 p(br(), "The image above corresponds to the full light-curve of Star Kepler-86, over the entire Kepler mission. The arrows show a transiting planet, Kepler-86 b."), width = '100%', align = 'center',
                                 style = 'padding: 10px; width: 720px')),
                                 tabPanel("Global View",
                                          br(),
                                          wellPanel(
                                            h4("Global View of Kepler-86's light curve:"),
                                            tags$hr(style = 'border-color:white;'),
                                            p("The 'Global-View' is a fixed-length representation of the entire light curve.
                                              This view is obtained by means of flattening, folding and centering the light curve on the TCE period.
                                              Below is a representation of Kepler-86, with the main event centered at t = 0."),
                                            tags$img(src = '12735740_global_view.png', height = '400px', width = '700px'), 
                                            style = 'padding:10px; width:720px;', align = 'center'
                                          )))),
                        
                        tabPanel("Identifying Exoplanets",
                                 br(),
                                 tags$h3("Using the Keras and Tensorflow libraries for vetting transiting planet candidates"),
                                 
                                 tags$style("h3{color: #4979ab;
                                 font-size: 15px;
                                 font-style: bold;
                                 }"),
                                 
                                 hr(),
                                 wellPanel(p("Among the recently developed techniques to facilitate the search for extra-solar planets (Exoplanets), deep learning has emerged as a promising tool."),
                                 p("In this section, we will train a model using Keras combined with a Tensorflow backend to assess the likelihood that a given candidate is indeed a planet."),
                                 p("Ideally, the model should be designed in such a way that it becomes able to discriminate between a transiting exoplanet and a false positive such as 
                                   eclipsing binaries, instrumental artifacts and stellar variability (Shallue & Vanderburg 2018)."))),
                        tabPanel("Exploratory Graphs",
                                 br(),
                                 wellPanel(tags$a(h4("Plot Section"), onclick = "fakeClick('Correlation')"), 
                                           p("Interactive plots that allow you to visualize relationships between planetary and/or stellar parameters."), 
                                           tags$hr(style = 'border-color:white;'),
                                                                           tags$img(src = "CorrelationExample.png", height = '400px', width = '700px'),
                                   tags$h6("figure 1. How planet radius behaves as a function of the Orbit Semi-Major axis length."),
                                   align = 'center', width = '100%', style = 'padding:10px; width:720px',
                                   p(br(), "These plots are generated from measurements extracted from the Confirmed Planets Table. Plot types include:"),
                                   p(
                                     tags$ul(
                                     tags$li("Correlation graphs (figure 1) to show the relationship between two numerical variables;"), 
                                     tags$li("Histograms and density plots to visualize variable distribution;"),
                                     tags$li("Box plots for depicting groups of numerical data through their quartiles.")
                                           )))),
                        tabPanel("Observatories",
                                 tags$br(),
                                 br(),
                                 wellPanel(tags$a(tags$strong(tags$h5("Tips for navigating the observatories interface")), onclick = "fakeClick('Observatories')"),
                                   tags$video(src = 'observatories_how_to_480.mov',  autoplay = T, controls = T, width = '750px', height = '600px'), width = '100%',
                                           style = 'padding:0px;width: 770px; background-color:black;'))
                        
                    )
                )
            )
             
    ),
    
    
    
###################################################################################################################
##                                                      DATA                                                     ##
###################################################################################################################
    
    navbarMenu("Data", icon = icon("database"),
              
       
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
             
                         tabPanel(
                         "Threshold Crossing Events", class = 'img4', 
                      div(style = "padding: 20px !important;"),
                      fluidPage(sidebarLayout(
                        sidebarPanel(tags$style(".well {background-color: #375a7f};"),
             shinyWidgets::prettyCheckboxGroup("selected_vars", "Columns to Display", animation = 'jelly', 
                                               status = 'info', choices = names(select(tce, loc_rowid, `Kepler ID` = kepid, `Planet Number` = tce_plnt_num, `Orbital Period (days)` = tce_period,
                                                                                       `Transit Epoch (BJD)` = tce_time0bk, `Impact Parameter` = tce_impact, `Transit Duration (Days)` = tce_duration,
                                                                                       `Transit Depth (ppm)` = tce_depth, `Transit Signal to Noise (SNR)` = tce_model_snr, `Autovetter Training Set Label` = av_training_set)),
                                               
                                               selected = names(select(tce, -loc_rowid, `Kepler ID` = kepid, `Planet Number` = tce_plnt_num, `Orbital Period (days)` = tce_period,
                                                                       `Transit Epoch (BJD)` = tce_time0bk, `Impact Parameter` = tce_impact, `Transit Duration (Days)` = tce_duration,
                                                                       `Transit Depth (ppm)` = tce_depth, `Transit Signal to Noise (SNR)` = tce_model_snr, `Autovetter Training Set Label` = av_training_set))),
             tags$hr(style = 'border-color:white;'),
             selectInput("var2", choices = names(select(tce, -loc_rowid, -kepid)), selected = 'tce_period',label = h3('Summary Statistics', style = 'color:white;')),
             verbatimTextOutput('tce_summary')),
           
             mainPanel(DT::dataTableOutput("tce")))
             )),
             
             tabPanel("Query Builder", class = 'my_img2',
                      
                      fluidPage(
                        fluidRow(
                          br(),
                          column(6, br(), queryBuilderOutput('querybuilder', width = 800, height = 300)),
                          column(6, uiOutput('txtValidation'), align = 'right', uiOutput('txtFilterText'), verbatimTextOutput('txtSQL')), style = 'height:200px;'),
                    hr(),
                        fluidRow(column(12,DT::dataTableOutput('dt', width = '800px', height = '600px'), align = 'center'), style = 'padding:10px;')
                     
                       
                      ))),



###################################################################################################################
##                                          EXPLORATORY GRAPHS                                                   ## 
###################################################################################################################

navbarMenu("Exploratory Graphs", icon = icon("chart-area"),
           
           tabPanel("Correlation",  
                    div(style = 'margin-bottom: -30px;'),
                    class = 'img_',
                    div(style = "padding: 20px !important;"),
                    fluidRow(
                      column(6,
                            wellPanel(
                              tags$div(style = "display: inline-block;vertical-align:top;", 
                                       
                                     
                                       
                                       shinyWidgets::dropdownButton( inputId = "Dropdown1", label = 'Test',
                                         
                                tags$h4("Graph Options"),
                                
                                selectInput('xcolumn', "X-Axis", choices = names(planets %>% dplyr::select(which(sapply(.,class) == 'numeric'), -`Planet Name`, -`Right Ascension (sexagesimal)`, -`Declination (sexagesimal)`, -`Planet Hostname`)), 
                                            selected = "Planet Radius (Jupiter radii)"),
                                
                                prettyCheckbox(inputId = "scale_x", label = "Logarithmic Scale", icon = icon("check"), animation = 'pulse', value = T),
                                
                                hr(),
                                
                                selectInput('ycolumn', 'Y-Axis', choices = names(planets %>% dplyr::select(which(sapply(.,class) == 'numeric'), -`Planet Name`, -`Right Ascension (sexagesimal)`,
                                                                                                           -`Declination (sexagesimal)`, -`Planet Hostname`)), 
                                            selected = "Planet Mass or M*sin(i) [Jupiter mass]"),
                                
                                prettyCheckbox(inputId = "scale_y", label = "Logarithmic Scale", icon = icon("check"), animation = 'pulse', value = T),
                                
                                hr(),
                                
                                selectInput('color', 'Color', choices = c('None', names(planets %>% dplyr::select(which(sapply(.,class) == 'numeric'),
                                                                                                                  `Discovery Method`,-`Planet Name`, 
                                                                                                                  -`Right Ascension (sexagesimal)`, -`Declination (sexagesimal)`, 
                                                                                                                  -`Planet Hostname`))),
                                            selected = 'Discovery Method'),
                                
                                hr(),
                                
                                materialSwitch(inputId = "smooth", label = "Linear Smoothing", status = 'primary', value = T),
                                materialSwitch(inputId = 'loess', label = "Lowess Smoothing", status = 'primary'),
                                
                                status = 'success', icon = icon('gear'),
                                tooltip = tooltipOptions(title = 'Global Options'), style = 'color:blue;')),
                                div(style = "display: inline-block;vertical-align:top;width: 250px; align = 'center", HTML('<br>')),
                                div(style = "display: inline-block;vertical-align:top;", h4("Correlation Plot")),
                               
                              tags$hr(style="border-color: white;"),
                              
                              plotOutput("dotplot", click = "plot_click1",dblclick = "plot_dblclick", brush = brushOpts(id = "plot_brush1")), 
                              style = 'border: 4px solid; border-color:#4979ab;'),
                              
                              ###
                            
                             
                             br(),
                             
                             actionBttn("exclude_toggle", "Exclude Brushed Points", style = 'float'), actionBttn("reset_plot", "Reset", style = 'float')),
                      
                      column(6, 
                             wellPanel(div(style = "display: inline-block;vertical-align:top;", shinyWidgets::dropdownButton(
                        tags$h4("BoxPlot Options"),
                        selectInput('x_col', "X-Axis", choices = names(planets %>% dplyr::select(which(sapply(.,class) %in% c('integer', 'character', 'factor')), -`Planet Name`, -`Right Ascension (sexagesimal)`, -`Declination (sexagesimal)`, -`Planet Hostname`)), 
                                    selected = "Discovery Method"),status = 'success', icon = icon('gear'),
                        tooltip = tooltipOptions(title = 'Global Options'))),
                        div(style = "display: inline-block;vertical-align:top;width: 250px; align = 'center", HTML('<br>')),
                        div(style = "display: inline-block;vertical-align:top;", h4("Distribution - Box Plot")),
                        tags$hr(style="border-color: white;"),
                        plotOutput('boxplots',brush = brushOpts(id = "plot_brush2"), hover = hoverOpts(id = "boxplot_hover")),  style = 'border: 4px solid; border-color:#4979ab;'))),
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
                      tags$h4("List of Inputs"),
                      selectInput('xcol', "X-Variable", choices = names(planets %>% dplyr::select(which(sapply(.,class) %in% c('integer', 'character', 'factor', 'numeric')), -`Planet Name`, -`Right Ascension (sexagesimal)`, -`Declination (sexagesimal)`,-`Planet Hostname`, `Planet Radius (Jupiter radii)`)), 
                                  selected = "Discovery Method"),status = 'info', icon = icon('gear'),
                      tooltip = tooltipOptions(title = 'Global Options')), hr(), plotlyOutput("histograms")), hr(), 
                   wellPanel(shinyWidgets::dropdownButton(
                     tags$h4("List of Inputs"),
                     selectInput('col_x', "X-Variable", choices = names(planets %>% dplyr::select(which(sapply(.,class) %in% c('integer', 'character', 'factor', 'numeric')), -`Planet Name`, -`Right Ascension (sexagesimal)`, -`Declination (sexagesimal)`, -`Planet Hostname`, `Planet Radius (Jupiter radii)`)), 
                                 selected = "Planet Radius (Jupiter radii)"),
                     prettyCheckbox(inputId = "log_x", label = "Logarithmic Scale", icon = icon("check"), animation = 'pulse', value = T),status = 'danger', icon = icon('gear'),
                     tooltip = tooltipOptions(title = 'Global Options')), hr(),
                   plotlyOutput("dens_plot"))
                    
           )),


###################################################################################################################
##                                                  ASTROMAP                                                    ##
###################################################################################################################

# This tab contains the AstroMap.

tabPanel("AstroMap - Confirmed Planets",icon = icon('rocket') ,tags$style("body{background-color: black;}"),
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

tabPanel("Light Curves", icon = icon('sun'),
         tags$style(".navbar-default { margin : 20 !important; }"),
         div(style = "padding: 10px !important;"),
         tags$style("body{background-color: black;}"),
        fluidRow(
          column(6, wellPanel(selectizeGroupUI(id = 'my_filters',
                                               params = list(
                                                 kepid = list(inputId = 'kepid', title = "Kepler ID: "),
                                                 tce_plnt_num = list(inputId = 'tce_plnt_num', title = "Plnt Num: ")
                                               )),

          DT::dataTableOutput("cond_table")),
          hr(),
          wellPanel(tags$img(src = "transitgif2.gif", align = 'center', width = '100%'), style = 'background-color:orange;')), 
          column(6, 
                 wellPanel(actionBttn("plot_", "Plot", style = 'material-flat', icon = icon("chart-bar")),
                           plotOutput("full_lightcurve")),
                 hr(),
                 wellPanel(plotOutput('test')))
       )),



   
###################################################################################################################



tabPanel("Dimensionality Reduction and K-means Clustering"),


###################################################################################################################
#                                                 OBSERVATORIES                                                   #
###################################################################################################################



tabPanel("Observatories", icon = icon("location-arrow", class = 'fas'), class = 'my_img2',
    
         div(style = 'margin-bottom: 10px;'),
         tags$style(type = "text/css", "#facilities {height: calc(100vh - 80px) !important;}"),
         div(class = 'outer'),
         tags$head(includeCSS('style.css')),
        
             
             
             leafletOutput("facilities", width = "100%"),
         
             absolutePanel(id = "controls", class = "panel panel-default", fixed = F,
                           draggable = TRUE, top = 250, left = 90, right = 'auto', bottom = "auto",
                           width = 480, height = "auto",
                           
                           h2("Observatory Explorer"),
                           tags$style(HTML('#obstable table.dataTable tr:nth-child(even) {background-color: black !important;}')),
                           tags$style(HTML('#obstable table.dataTable tr:nth-child(odd) {background-color: white !important;}')),
                           tags$style(HTML('#obstable table.dataTable tr:nth-child(odd) {color: black !important;}')),
                           tags$style(HTML('#obstable table.dataTable th {background-color: white !important;}')),
                           tags$style(HTML('#obstable table.dataTable th {color: black !important;}')),
                           

                        sliderInput(
                             inputId = "pl_discovered",label = "Number of Discovered Planets", min = min(facility_coordinates$`Discovered Planets`), 
                             max = 400, step = NULL, value = c(min(facility_coordinates$`Discovered Planets`), max(facility_coordinates$`Discovered Planets`))),
                             shinyWidgets::prettyCheckbox("kepler", label = "Include Kepler Mission (+2,000 planets discovered)", value = T, animation = 'jelly', status = 'info'),
                        dataTableOutput("obstable"),style = 'background-color:white;'
                           ), 
         
         absolutePanel(id = 'scatter_plot',
                             
                             shinyWidgets::dropdownButton(
                               tags$h4("Graph Options"),
                               
                               selectInput('colonne_x', "X-Axis", choices = names(planets %>% dplyr::select(which(sapply(.,class) == 'numeric'), -`Planet Name`, -`Right Ascension (sexagesimal)`, -`Declination (sexagesimal)`, -`Planet Hostname`)), 
                                           selected = "Orbit Semi-Major Axis (AU)"),
                               
                               prettyCheckbox(inputId = "Xscale", label = "Logarithmic Scale", icon = icon("check"), animation = 'pulse', value = T),
                               
                               hr(),
                               
                               selectInput('colonne_y', 'Y-Axis', choices = names(planets %>% dplyr::select(which(sapply(.,class) == 'numeric'), -`Planet Name`, -`Right Ascension (sexagesimal)`,
                                                                                                          -`Declination (sexagesimal)`, -`Planet Hostname`)), 
                                           selected = "Planet Mass or M*sin(i) [Jupiter mass]"),
                               
                               prettyCheckbox(inputId = "Yscale", label = "Logarithmic Scale", icon = icon("check"), animation = 'pulse', value = T),
                               
                               hr(),
                               
                               materialSwitch(inputId = "smooth1", label = "Linear Smoothing", status = 'primary', value = F),
                               materialSwitch(inputId = 'smooth2', label = "Lowess Smoothing", status = 'primary'),
                               
                               status = 'danger', icon = icon('gear'),
                               tooltip = tooltipOptions(title = 'Global Options')),
                             
                             hr(style = 'border-color:white;'),
                       fluidRow(verbatimTextOutput("Click_text")),
                             
                             plotlyOutput("planet_plot"),  fixed = F, draggable = F, right = 40, left = 'auto', top = 225, width = 500,  tags$head(tags$script(src="clickevent.js"))),uiOutput("modals")),

###################################################################################################################

tabPanel("Sources", icon = icon('copyright')),
tabPanel("Contact",icon = icon("id-badge") , 
         img(src = "washington_state.jpg", height = '750px', width ='1645px',align = 'center', style = 'margin-left:-5px; border:0.5px solid; border-color:white !important;'), 

         absolutePanel(br(),
                       tags$ul(
           tags$li(tags$a(actionBttn(style = 'material-flat', inputId = "linkedin", label = "Linkedin", icon = icon('linkedin')),href = "https://www.linkedin.com/in/jean-claude-kameni-3665a823/", target = "_blank")),
           hr(style = 'border-color:white;'),
           tags$li(tags$a(actionBttn(style = 'material-flat', inputId = 'github', label = "Github", icon = icon('github')), href = "https://github.com/WanderingNomad77", target = "_blank")),
           hr(style = 'border-color:white;'),
           tags$li(tags$a(actionBttn(style = 'material-flat', inputId = 'instagram', label = "Instagram", icon = icon('instagram')), href = "https://www.instagram.com/j.c.k.206/", target = "_blank")),
           hr(style = 'border-color:white;'),
           tags$li(tags$a(actionBttn(style = 'material-flat',inputId = "email1", label = "Email Me", 
                          icon = icon("envelope", lib = "font-awesome")),
             href="mailto:kamenijc@gmail.com"))),
           style = "background-color: #4979ab;width: 400px; border:1px solid; border-color:white", top = '275px', bottom = 'auto', left =  '60', right = 'auto'), 


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
    tags$style(HTML(".img4{margin-right: -14px;} .img4{margin-left: -14px;} .img4{padding-left: 10px;} .img4{padding-right: 10px !important; height: 1020px;}")),
    

tags$style(HTML(".img3{background-image: url(Kepler452b.gif);}
                          .img3{margin-top: 20px; height: 720px;}  ")),


tags$style(HTML(".img_{background-image: url(moon.jpg);}
                          .img_{margin-top: 20px; height:100%;}  ")),

tags$style(HTML(".img{background-image: url(dims.jpeg);}")),    
          tags$style(HTML(".img{margin-top: -20px;}")),
          tags$style(HTML(".img{margin-bottom: -18px;}")),
          tags$style(HTML(".img{margin-right: -5px;} .img{margin-left: 5px;} .img{padding-left: 5px;} .img{padding-right: -5px !important; height: 100%;}")))


)))))
