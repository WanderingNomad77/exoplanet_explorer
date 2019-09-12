library(shiny)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(htmlwidgets)
library(htmltools)
library(shinydashboard)
library(tidyverse)
library(leaflet)

# Define UI for application

shinyUI(
    fluidPage(titlePanel(
        fluidRow(
            column(5, div(class = " titleContainer",tags$h2("NASA Exoplanet Explorer", align = 'left'))),
            column(1, '', windowTitle = 'NASA Exoplanet Archive'),
            column(6, tags$a( href = "https://exoplanetarchive.ipac.caltech.edu",tags$img(src = "NASA.png",height = '80px', width = '180px', align = 'right'),target="_blank")),

            tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=Squada+One&display=swap"),
            tags$style("h2{font-family: 'Squada One', serif;}")
                 )),
            
        div(id="navAlign",
             
        navbarPage(theme = shinytheme('darkly'),
                   
                   
    # Application title
    title = "", fluid = T, collapsible = T,


    ###################################################################################################################
    
    tabPanel("Description",class = "img3",
            sidebarLayout(position = 'right', 
                sidebarPanel(
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
                                 hr(),
                                 p("The objective is to assess the likelihood that a given candidate is indeed a planet by means of training a deep convolutional neural
                                   network. The model is designed in such a way that it becomes able to discriminate between a transiting exoplanet and a false positive such as 
                                   eclipsing binaries, instrumental artifacts and stellar variability (Shallue & Vanderburg 2018).")),
                        tabPanel("Observatories")
                    )
                )
            )
             
    ),
    
    
    
 # This panel contains the main data frame and several downloads options. 
 
    tabPanel("Data", class = "my_img",
             div(style = "padding: 20px !important; "),

       fluidPage(sidebarLayout(
           sidebarPanel(tags$style(".well {background-color:#375a7f;}"),
                        shinyWidgets::prettyCheckboxGroup("show_vars", "Columns to Display", animation = "pulse",
                                                          status = 'primary', names(planets), selected = names(planets[1:7]))),
       
       mainPanel(
                 tags$style(HTML('table.dataTable tr:nth-child(even) {background-color: #00bc8c !important;}')),
                 tags$style(HTML('table.dataTable tr:nth-child(odd) {background-color: #375a7f !important;}')),
                 tags$style(HTML('table.dataTable th {background-color: black !important;}')),
                 DT::dataTableOutput("planets", width = '100%')
   )))), 

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
                                     plotOutput('celestial', width = '1070px'))))),

###################################################################################################################

tabPanel("Dimensionality Reduction and K-means Clustering"),


###################################################################################################################

tabPanel("Exploratory Graphs", class= 'my_img',
         div(style = "padding: 20px !important;"),
         
             fluidRow(
                 column(1,
                 shinyWidgets::dropdownButton(
                 tags$h3("List of Inputs"),
                 selectInput('xcol', "X-Variable", choices = names(planets %>% select(which(sapply(.,class) %in% c('integer', 'character')), -pl_name, -ra_str, -dec_str, -loc_rowid, -pl_hostname)), 
                             selected = 'pl_discmethod'),status = 'danger', icon = icon('gear'),
                 tooltip = tooltipOptions(title = 'Click to Select Input'))),
                 column(5, 
                     plotlyOutput("histograms"), tags$caption("test")))),

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
         


###################################################################################################################

tags$head(
    tags$style(HTML(".my_img{background-image: url(dims.jpeg);}")),
    tags$style(HTML(".my_img{margin-top: -20px;}")),
    tags$style(HTML(".my_img{margin_bottom: -18px;}")),
    tags$style(HTML(".my_img{margin-right: -20px;} .my_img{margin-left: -20px;} .my_img{padding-left: 10px;} .my_img{padding-right: 10px !improtant;}"))),

tags$head(
    tags$style(HTML(".my_img2{background-image: url(https://www.jgbennett.org/wp-content/uploads/2015/11/Image-world-5.jpg);}
                    .my_img2{margin-top: -20px;}"))),

tags$head(tags$style(HTML(".img3{background-image: url(https://upload.wikimedia.org/wikipedia/commons/c/cc/Kepler-452b_artist_concept_-_animated_GIF.gif);}
                          .img3{margin-top: 20px; height: 720px;}  ")))

))))
