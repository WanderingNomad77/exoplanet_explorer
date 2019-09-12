

library(shiny)
library(DT)
library(grid)
library(Cairo)
library(ggthemes)

# Define server logic 

shinyServer(function(input, output) {

  # Dowloads tab datatable 
  
    output$planets <- DT::renderDataTable({
        
  
        DT::datatable(data = planets[, input$show_vars, drop = F], extensions = 'Buttons', style = 'bootstrap', escape = F, options = list(dom = 'Bfrtip', buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print'))) %>%
            formatStyle(names(planets[, input$show_vars]), color = 'white')
        
    })

    ###################################################################################################################
    
  # Astro Map Tab Graphs
    
    output$point <- renderPlot({    #1/2
     
 
     p1 <- ggplot(planets) + geom_point(data = planets, aes(x = ra, y = dec), col = 'white', size = 0.5) + theme_nightsky()
        
     s1 = input$tbl_rows_selected
   

     if (length(s1)) {
         p1 <- p1 + geom_point(data = planets[s1,],aes(ra, dec), col = 'red',shape = 'triangle', inherit.aes = F, size =2.5)
     }
  
      p1
        
    })
    
    output$celestial <- renderPlot({      #2/2
        
        
        g <- ggplot(x, aes()) + geom_sf(col= 'lightgrey', size = 0.5) + theme_nightsky() +
        geom_sf(data = milky_sf_trans, alpha = 0.2) + 
        geom_sf(data = stars_sf, col ='yellow', size = 0.2, alpha = 0.5)  
        grob <- ggplotGrob(g)
        grob$respect <- FALSE
        grid.newpage()
        grid.draw(grob)
        
    })
    
 # Reactive expressions to show brushed points in data frame.
    
    selectedData <- reactive({
        data <- brushedPoints(planets[,c("pl_name","ra", "dec")], input$plot_brush)
        if(nrow(data) == 0)
            data <- planets[,c("pl_name","ra", "dec")]
        data})
    
    output$tbl <- DT::renderDataTable({
        
       
   # Data table for Astro Map tab.
      
        DT::datatable(selectedData(), style = 'bootstrap', options = list(pageLength = 20),rownames = T)  %>%
            formatStyle(names(selectedData()), color = 'white')
    })
    
    ranges2 <- reactiveValues(x = NULL, y = NULL)
    
    observe({
        brush <- input$plot_brush
        if (!is.null(brush)) {
            ranges2$x <- c(brush$xmin, brush$xmax)
            ranges2$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges2$x <- NULL
            ranges2$y <- NULL
        }
    })
    
    
    ###################################################################################################################
    
    
    # Exploratory Plots
    
    output$histograms <- renderPlotly({
        
        ggplot(planets, aes_string(x = input$xcol)) + 
            geom_histogram(stat = 'count') +
            theme_foundation() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x  = element_text(hjust = 5))
    })
    
    
    ###################################################################################################################
    
    # Observatories Tab
    
    qsub <- reactive({             
   
      if(!input$kepler)
      dat <- filter(facility_coordinates, `Discovered Planets` <= input$pl_discovered[2], `Discovered Planets` >= input$pl_discovered[1])
      
      else if(input$kepler) 
      {
        dat <- filter(facility_coordinates, `Discovered Planets` <= input$pl_discovered[2], `Discovered Planets` >= input$pl_discovered[1]) %>% 
          rbind(facility_coordinates[facility_coordinates$pl_facility == "Kepler",])
      }
      
    })
    
    # MAP
    
    output$facilities <- renderLeaflet({
      
      m <- leaflet(data = qsub()) %>%
        addProviderTiles(providers$Stamen.Toner) %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng= qsub()$lon, lat= qsub()$lat,
                   popup = paste(
                     paste(tags$strong("Observatory: "),qsub()$pl_facility, sep = ""), 
                     paste(tags$strong("Number of Discovered Planets: "), qsub()$`Discovered Planets`, sep = ""), sep = '<br/>'))
      
      m
      
      proxy2 <- leafletProxy("facilities")
      
      if(input$kepler) 
        proxy2 %>%
        addMarkers(popup = paste(
          paste(tags$strong("Observatory: "), "Kepler Mission", sep = ""), 
          paste(tags$strong("Number of Discovered Planets: "), facility_coordinates$`Discovered Planets`[facility_coordinates$pl_facility == "Kepler"], sep = ""), sep = '<br/>'),
          lng =-76.62547,
          lat = 39.33273)
      
      m
      
    })
    
    # Data table
    
    output$obs_table <- renderDataTable({

      DT::datatable(qsub(), style = 'bootstrap', selection = 'single',options = list(pageLength = 5, scrollX = T, stateSave = T))
      
    })
    
    new_icon <- makeAwesomeIcon(icon ='flag',markerColor = "red", iconColor = 'white')
    
    previous_row <- reactiveVal()
    
   
    observeEvent(input$obs_table_rows_selected, {
      row_selected <- qsub()[input$obs_table_rows_selected,]
      proxy <- leafletProxy("facilities")
      proxy %>%
        addAwesomeMarkers(popup = paste(
          paste(tags$strong("Observatory: "),row_selected$pl_facility, sep = ""), 
          paste(tags$strong("Number of Discovered Planets: "), row_selected$`Discovered Planets`, sep = ""), sep = '<br/>'),
          layerId = row_selected$pl_facility,
          lng = row_selected$lon,
          lat = row_selected$lat,
          icon = new_icon)
      
      if(!is.null(previous_row())) {
        
        proxy %>%
          addMarkers(popup = paste(
            paste(tags$strong("Observatory: "),previous_row()$pl_facility, sep = ""), 
            paste(tags$strong("Number of Discovered Planets: "), previous_row()$`Discovered Planets`, sep = ""), sep = '<br/>'),
            layerId = previous_row()$pl_facility,
            lng = previous_row()$lon,
            lat = previous_row()$lat)
      }
      previous_row(row_selected)
    })
    
})





