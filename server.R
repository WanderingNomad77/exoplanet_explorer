library(shiny)


# Define server logic 

shinyServer(function(input, output, session) {

  
  
###################################################################################################################
#                                                    DATA                                                         #
###################################################################################################################
  
    output$planets <- DT::renderDataTable(server = F, {
        
  
        DT::datatable(
          data = planets[, input$show_vars, drop = F], 
          extensions = 'Buttons', style = 'bootstrap', escape = F, 
          options = list(
            dom = 'Bfrtip', buttons = 
              list('copy', 'print', list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )), 
            scrollX =T)) %>%
            formatStyle(names(planets[, input$show_vars]), color = 'white')
        
    })
    
    # Summary statistics for planets table. 
    
    output$planetary_summary <- renderPrint({
      
      summary(planets[,(input$var1)])
    })
    
    
    # TCE Table
    
    output$tce <- DT::renderDataTable(server = F, {
      options(scipen = 999)
      DT::datatable(
        data = select(tce, loc_rowid, `Kepler ID` = kepid, `Planet Number` = tce_plnt_num, `Orbital Period (days)` = tce_period,
                      `Transit Epoch (BJD)` = tce_time0bk, `Impact Parameter` = tce_impact, `Transit Duration (Days)` = tce_duration,
                      `Transit Depth (ppm)` = tce_depth, `Transit Signal to Noise (SNR)` = tce_model_snr, `Autovetter Training Set Label` = av_training_set)[, input$selected_vars, drop = F],
        extensions = 'Buttons', style = 'bootstrap', escape = F, rownames = F,
        options = list(
          dom = 'Bfrtip', buttons = 
            list('copy', 'print', list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            )), 
          scrollX =T)) %>%
        formatRound(columns = c("Orbital Period (days)", "Impact Parameter", "Transit Duration (Days)",
                                "Transit Depth (ppm)"), digits = 3)
        
    })
    
    # Summary stats for tce table 
    
    output$tce_summary <- renderPrint({
      
      summary(tce[,(input$var2)])
      
    })

    
    # Query Builder
    
    output$querybuilder <- renderQueryBuilder({
      
      queryBuilder(data = df.data, filters = list(list(name = 'Planet Hostname', type = 'string'),
                                                  list(name = 'pl_letter', type = 'string'),
                                                  list(name = 'Planet Name', type = 'string'),
                                                  list(name = 'Discovery Method', type = 'string', input = 'selectize'),
                                                  list(name = 'Planet Mass or M*sin(i) Provenance', type = 'string', input = 'selectize'),
                                                  list(name = 'Kepler Field Flag', type = 'string', input = 'selectize'),
                                                  list(name = 'K2 Field Flag', type = 'string', input = 'selectize'),
                                                  list(name = 'Discovery Facility', type = 'string', input = 'selectize'),
                                                  list(name = 'Orbital Period (days)', type = 'double', min = min(df.data$`Orbital Period (days)`),
                                                       max = max(df.data$`Orbital Period (days)`), step = 0.4),
                                                  list(name = 'Orbit Semi-Major Axis (AU)', type = 'double', min = min(df.data$`Orbit Semi-Major Axis (AU)`),
                                                       max = max(df.data$`Orbit Semi-Major Axis (AU)`), step= 0.4),
                                                  list(name = 'Eccentricity', type = 'double'),
                                                  list(name = 'Inclination (deg)', type = 'double'),
                                                  list(name = 'Planet Mass or M*sin(i) [Jupiter mass]', type = 'double'),
                                                  list(name = "Planet Radius (Jupiter radii)", type = 'double'),
                                                  list(name = "Planet Density (g/cm**3)", type = 'double'),
                                                  list(name = "Right Ascension (decimal degrees)", type = 'double'),
                                                  list(name = "Declination (decimal degrees)", type = 'double'),
                                                  list(name = "st_dist", type ='double'),
                                                  list(name = 'gaia_dist', type = 'double'),
                                                  list(name = 'st_optmag', type = 'double'),
                                                  list(name = 'st_mass', type = 'double'),
                                                  list(name = 'st_teff', type = 'double'),
                                                  list(name = 'st_rad', type = 'double'),
                                                  list(name = 'gaia_gmag', type = 'double')),
                   
                   autoassign = FALSE,
                   default_condition = 'AND',
                   allow_empty = TRUE,
                   display_errors = FALSE,
                   display_empty_filter = FALSE
                   )
    })
    
    output$txtValidation <- renderUI({
      if(input$querybuilder_validate == TRUE) {
        h3('VALID QUERY', style="color:green")
      } else {
        h3('INVALID QUERY', style="color:red")
      }
    })
    
    output$txtFilterText <- renderUI({
      req(input$querybuilder_validate)
      h4(span('Filter sent to dplyr: ', style="color:blue"), span(filterTable(input$querybuilder_out, df.data, 'text'), style="color:green"))
    })
    
    output$txtFilterList <- renderPrint({
      req(input$querybuilder_validate)
      input$querybuilder_out
    })
    
    output$txtSQL <- renderPrint({
      req(input$querybuilder_validate)
      input$querybuilder_sql
    })
    
    
    output$dt <- renderDataTable(server = F, {
      req(input$querybuilder_validate)
      df <- filterTable(input$querybuilder_out, df.data, 'table')
      df <- df %>%
        select(-name, -nameFactor)
      DT::datatable(df ,style = 'bootstrap', extensions = c('Responsive', 'Buttons', 'Scroller'), options = list(dom = 'Bfrtip',
                                                                                                                 deferRender = F,
                                                                                                                 scrollY = 200,
                                                                                                                 scroller = TRUE,
                                                                                                       buttons = 
                                                                                                       list('copy', 'print',I('colvis'),
                                                                                                            list(
                                                                                                         extend = 'collection',
                                                                                                         buttons = c('csv', 'excel', 'pdf'),
                                                                                                         text = 'Download'
                                                                                                       ))))
    })
    
###################################################################################################################
#                                             Astro Map Tab Graphs                                                #
###################################################################################################################
    
    
    output$point <- renderPlot({    #1/2
     
 
     p1 <- ggplot() + 
       geom_point(data = planets, aes(x = `Right Ascension (decimal degrees)`, y = `Declination (decimal degrees)`), col = 'lightblue', size = 0.5, alpha = 0.3) +
       scale_x_reverse() +
       xlab('Right Ascension') + 
       ylab('Declination') +
       theme(axis.title.y = element_text(angle = 90)) +
       theme_nightsky() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                              
                              panel.border=element_blank(),panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank())
        
     s1 = input$tbl_rows_selected
   

     if (length(s1)) {
         p1 <- p1 + geom_point(data = planets[s1,],aes(`Right Ascension (decimal degrees)`, `Declination (decimal degrees)`), col = 'red',shape = 'triangle', inherit.aes = F, size =2.5)
     }
  
      p1 
        
    })
    
    output$celestial <- renderPlotly({      #2/2
        
        
g <- ggplot(x2, aes()) +
  geom_sf(data = x2, aes(label = Planet), col= 'lightblue', size = 0.5, alpha = 0.5) +
  geom_sf(data = milky_sf_trans, alpha = 0.3, aes(fill = id)) + 
  scale_fill_grey() + 
  geom_sf(data = stars_bright_sf, col = 'red', 
          aes(size=stars_bright_sf$newmag, text= paste('</br>Name: ',name,'</br>Stellar Magnitude: ',mag,'</br>Constellation: ',con))) +
  geom_sf(data = constellations_sf, col = 'lightgrey', apha = 0.2, aes(label = Constellation)) +
  geom_sf_text(data = constellations_sf, aes(label = constellations_sf$Constellation), col = 'orange' ) +
  xlab("") + ylab("") +
  theme_nightsky() 



g %>%
  ggplotly(tooltip = c('label', 'text')) %>%
  layout(height = 800)
     
        
    })
    
 # Reactive expressions to show brushed points in data frame.
    
    selectedData <- reactive({
        data <- brushedPoints(planets[,c("Planet Name","Right Ascension (decimal degrees)", "Declination (decimal degrees)")], input$plot_brush)
        if(nrow(data) == 0)
            data <- planets[,c("Planet Name","Right Ascension (decimal degrees)", "Declination (decimal degrees)")]
        data})
    
    output$tbl <- DT::renderDataTable({
        
       
   # Data table for Astro Map tab.
      
        DT::datatable(selectedData(), style = 'bootstrap', options = list(pageLength = 5),rownames = F)
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
#                                                  Light Curves                                                   #
###################################################################################################################   
    
# Full light curve
    
# Group of mutually dependent `selectizeInput` for filtering data.frame's columns .
# In case a target has more than 1 plnt_num, users can specify which event they wish to plot. 
    
    res_mod <- callModule(
      module = selectizeGroupServer,
      id = 'my_filters',
      data = tce,
      vars = c("kepid", 'tce_plnt_num')
    )
 

    
# Data table output
    
  output$cond_table <- DT::renderDataTable(res_mod(), style = 'bootstrap', options = list(pageLength = 5, scrollX = T),rownames = F )
    
# Plot output
  
   output$test <- renderPlot({
    generate_global_view(keplerID = ntext(), plnt_num = plnt_num())
   })
    
   # ntext() contains reactive value that corresponds to a kepler id. plnt_num() refers to a corresponding tce event. 
   
   # Graph is generated upon clicking on the 'Plot' action button. 
   
   ntext <- eventReactive(input$plot_, {
     as.character(res_mod()$kepid)
   })
   
   plnt_num <- eventReactive(input$plot_,{
     as.character(res_mod()$tce_plnt_num)
   })
    
   output$full_lightcurve <- renderPlot ({
     
     generate_view(keplerID = ntext())
     
   })
    
###################################################################################################################
#                                                 Exploratory Plots                                               #
################################################################################################################### 
    
options(scipen = 999)
    
## Histograms
    
    output$histograms <- renderPlotly({
        
        p <- ggplot(subset(planets, !is.na(input$xcol)), aes_string(x = as.name(input$xcol))) + 
            theme_economist() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x  = element_text(hjust = 5))
        
        p
        
        if(input$xcol %in% names(planets %>% dplyr::select(which(sapply(.,class) == 'numeric'))))
          p + geom_histogram(binwidth = 0.1, aes(fill = `Discovery Method`)) +
          scale_x_log10() +
          scale_fill_manual(values = c("grey","black","brown", "orange", "red", "purple", "darkblue", "lightblue",
                                       'blue', "darkgreen")) 
        
        
        
        else { p + geom_histogram(stat = 'count', aes(fill = `Discovery Method`))  +
            scale_fill_manual(values = c("grey","black","brown", "orange", "red", "purple", "darkblue", "lightblue",
                                         'blue', "darkgreen")) }
    })
    
    output$dens_plot <- renderPlotly({
      
      d <- ggplot(data = planets, aes_string(x = as.name(na.omit(input$col_x)))) +
        aes(text = input$col_x) +
        theme_economist() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x  = element_text(hjust = 5)) +
        geom_density(alpha = 0.3, aes(fill = `Discovery Method`))
      
      if(input$log_x)
        d <- d + scale_x_log10()
      
      d
    })
    
    
## Correlation Plots
    
    
    ### Define reactive values for point exclusion. 
    
    vals <- reactiveValues(
      keeprows = rep(TRUE, nrow(planets))
    )
   
    ### Plot logic. 
    
    # Scatter Plot 
    
    output$dotplot <- renderPlot({
      
      keep <- planets[vals$keeprows, , drop = F]
      exclude <- planets[!vals$keeprows, , drop = F]
      
      
      # Apply filter by year of discovery
      
      keep$`Year of Discovery` <- as.numeric(as.character(keep$`Year of Discovery`))
      keep <- keep %>%
        filter(`Year of Discovery` <= input$year[2],
               `Year of Discovery` >= input$year[1])
      
      keep <- keep %>%
        filter(`Discovery Method` %in% input$disc_method)
      
      # Pearson Correlation info
      
      grob3 <- grobTree(textGrob(paste("R = ", round(cor(x = keep[input$xcolumn], y = keep[input$ycolumn],  use = 'na.or.complete'), 4) ), x = 0.60, y = 0.95, hjust = 0 ,gp = gpar(col = "red", fontsize = 11, fontface = "bold")))
      
 
      # Main plot 
      
        a <- ggplot(data = filter(na.omit(keep[,c(input$x_col, input$ycolumn, input$xcolumn,input$color, "Planet Name", "Discovery Method", "Year of Discovery")])), 
                    aes_string(x = as.name(input$xcolumn), y = as.name(input$ycolumn))) +
          geom_point() +
          geom_point(data = exclude, fill = NA, shape = 21, alpha = 0.2) +
          labs(title = paste(as.name(input$ycolumn), "vs.", as.name(input$xcolumn), sep = " ")) +
          theme_gdocs() +
          theme(axis.title = element_text(size = 12, vjust = 1), plot.title = element_text(size = 16, vjust = 1), legend.text = element_text(size = 9),
                legend.position = 'right')
          

        # Global Options
        
      if (input$color != 'None')
        a <- a + aes_string(color = as.name(input$color))
        
        if (input$color == 'None')
          a <- a + theme(axis.title = element_text(size = 12, vjust = 1), plot.title = element_text(size = 16, vjust = 1), legend.text = element_text(size = 9),
                legend.position = 'none')
      
      if (input$color == 'Discovery Method')
        a <- a + aes_string(color = as.name(input$color)) + scale_color_manual(values = c("#ff0000","#f79292","#b05cfa","#387bab", "#64ab2b", "#2e6601", 
                                                                                          "#37ada2", "#05dbf2","#ffd88a", "#f2a200",'#f0a5e2'))
      
      if (input$smooth)
        a <- a + geom_smooth(method = 'lm', se = F, col =  'orange', fullrange = F, formula = y ~ x)
      
      if(input$loess)
        a <- a + geom_smooth(method = 'loess', se = F, col = 'red', fullrange = T)
      
      if(input$scale_x)
        a <- a + scale_x_log10() 
      
      if(input$scale_y)
        a <- a + scale_y_log10()
        
      if(input$facet)
        a <- a + facet_wrap(~`Discovery Method`, drop = T)
        
      if(input$corr)
      a <- a + annotation_custom(grob3)

       if(input$facet && input$color == "Discovery Method")
       a <- a + facet_wrap(~`Discovery Method`, drop = T) + theme(legend.position = 'none')
        
      
        # Print Plot
      a
      
    })
    
    # Remove points that are clicked
    
    observeEvent(input$plot_dblclick, {
      
      res <- nearPoints(planets, input$plot_dblclick, allRows = T)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
    }) 
    
    # Remove points that are brushed
    
    observeEvent(input$exclude_toggle, {
      res <- brushedPoints(planets, input$plot_brush1, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    # Reset Plot
    
    observeEvent(input$reset_plot, {
      
      vals$keeprows <- rep(TRUE, nrow(planets))
      
    })
    
    
    # Data table containing double click point info. 
    
    output$clickinfo <- DT::renderDataTable({
      
      DT::datatable(nearPoints(na.omit(planets[c("Planet Name", "Discovery Method", "Discovery Facility", input$xcolumn, input$ycolumn)]), 
                 input$plot_click1, threshold = input$max_distance, maxpoints = input$max_points), filter = 'bottom', caption = 'Clicked Points Table',
                 style = 'bootstrap', extensions = c('Responsive', 'Scroller'), options = list(pageLength = 5, sDom  = '<"top">flrt<"bottom">ip',
                                                                                deferRender = TRUE,
                                                                                scrollY = 200,
                                                                                scroller = TRUE))
      
    })
    
    output$brushinfo <- DT::renderDataTable({
      
      DT::datatable(brushedPoints(na.omit(planets[c("Planet Name", "Discovery Method", "Discovery Facility", input$xcolumn, input$ycolumn)]),
                                  input$plot_brush1, xvar = input$xcolumn, yvar = input$ycolumn), filter = 'bottom', caption = 'Brushed Points Table' ,
                    style = 'bootstrap', extensions = c('Responsive', 'Scroller'),options = list(pageLength = 5, sDom  = '<"top">flrt<"bottom">ip',
                                                                                                 deferRender = TRUE,
                                                                                                 scrollY = 200,
                                                                                                 scroller = TRUE))
    })
    

    #######################
    #     Box plots       #
    #######################
    
    # First boxplot contains data brushed in the scatter plot, i.e. selectedInfo().
    
    selectedInfo <- reactive({
      
      data <- brushedPoints(na.omit(planets[,c(input$x_col, input$ycolumn, input$xcolumn, "Planet Name", "Discovery Method", "Year of Discovery")]), input$plot_brush1)
      
      if (nrow(data) == 0)
        data <- na.omit(planets[,c(input$x_col, input$ycolumn, input$xcolumn, "Planet Name", "Discovery Method", "Year of Discovery")])
      
      # Filtering options: 
      
      data$`Year of Discovery` <- as.numeric(as.character(data$`Year of Discovery`))
      data <- data %>%
        filter(`Year of Discovery` <= input$year[2],
               `Year of Discovery` >= input$year[1])
      
      data <- data %>%
        filter(`Discovery Method` %in% input$disc_method)
      
      
      data
      
    })
   
    # First box plot. Response variable is the same as in the scatter plot. 
    
    output$boxplot1 <- renderPlotly({
      
      
      # Plot logic
      
      b <- ggplot(filter(selectedInfo(), `Discovery Method` != "NA"), aes_string(x = as.name(input$x_col) , y = as.name(input$ycolumn), fill = as.name(input$x_col),
                                                                                 label = as.name('Planet Name'))) + 
        geom_boxplot() +
        scale_fill_manual(values = c("#ff0000","#f79292","#b05cfa","#387bab", "#64ab2b", "#2e6601", 
                                     "#37ada2", "#05dbf2","#ffd88a", "#f2a200",'#f0a5e2')) +
        geom_point(shape = 21) +
        ggtitle(paste(input$ycolumn, "-", input$x_col, "Distribution", sep =" ")) +
        theme_gdocs() +
        theme(axis.title = element_text(size = 12, vjust = 1), legend.position = 'none', axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
              plot.title = element_text(size = 15, colour = '#4979ab')) 
      
      
      #  Graph Options
      
      if(input$logbox) {
        b <- b + scale_y_log10()
        b %>%
          ggplotly(tooltip = c('y', "label"))
      }
      else{
      b %>%
        ggplotly(tooltip = c('y', "label"))
      }
      
      if(input$coord_flip) {
        b <- b + coord_flip()
        b %>%
          ggplotly(tooltip = c('y', 'label'))
      }
      else {
        
        b %>%
          ggplotly(tooltip = c('y', "label"))
      }
    
      
    })
    

    # The second boxplot contains the data from the scatter plot before and after the exclusion of points. The response variable corresponds to the x-axis of the scatter plot.
    
    output$boxplot2 <- renderPlotly({
      
      
      # Transformation function for axis values (2 decimal places).
      
      scaleFUN <- function(x) sprintf("%.2f", x)
      
      keep <- planets[vals$keeprows, , drop = F]
      
      # Applying filters:
      keep$`Year of Discovery` <- as.numeric(as.character(keep$`Year of Discovery`))
      keep <- keep %>%
        filter(`Year of Discovery` <= input$year[2],
               `Year of Discovery` >= input$year[1])
      
      keep <- keep %>%
        filter(`Discovery Method` %in% input$disc_method)
      
      # Plot logic
      
     c <- ggplot(data = filter(na.omit(keep[,c(input$x_col2, input$ycolumn, input$xcolumn, "Planet Name", "Discovery Method", "Year of Discovery")]), `Discovery Method` != "NA"), 
                 aes_string(x = as.name(input$x_col2) , y = as.name(input$xcolumn), fill = as.name(input$x_col2), label = as.name('Planet Name'))) + 
        geom_boxplot() +
        scale_fill_manual(values = c("#ff0000","#f79292","#b05cfa","#387bab", "#64ab2b", "#2e6601", 
                                     "#37ada2", "#05dbf2","#ffd88a", "#f2a200",'#f0a5e2')) +
        geom_point(shape = 21, label = as.name("Planet Name")) +
        theme_gdocs() +
       ggtitle(paste(input$xcolumn, '-', input$x_col2, 'Distribution',sep =" ")) +
        theme(axis.title = element_text(size = 12, vjust = 1), legend.position = 'none', axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
              plot.title = element_text(size = 15,  colour = '#4979ab'))
       
      
     # Options
      
     if(input$logbox2) {
       c <- c + scale_y_log10()
       c %>%
         ggplotly(tooltip = c('y', "label"))
     }
     else{
       c %>%
         ggplotly(tooltip = c('y', "label"))
     }
     
     if(input$coord_flip2) {
       c <- c + coord_flip()
       c %>%
         ggplotly(tooltip = c('y', 'label'))
     }
     else {
       
       c %>%
         ggplotly(tooltip = c('y', "label"))
     }
      
    })
    
    
###################################################################################################################
#                                                   Observatories Tab                                             #
###################################################################################################################
    
    qsub <- reactive({             
   
      if(!input$kepler)
      dat <- filter(facility_coordinates, `Discovered Planets` <= input$pl_discovered[2], `Discovered Planets` >= input$pl_discovered[1])
      
      else if(input$kepler) 
      {
        dat <- filter(facility_coordinates, `Discovered Planets` <= input$pl_discovered[2], `Discovered Planets` >= input$pl_discovered[1]) %>% 
          rbind(facility_coordinates[facility_coordinates$`Discovery Facility` == "Kepler",])
      }
      
    })
    
    # Data table
    
    
    output$obstable <- renderDataTable({
      
      sketch <- tags$table(
        class = "row-border stripe hover compact",
        tableHeader(c("", names(qsub()))),
        tableFooter(c("", buttons)))
      
      DT::datatable(qsub(), container = sketch, style = 'bootstrap', selection = 'single',options = list(pageLength = 5, scrollX = T, stateSave = T, columnDefs = list(
        list(
             className = "dt-center",
             targets = "_all"))), escape = F)
    })
    
    
   
    previous_row <- reactiveVal()
    
    new_icon <- makeAwesomeIcon(icon ='flag',markerColor = "red", iconColor = 'white')
    
    
    
    observeEvent(input$obstable_rows_selected, {
      row_selected <- qsub()[input$obstable_rows_selected,]
      proxy <- leafletProxy("facilities")
      proxy %>%
        addAwesomeMarkers(popup = paste(
          paste(tags$strong("Observatory: "), row_selected$website, sep = ""),
          paste(tags$strong("Number of Discovered Planets: "), row_selected$`Discovered Planets`, sep = ""),
          paste(tags$strong("Discovery Methods: "), row_selected$`Discovery Methods`, sep = ""),
          paste(tags$strong("Techniques used: "), paste(unique(planets$`Discovery Method`[planets$`Discovery Facility` == row_selected$`Discovery Facility`]), collapse= ", "), sep = ""),
          sep = '<br/>'),
          options = popupOptions(closeOnClick = F),
          layerId = row_selected$`Discovery Facility`,
          lng = row_selected$lon,
          lat = row_selected$lat,
          icon = new_icon,
          label = htmlEscape(row_selected$`Discovery Facility`)) %>%
        setView(zoom = 4, lat = row_selected$lat, lng = row_selected$lon)
      
      
      if(!is.null(previous_row())) {
        
        proxy %>%
          addMarkers(popup = paste(
            paste(tags$strong("Website: "), previous_row()$website, sep = ""), 
            paste(tags$strong("Number of Discovered Planets: "), previous_row()$`Discovered Planets`, sep = ""),
            paste(tags$strong("Discovery Methods: "), previous_row()$`Discovery Methods`, sep = ""),
            paste(tags$strong("Techniques used: "), paste(unique(planets$`Discovery Method`[planets$`Discovery Facility` == previous_row()$`Discovery Facility`]), collapse= ", "), sep = ""),
            sep = '<br/>'),
            options = popupOptions(closeOnClick = F),
            layerId = previous_row()$`Discovery Facility`,
            lng = previous_row()$lon,
            lat = previous_row()$lat)
      }
      
      
      previous_row(row_selected)
    })
    

    # MAP
    
    output$facilities <- renderLeaflet({
      
      m <- leaflet(data = qsub()) %>%
        addProviderTiles(providers$Stamen.Toner) %>%  # Add third party map tiles
        addMarkers(lng= qsub()$lon, lat= qsub()$lat, layerId = as.character(qsub()$`Discovery Facility`),
                   label = htmlEscape(qsub()$`Discovery Facility`) ,options = popupOptions(closeOnClick = F, autoClose = F),
                   popup = paste(
                     paste(tags$strong("Observatory: "), qsub()$website, sep = ""), 
                     paste(tags$strong("Number of Planets Discovered: "), qsub()$`Discovered Planets`, sep = ""),
                     paste(tags$strong("Discovery Methods: "), qsub()$`Discovery Methods`, sep = ""),
                     paste(tags$strong("Techniques used: "), paste(unique(planets$`Discovery Method`[planets$`Discovery Facility` == qsub()$`Discovery Facility`]), collapse= ", "), sep = ""),
                     sep = '<br/>')) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
      
      m
      
      
      proxy2 <- leafletProxy("facilities")
      
      if(input$kepler) 
        proxy2 %>%
        addMarkers(popup = paste(
          paste(tags$strong("Observatory: "), facility_coordinates$website[facility_coordinates$`Discovery Facility` == 'Kepler'], sep = ""),
          paste(tags$strong("Number of Discovered Planets: "), facility_coordinates$`Discovered Planets`[facility_coordinates$`Discovery Facility` == "Kepler"], sep = ""),
          paste(tags$strong("Discovery Methods: "), facility_coordinates$`Discovery Methods`[facility_coordinates$`Discovery Facility` == 'Kepler'], sep = ""),
          paste(tags$strong("Techniques used: "), paste(unique(planets$`Discovery Method`[planets$`Discovery Facility` == "Kepler"]), collapse= ", "), sep = ""),
          sep = '<br/>'),
          lng =-76.62547,
          lat = 39.33273)
      
      m
      
    })
    
    
    observeEvent(input$facilities_marker_click, {
      clickId <- input$facilities_marker_click$id
      DT::dataTableProxy("obstable") %>%
        selectRows(which(qsub()$`Discovery Facility` == clickId)) %>%
        selectPage(which(qsub()$`Discovery Facility` == clickId) %/% input$obstable_state$length + 1)
    })
    
    observeEvent(input$obstable_rows_selected, {
      
      row_selected <- qsub()[input$obstable_rows_selected,]
   

      output$planet_plot <-  renderPlotly({

        
    
            facility_planets <- planets %>%
              filter(`Discovery Facility` == row_selected$`Discovery Facility`)
        
          


        p <- ggplot(facility_planets, aes_string(as.name(input$colonne_x), as.name(input$colonne_y))) + 
          geom_point(aes(label = `Planet Name`, col = `Discovery Method`)) +
          labs(title = as.character(row_selected$`Discovery Facility`)) +
          theme_bw() +
          theme(plot.title = element_text(size = 11, hjust = 0))
        
        if(input$Xscale)
          p <- p + scale_x_log10()
        
        if(input$Yscale)
          p <- p + scale_y_log10()
        
        if(input$smooth1)
          p <- p + geom_smooth(method = 'lm')
        
        if(input$smooth2)
          p <- p + geom_smooth(method = 'loess')
        
        
      
        g <- plotly_build(p)
        
        g$x$data[[1]]$customdata = facility_planets$urls
        
        
        js <- "
function(el, x) {
  el.on('plotly_click', function(d) {
    var point = d.points[0];
    var url = point.data.customdata[point.pointNumber];
    window.open(url);
  });
}"
        
  g %>% onRender(js)
        
      
        
        })
        
    })
    
 
   # DT buttons
    
    dat <- facility_coordinates
    
    buttons <- lapply(1:ncol(dat), function(i){
      actionButton(
        paste0("this_id_is_not_used",i),
        "plot",
        class = "btn-primary btn-sm",
        style = "border-radius: 50%;", 
        onclick = sprintf(
          "Shiny.setInputValue('button', %d, {priority:'event'});
        $('#modal%d').modal('show');", i, i)
      )
    })
    
    # Modals 
    
      output$modals <- renderUI({
        lapply(1:ncol(dat), function(i){
          bsModal(
            id = paste0("modal",i),
            title = names(dat)[i],
            trigger = paste0("this_is_not_used",i),
            if(is.numeric(dat[[i]]) && length(unique(dat[[i]]))>19){
              fluidRow(
                column(5, radioButtons(paste0("radio",i), "",
                                       c("density", "histogram"), inline = TRUE),
                       prettyCheckbox(paste0('chkbox',i),"Log")),
                column(7,
                       conditionalPanel(
                         condition = sprintf("input.radio%d=='histogram'",i),
                         sliderInput(paste0("slider",i), "Number of bins",
                                     min = 5, max = 100, value = 30)
                         
                       ))
              )
            },
            plotOutput(paste0("plot",i))
          )
        })
      })
      
      # plots in modals
      
      for(i in 1:ncol(dat)){
        local({
          ii <- i
          output[[paste0("plot",ii)]] <- renderPlot({
            if(is.numeric(dat[[ii]]) && length(unique(dat[[ii]]))>19){
              if(input[[paste0("radio",ii)]] == "density"){
                pp <- ggplot(qsub(), aes_string(as.name(names(qsub())[ii]))) + 
                  geom_density(fill = "seashell", color = "seashell") + 
                  stat_density(geom = "line", size = 1) + 
                  theme_bw() + theme(axis.title = element_text(size = 16))
                print(pp)
                if(input[[paste0('chkbox',ii)]])
                  pp + scale_x_log10()
              }else{
                hist <- ggplot(qsub(), aes_string(as.name(names(qsub())[ii]))) + 
                  geom_histogram(bins = input[[paste0("slider",ii)]]) + 
                  theme_bw() + theme(axis.title = element_text(size = 16))
                print(hist)
                if(input[[paste0('chkbox',ii)]])
                  hist + scale_x_log10()
              }
            }else{
             
              hist2 <- ggplot(qsub(), aes_string(as.name(names(qsub())[ii]))) + geom_bar() + 
                geom_text(stat="count", aes(label=..count..), vjust=-0.5) + 
                xlab(names(qsub())[ii]) + theme_bw()
              print(hist2)
              
            }
          
          })
          
        })
      }
    
#########
      
      output$test4 <- renderPlotly({
        planets3 <- planets %>%
          select(disc = `Year of Discovery`, pl_name = `Planet Name`, method = `Discovery Method`)
        
      
        gb <- planets3 %>%
          select(Year = disc) %>%
          group_by(Year) %>%
          summarize(`Number of Planets Discovered` = n())
        
        
        
        ggplotly(ggplot(gb, aes(as.integer(Year), `Number of Planets Discovered`, label = Year)) + 
                   geom_area(fill = '#258f5a') + 
                   scale_x_continuous(breaks = c(1:28), labels = c(1989, 1992, 1994:2019)) +
                   
                   xlab("Year") + ggtitle("Yearly Exoplanet Discoveries") +
                   theme_economist() +
                   theme(axis.text = element_text(angle = 90)),
                 tooltip = c('label', 'y'))
        
       
        
      })
})