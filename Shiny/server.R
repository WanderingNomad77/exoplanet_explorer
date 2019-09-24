library(shiny)
library(DT)
library(grid)
library(Cairo)
library(ggthemes)
library(ggthemr)
library(parallel)
library(dplyr)


# Define server logic 

shinyServer(function(input, output, session) {

  
  # Dowloads tab datatable 
  
    output$planets <- DT::renderDataTable({
        
  
        DT::datatable(
          data = planets[, input$show_vars, drop = F], 
          extensions = 'Buttons', style = 'bootstrap', escape = F, 
          options = list(
            dom = 'Bfrtip',buttons = list('copy', 'print'), 
            scrollX =T)) %>%
            formatStyle(names(planets[, input$show_vars]), color = 'white')
        
    })
    
    output$download <- downloadHandler(
      filename = paste("planetary_data",Sys.Date(), ".csv", sep = ""),
      content = function(file) {
        write.csv(planets[, input$show_vars, drop = F], file, row.names = F)
      }
    )
    
    # TCE Table
    
    output$tce <- DT::renderDataTable({
      options(scipen = 999)
      DT::datatable(
        data = select(tce, loc_rowid, `Kepler ID` = kepid, `Planet Number` = tce_plnt_num, `Orbital Period (days)` = tce_period,
                      `Transit Epoch (BJD)` = tce_time0bk, `Impact Parameter` = tce_impact, `Transit Duration (Days)` = tce_duration,
                      `Transit Depth (ppm)` = tce_depth, `Transit Signal to Noise (SNR)` = tce_model_snr, `Autovetter Training Set Label` = av_training_set)[, input$selected_vars, drop = F],
        extensions = 'Buttons', style = 'bootstrap', escape = F, rownames = F,
        options = list(
          dom = 'Bfrtip',buttons = list('copy', 'print'), 
          scrollX =T)) %>%
        formatRound(columns = c("Orbital Period (days)", "Impact Parameter", "Transit Duration (Days)",
                                "Transit Depth (ppm)"), digits = 3)
        
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
       theme_nightsky()
        
     s1 = input$tbl_rows_selected
   

     if (length(s1)) {
         p1 <- p1 + geom_point(data = planets[s1,],aes(`Right Ascension (decimal degrees)`, `Declination (decimal degrees)`), col = 'red',shape = 'triangle', inherit.aes = F, size =2.5)
     }
  
      p1
        
    })
    
    output$celestial <- renderPlot({      #2/2
        
        
g <- ggplot(x, aes()) + geom_sf(col= 'lightblue', size = 0.5, alpha = 0.5)  +
  geom_sf(data = milky_sf_trans, alpha = 0.3, aes(fill = id)) + 
  scale_fill_grey() + 
  geom_sf(data = stars_bright_sf, col = 'red', 
          aes(size=stars_bright_sf$newmag, text=paste('</br>Name: ',name,'</br>Stellar Magnitude: ',mag,'</br>Constellation: ',con))) +
  geom_sf(data = constellations_sf, col = 'lightgrey', apha = 0.2) +
  ggsflabel::geom_sf_text_repel(data= stars_bright_sf,
                                aes(label=stars_bright_sf$name),
                                nudge_x = -2, 
                                colour="Yellow",
                                size= 5) + 
  ggsflabel::geom_sf_text_repel(data= constellations_sf,
                                aes(label=constellations_sf$id),
                                nudge_x = -1, 
                                size = 4,
                                colour="white") +
  xlab("") + ylab("") +
  theme_nightsky() 
     
        grob <- ggplotGrob(g)
        grob$respect <- FALSE
        grid.newpage()
        grid.draw(grob)
        
    })
    
 # Reactive expressions to show brushed points in data frame.
    
    selectedData <- reactive({
        data <- brushedPoints(planets[,c("Planet Name","Right Ascension (decimal degrees)", "Declination (decimal degrees)")], input$plot_brush)
        if(nrow(data) == 0)
            data <- planets[,c("Planet Name","Right Ascension (decimal degrees)", "Declination (decimal degrees)")]
        data})
    
    output$tbl <- DT::renderDataTable({
        
       
   # Data table for Astro Map tab.
      
        DT::datatable(selectedData(), style = 'bootstrap', options = list(pageLength = 15),rownames = T)  %>%
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
    
    output$dotplot <- renderPlot({
      
      keep <- planets[vals$keeprows, , drop = F]
      exclude <- planets[!vals$keeprows, , drop = F]
      
      grob3 <- grobTree(textGrob(paste("Pearson Correlation : ", round(cor(x = keep[input$xcolumn], y = keep[input$ycolumn],  use = "complete.obs"), 4) ), x = 0.63, y = 0.97, hjust = 0, gp = gpar(col = "red", fontsize = 11, fontface = "bold")))
      
        a <- ggplot(data = keep, aes_string(x = as.name(input$xcolumn), y = as.name(input$ycolumn))) +
          geom_point() +
          geom_point(data = exclude, fill = NA, shape = 21, alpha = 0.2) +
          labs(title = paste(as.name(input$ycolumn), "vs.", as.name(input$xcolumn), sep = " ")) +
          theme_economist() +
          theme(axis.title = element_text(size = 12, vjust = 1), plot.title = element_text(size = 16, vjust = 1), legend.text = element_text(size = 9)) +
          annotation_custom(grob3)
          

        
        # Global Options
        
      if (input$color != 'None')
        a <- a + aes_string(color= as.name(input$color))
      
      if (input$color == 'Discovery Method')
        a <- a + aes_string(color = as.name(input$color)) + scale_color_manual(values = c("grey","black","brown", "orange", "green", "darkgreen", 
                                                                                          "yellow", "red",'blue', "purple"))
      
      if (input$smooth)
        a <- a + geom_smooth(method = 'lm', se = F, col =  'orange', fullrange = F, formula = y ~ x)
      
      if(input$loess)
        a <- a + geom_smooth(method = 'loess', se = F, col = 'red', fullrange = T)
      
      if(input$scale_x)
        a <- a + scale_x_log10() 
      
      if(input$scale_y)
        a <- a + scale_y_log10()
      
   
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
    
    output$hoverinfo <- DT::renderDataTable({
      
      DT::datatable(nearPoints(na.omit(planets[c("Planet Name", "Discovery Method", "pl_facility", input$xcolumn, input$ycolumn)]), 
                 input$plot_click1, threshold = input$max_distance, maxpoints = input$max_points), style = 'bootstrap')
      
    })
    
    output$brushinfo <- DT::renderDataTable({
      
      DT::datatable(brushedPoints(na.omit(planets[c("Planet Name", "Discovery Method", "pl_facility", input$xcolumn, input$ycolumn)]),
                                  input$plot_brush1, xvar = input$xcolumn, yvar = input$ycolumn), style = 'bootstrap')
    })
    
    
    
    ### Box plots
    
    selectedInfo <- reactive({
      
      data <- brushedPoints(planets, input$plot_brush1)
      
      if (nrow(data) == 0)
        data <- planets
      
  data
  
    })
    
 
    output$boxplots <- renderPlot({
      
       ggplot(filter(selectedInfo(), `Discovery Method` != "NA", !is.na(input$xcol)), aes_string(x = as.name(input$x_col) , y = as.name(input$ycolumn), fill = as.name(input$x_col))) + 
       geom_boxplot() +
        scale_fill_manual(values = c("grey","black","brown", "orange", "green", "darkgreen", 
                                      "yellow", "red",'blue', "purple")) +
       geom_point(shape = 21) +
       theme_wsj() +
       theme(axis.title = element_text(size = 10, vjust = 1), legend.position = 'none', axis.text.x = element_text(angle = 45, hjust = 1))  +
       ylab(as.name(input$ycolumn)) + xlab(as.name(input$x_col))
      
    
    })
    
    
    # Outlier Removal: Remove outliers by selecting points in box plot. The selected points will be excluded in the correlation plot. 
    
    observeEvent(input$plot_brush2, {
      res <- brushedPoints(planets, input$plot_brush2, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    # Show brushed points in box plots in table
    
    output$box_brushinfo <- DT::renderDataTable({
      
      DT::datatable(brushedPoints(na.omit(planets[c("Planet Name", "Discovery Method", "pl_facility", input$x_col, input$ycolumn)]),
                                  input$plot_brush2, xvar = input$x_col, yvar = input$ycolumn), style = 'bootstrap')
    })
    
    # Show box plot hover info in table 
    
    output$box_near_point_info <- DT::renderDataTable({
      
      DT::datatable(nearPoints(na.omit(planets[c("Planet Name", "Discovery Method", "pl_facility", input$x_col, input$ycolumn)]),
                                  input$boxplot_hover), style = 'bootstrap', options = list(dom = 't'))
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
          rbind(facility_coordinates[facility_coordinates$pl_facility == "Kepler",])
      }
      
    })
    
    # MAP
    
    output$facilities <- renderLeaflet({
      
      m <- leaflet(data = qsub()) %>%
        addProviderTiles(providers$Stamen.Toner) %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng= qsub()$lon, lat= qsub()$lat,
                   popup = paste(
                     paste(tags$strong("Observatory: "), qsub()$website, sep = ""), 
                     paste(tags$strong("Number of Planets Discovered: "), qsub()$`Discovered Planets`, sep = ""),
                     paste(tags$strong("Discovery Methods: "), qsub()$`Discovery Methods`, sep = ""),
                     sep = '<br/>')) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
      
      m
      
      proxy2 <- leafletProxy("facilities")
      
      if(input$kepler) 
        proxy2 %>%
        addMarkers(popup = paste(
          paste(tags$strong("Observatory: "), facility_coordinates$website[facility_coordinates$pl_facility == 'Kepler'], sep = ""),
          paste(tags$strong("Number of Discovered Planets: "), facility_coordinates$`Discovered Planets`[facility_coordinates$pl_facility == "Kepler"], sep = ""),
          sep = '<br/>'),
          lng =-76.62547,
          lat = 39.33273)
      
      m
      
    })
    
    # Data table
    
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
    
    output$obs_table <- renderDataTable({
      sketch <- tags$table(
        class = "row-border stripe hover compact",
        tableHeader(c("", names(qsub()))),
        tableFooter(c("", buttons))
      )

      DT::datatable(qsub(), container = sketch, style = 'bootstrap', selection = 'single',options = list(pageLength = 5, scrollX = T, stateSave = T, columnDefs = list(
        list(
          className = "dt-center",
          targets = "_all"
        )
      )), escape = F)
      
      
    })
    
 
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
      
      # plots in modals ####
      
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
                ggplot(qsub(), aes_string(as.name(names(qsub())[ii]))) + 
                  geom_histogram(bins = input[[paste0("slider",ii)]]) + 
                  theme_bw() + theme(axis.title = element_text(size = 16))
              }
            }else{
             
              ggplot(qsub(), aes_string(as.name(names(qsub())[ii]))) + geom_bar() + 
                geom_text(stat="count", aes(label=..count..), vjust=-0.5) + 
                xlab(names(qsub())[ii]) + theme_bw()
             
              
            }
          
          })
          
        })
      }

    
    new_icon <- makeAwesomeIcon(icon ='flag',markerColor = "red", iconColor = 'white')
    
    previous_row <- reactiveVal()
    
   
    observeEvent(input$obs_table_rows_selected, {
      row_selected <- qsub()[input$obs_table_rows_selected,]
      proxy <- leafletProxy("facilities")
      proxy %>%
        addAwesomeMarkers(popup = paste(
          paste(tags$strong("Observatory: "), row_selected$website, sep = ""),
          paste(tags$strong("Number of Discovered Planets: "), row_selected$`Discovered Planets`, sep = ""),
          paste(tags$strong("Discovery Methods: "), row_selected$`Discovery Methods`, sep = ""),
          sep = '<br/>'),
          layerId = row_selected$pl_facility,
          lng = row_selected$lon,
          lat = row_selected$lat,
          icon = new_icon) %>%
        setView(zoom = 4, lat = row_selected$lat, lng = row_selected$lon)
      
      if(!is.null(previous_row())) {
        
        proxy %>%
          addMarkers(popup = paste(
            paste(tags$strong("Website: "), previous_row()$website, sep = ""), 
            paste(tags$strong("Number of Discovered Planets: "), previous_row()$`Discovered Planets`, sep = ""),
            paste(tags$strong("Discovery Methods: "), previous_row()$`Discovery Methods`, sep = ""),
            sep = '<br/>'),
            layerId = previous_row()$pl_facility,
            lng = previous_row()$lon,
            lat = previous_row()$lat)
      }
      previous_row(row_selected)
    })
    

 
 ########
    
    #TEST
    
    
    output$querybuilder <- renderQueryBuilder({
      queryBuilder(data = df.data, filters = list(list(name = 'Planet Hostname', type = 'string'),
                                                  list(name = 'pl_letter', type = 'string'),
                                                       list(name = 'Planet Name', type = 'string'),
                                                       list(name = 'Discovery Method', type = 'string', input = 'selectize')),
                                                  
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
    
    
    output$dt <- renderDataTable({
      req(input$querybuilder_validate)
      df <- filterTable(input$querybuilder_out, df.data, 'table')
      DT::datatable(df)
    })
    

  
    
})