shinyServer(function(input, output, session) {
  
  costreport_df_react <- reactive({costreport_df %>% 
                                 filter(State %in% input$state & 
                                          Year %in% input$year)
    })
  
  output$table = DT::renderDataTable({
    costreport_df_table <- costreport_df_react() %>% 
      select(-Lat, -Long)
    
    datatable(costreport_df_table, 
              filter = 'top', 
              options = list(pageLength = 10, autoWidth = TRUE),
              rownames= FALSE) %>% 
      DT::formatCurrency(c("IME", "DGME", "Income"), currency = "$", digits = 0)
  })
  
  output$map <- renderLeaflet({
    costreport_df_map <- costreport_df_react() %>% 
      select(`Hospital ID`,
             Year,
             Hospital,
             Lat,
             Long)
    
    map_bounds <- costreport_df_map %>% 
      summarize(Lat1 = min(Lat, na.rm = T),
                Lat2 = max(Lat, na.rm = T),
                Long1 = min(Long, na.rm = T),
                Long2 = max(Long, na.rm = T))
    
    map_labs <- lapply(seq(nrow(costreport_df_map)), function(i) {
      paste0(
        '<b>',
        costreport_df_map[i, "Hospital"],
        '</b>',
        '<br/>',
        'ID: ',
        costreport_df_map[i, "Hospital ID"],
        '<br/>'
      )
    })
    
    leaflet_map <- costreport_df_map %>%
      leaflet() %>%
      addTiles() %>%
      fitBounds(lng1 = map_bounds$Long1, 
                lng2 = map_bounds$Long2,
                lat1 = map_bounds$Lat1,
                lat2 = map_bounds$Lat2) %>%
      addMarkers(
        lng = ~ Long,
        lat = ~ Lat,
        popup = lapply(map_labs, HTML)
      )
    leaflet_map
  })
  
  output$pivot <- rpivotTable::renderRpivotTable({
    costreport_df_pivot <- costreport_df %>% 
      select(-Long, -Lat)
    
    rpivotTable(data = costreport_df_pivot,
                rows = "Year", 
                cols="Teaching Status", 
                vals = "Freq", 
                aggregatorName = "Count", 
                rendererName = "Table")
  })
    
})