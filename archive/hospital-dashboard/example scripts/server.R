# Define server logic for the Shiny app
shinyServer(function(input, output, session) {
  observe({
    x <- input$id
    
    # Can use character(0) to remove all choices
    if (x == TRUE) {
      ws_list <- worksite_list_all
      cl_list <- clinic_list_all
    } else {
      ws_list <- worksite_list_25_miles
      cl_list <- clinic_list_30_miles
    }
    # Can also set the label and select items
    updatePickerInput(
      session,
      inputId = "worksite",
      label = paste0("Choose a worksite: ", length(ws_list), " sites"),
      choices = ws_list
    )
    
    updatePickerInput(
      session,
      inputId = "clinic",
      label = paste0("Choose a clinic to adjust: ", length(cl_list), " sites"),
      choices = cl_list
    )
  })
  
  access_data_react <- reactive({
    ws_cl_crosswalk %>%
      filter(worksite_label == input$worksite &
               drive_dist <= 35 & candidate == 1) %>%
      mutate(
        clinic_access_candidate = if_else(candidate == 1, "Y", "N"),
        clinic_access_prior_year = if_else(clinic_access_prior_year == 1, "Y", "N"),
        access_adj = if_else(clinic_label %in% input$clinic, "Y", "N"),
        clinic_costlevel_after_access = if_else(
          access_adj == "Y" &
            clinic_costlevel2020 > 2,
          2,
          clinic_costlevel2020
        ),
        afford_after_access = if_else(clinic_costlevel_after_access %in% 1:2, 1, 0),
        afford_after_access_25_miles = if_else(drive_dist <= 25 &
                                                 afford_after_access == 1, 1, 0),
        afford_after_access_30_miles = if_else(drive_dist <= 30 &
                                                 afford_after_access == 1, 1, 0),
        afford_after_access_35_miles = if_else(drive_dist <= 35 &
                                                 afford_after_access == 1, 1, 0)
      ) %>%
      group_by(worksite_label) %>%
      mutate(
        total_worksite_access_clinics_25_miles = sum(afford_after_access_25_miles),
        total_worksite_access_clinics_30_miles = sum(afford_after_access_30_miles),
        total_worksite_access_clinics_35_miles = sum(afford_after_access_35_miles)
      ) %>%
      ungroup() %>%
      select(
        worksite_name = worksite_label,
        worksite_FTE = worksite_fte,
        clinic_name = clinic_label,
        clinic_address = full_address,
        clinic_access_candidate,
        access_adjustment_prior_year = clinic_access_prior_year,
        original_cost_level = clinic_costlevel2020,
        driving_distance = drive_dist,
        access_adjustment = access_adj,
        cost_level_after_adjustments = clinic_costlevel_after_access,
        total_worksite_access_clinics_25_miles,
        total_worksite_access_clinics_30_miles,
        total_worksite_access_clinics_35_miles
      )
  })
  
  clinic_data_react <- reactive({
    ws_cl_crosswalk %>%
      filter(worksite_label == input$worksite &
               drive_dist <= 35) %>%
      select(clinic_label) %>%
      inner_join(clinics, by = "clinic_label") %>%
      mutate(
        clinic_fulltime = if_else(clinic_fulltime == 1, "Y", "N"),
        clinic_access_candidate = if_else(candidate == 1, "Y", "N"),
        clinic_access_prior_year = if_else(clinic_access_prior_year == 1, "Y", "N"),
        clinic_type = if_else(clinic_type == 1, "PCC", "not PCC")
      ) %>%
      arrange(desc(clinic_access_candidate), mmb_num) %>%
      select(
        clinic_name = clinic_label,
        clinic_address = full_address,
        clinic_access_candidate,
        clinic_fulltime,
        clinic_type,
        clinic_doctors,
        access_adjustment_prior_year = clinic_access_prior_year,
        original_cost_level = clinic_costlevel2020,
        contains('costlevel2019_'),
        contains('costlevel2020_'),
        contains('doctors_'),
        contains('care_system_'),
        contains('comments')
      ) %>%
      rename_at(vars(contains('costlevel2019')), list(~ str_replace(
        ., "costlevel2019", "cost_level_2019"
      ))) %>%
      rename_at(vars(contains('costlevel2020')), list(~ str_replace(
        ., "costlevel2020", "cost_level_2020"
      )))
  })
  
  worksite_data_react <- reactive({
    worksites %>%
      filter(worksite_label == input$worksite) %>%
      select(
        worksite = worksite_label,
        worksite_FTE = worksite_fte,
        location_agency = agency_name,
        location_number = location_nbr,
        location_name,
        location_FTE = location_fte
      )
  })
  
  peip_data_react <- reactive({
    peip_crosswalk %>% 
      mutate(
        access_adj = if_else(clinic_label %in% input$clinic, "Y", "N"),
        clinic_costlevel_after_access = if_else(
          access_adj == "Y" &
            clinic_costlevel2020 > 2,
          2,
          clinic_costlevel2020
        )) %>% 
      group_by(label) %>% 
      mutate(distance_to_access = min(drive_dist[clinic_costlevel_after_access %in% 1:2 & candidate == 1])) %>% 
      ungroup() %>% 
      distinct(label, .keep_all = TRUE) %>% 
      select(peip_label = label, group_name, full_address, cnty, employees, distance_to_access)
  })
  
  county_data_react <- reactive({
    ws_cl_crosswalk %>%
      filter(candidate == 1) %>%
      mutate(
        access_adj = if_else(clinic_label %in% input$clinic, "Y", "N"),
        clinic_costlevel_after_access = if_else(
          access_adj == "Y" &
            clinic_costlevel2020 > 2,
          2,
          clinic_costlevel2020
        ),
        afford_after_access = if_else(clinic_costlevel_after_access %in% 1:2, 1, 0),
        afford_after_access_25_miles = if_else(drive_dist <= 25 &
                                                 afford_after_access == 1, 1, 0),
        afford_after_access_30_miles = if_else(drive_dist <= 30 &
                                                 afford_after_access == 1, 1, 0),
        afford_after_access_35_miles = if_else(drive_dist <= 35 &
                                                 afford_after_access == 1, 1, 0)
      ) %>%
      group_by(worksite_label) %>% #calculate at worksite level
      mutate(
        worksite_access_clinics_25_miles = sum(afford_after_access_25_miles),
        worksite_access_clinics_25_miles = if_else(worksite_access_clinics_25_miles > 0 , "Y", "N"),
        worksite_access_clinics_30_miles = sum(afford_after_access_30_miles),
        worksite_access_clinics_30_miles = if_else(worksite_access_clinics_30_miles > 0 , "Y", "N"),
        worksite_access_clinics_35_miles = sum(afford_after_access_35_miles),
        worksite_access_clinics_35_miles = if_else(worksite_access_clinics_35_miles > 0 , "Y", "N")
      ) %>%
      ungroup() %>%
      distinct(worksite_label, .keep_all = TRUE) %>%
      select(
        worksite_label,
        worksite_fte,
        county_name,
        worksite_access_clinics_25_miles,
        worksite_access_clinics_30_miles,
        worksite_access_clinics_35_miles
      ) %>%
      group_by(county_name) %>%
      mutate(
        county_worksites = n(),
        total_FTE = sum(worksite_fte),
        FTE_no_access_25_miles = sum(worksite_fte[worksite_access_clinics_25_miles == "N"]),
        FTE_no_access_30_miles = sum(worksite_fte[worksite_access_clinics_30_miles == "N"]),
        FTE_no_access_35_miles = sum(worksite_fte[worksite_access_clinics_35_miles == "N"])
      ) %>%
      ungroup() %>%
      distinct(county_name, .keep_all = TRUE) %>%
      select(
        county = county_name,
        county_worksites,
        total_FTE,
        FTE_no_access_25_miles,
        FTE_no_access_30_miles,
        FTE_no_access_35_miles
      ) %>%
      mutate_at(vars(contains('FTE')), list(~ as.integer(.))) %>%
      adorn_totals(where = "row")
  })
  
  map_react <- reactive({
    bind_rows(
      ws_cl_crosswalk %>%
        filter(
          worksite_label == input$worksite &
            drive_dist <= 35 & candidate == 1
        ) %>%
        mutate(
          access_adj = if_else(clinic_label %in% input$clinic, "Y", "N"),
          clinic_costlevel_after_access = if_else(
            access_adj == "Y" &
              clinic_costlevel2020 > 2,
            2,
            clinic_costlevel2020
          )
        ) %>%
        distinct(clinic_label, .keep_all = TRUE) %>%
        select(
          label = clinic_label,
          lat = cl_lat,
          lon = cl_lon,
          drive_dist,
          costlevel = clinic_costlevel_after_access
        ) %>%
        mutate(type = "Clinic"),
      ws_cl_crosswalk %>%
        filter(worksite_label == input$worksite) %>%
        distinct(worksite_label, .keep_all = TRUE) %>%
        select(
          label = worksite_label,
          lat = ws_lat,
          lon = ws_lon
        ) %>%
        mutate(
          type = "Worksite",
          costlevel = NA,
          drive_dist = 0
        )
    )
  })
  
  statemap_react <- reactive({
    ws_cl_adjustments <- ws_cl_crosswalk %>%
      filter(drive_dist <= 35 & candidate == 1) %>%
      mutate(
        clinic_access_candidate = if_else(candidate == 1, "Y", "N"),
        clinic_access_prior_year = if_else(clinic_access_prior_year == 1, "Y", "N"),
        access_adj = if_else(clinic_label %in% input$clinic, "Y", "N"),
        clinic_costlevel_after_access = if_else(
          access_adj == "Y" &
            clinic_costlevel2020 > 2,
          2,
          clinic_costlevel2020
        ),
        afford_after_access = if_else(clinic_costlevel_after_access %in% 1:2, 1, 0),
        afford_after_access_25_miles = if_else(drive_dist <= 25 &
                                                 afford_after_access == 1, 1, 0),
        afford_after_access_30_miles = if_else(drive_dist <= 30 &
                                                 afford_after_access == 1, 1, 0),
        afford_after_access_35_miles = if_else(drive_dist <= 35 &
                                                 afford_after_access == 1, 1, 0)
      ) %>%
      group_by(worksite_label) %>%
      mutate(
        total_worksite_access_clinics_25_miles = sum(afford_after_access_25_miles),
        total_worksite_access_clinics_30_miles = sum(afford_after_access_30_miles),
        total_worksite_access_clinics_35_miles = sum(afford_after_access_35_miles),
        worksite_access = ifelse(
          total_worksite_access_clinics_25_miles > 0,
          "<25 miles",
          ifelse(
            total_worksite_access_clinics_30_miles > 0,
            "25-30 miles",
            ifelse(
              total_worksite_access_clinics_35_miles > 0,
              "30-35 miles",
              ">35 miles"
            )
          )
        ),
        clinic_costlevel_final = ifelse(
          access_adj == "Y",
          paste0(clinic_costlevel_after_access, "*"),
          as.character(clinic_costlevel_after_access)
        )
      ) %>%
      ungroup()
    
    bind_rows(
      ws_cl_adjustments %>%
        distinct(clinic_label, .keep_all = TRUE) %>%
        select(
          label = clinic_label,
          lat = cl_lat,
          lon = cl_lon,
          status = clinic_costlevel_final
        ) %>%
        mutate(type = "Clinic"),
      ws_cl_adjustments %>%
        distinct(worksite_label, .keep_all = TRUE) %>%
        select(
          label = worksite_label,
          lat = ws_lat,
          lon = ws_lon,
          status = worksite_access
        ) %>%
        mutate(type = "Worksite")
    ) %>%
      filter(!is.na(status)) %>%
      mutate(status = ifelse(
        type == "Clinic",
        paste0("Cost level: ", status),
        paste0("Access: ", status)
      )) %>%
      filter(status %in% input$statemap)
  })
  
  download_react <- reactive({
    input$clinic %>%
      enframe() %>%
      rename(row_number = name,
             clinic_label = value)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("access-adjustments_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(download_react(), file)
    }
  )
  
  output$workmap <- renderLeaflet({
    mapdf <- map_react()
    
    labs <- lapply(seq(nrow(mapdf)), function(i) {
      paste0(
        '<b>',
        mapdf[i, "label"],
        '</b>',
        '<br/>',
        'Type: ',
        mapdf[i, "type"],
        '<br/>',
        'Cost Level: ',
        mapdf[i, "costlevel"],
        '<br/>',
        'Distance: ',
        mapdf[i, "drive_dist"],
        ' miles',
        '<br/>'
      )
    })
    
    #https://icons8.com/icons/set/map-marker
    mapIcons <- icons(
      iconUrl = ifelse(
        mapdf$type == "Worksite",
        here::here("misc", "icons", "icons8-marker-48-blue.png"),
        ifelse(
          mapdf$costlevel == 1,
          here::here("misc", "icons", "icons8-marker-48-red1.png"),
          ifelse(
            mapdf$costlevel == 2,
            here::here("misc", "icons", "icons8-marker-48-red2.png"),
            ifelse(
              mapdf$costlevel == 3,
              here::here("misc", "icons", "icons8-marker-48-red3.png"),
              here::here("misc", "icons", "icons8-marker-48-red4.png")
            )
          )
        )
      ),
      iconWidth = 48,
      iconHeight = 48,
      iconAnchorX = 24,
      iconAnchorY = 47
    )
    
    mapdf_ws <-
      mapdf %>% filter(type == "Worksite") #pull worksite row to get coordinates for setView
    
    m <- mapdf %>%
      leaflet() %>%
      addTiles() %>%
      addCircles(
        lng = mapdf_ws$lon,
        lat = mapdf_ws$lat,
        weight = 1,
        radius = conv_unit(30, from = "mi", to = "m"),
        fillOpacity = 0.01
      ) %>%
      setView(lng = mapdf_ws$lon, lat = mapdf_ws$lat, 9) %>%
      addMarkers(
        lng = ~ lon,
        lat = ~ lat,
        popup = lapply(labs, HTML),
        icon = mapIcons
      )
    m
  })
  
  output$statemap <- renderLeaflet({
    statemapdf <- statemap_react()
    
    statelabs <- lapply(seq(nrow(statemapdf)), function(i) {
      paste0(
        '<b>',
        statemapdf[i, "label"],
        '</b>',
        '<br/>',
        'Type: ',
        statemapdf[i, "type"],
        '<br/>',
        'Status: ',
        statemapdf[i, "status"],
        '<br/>'
      )
    })
    
    #https://icons8.com/icons/set/map-marker
    statemapIcons <- icons(
      iconUrl = ifelse(
        statemapdf$status == "Cost level: 1",
        here::here("misc", "icons", "icons8-marker-48-red1.png"),
        ifelse(
          statemapdf$status %in% c("Cost level: 2", "Cost level: 2*"),
          here::here("misc", "icons", "icons8-marker-48-red2.png"),
          ifelse(
            statemapdf$status == "Cost level: 3",
            here::here("misc", "icons", "icons8-marker-48-red3.png"),
            ifelse(
              statemapdf$status == "Cost level: 4",
              here::here("misc", "icons", "icons8-marker-48-red4.png"),
              ifelse(
                statemapdf$status == "Access: <25 miles",
                here::here("misc", "icons", "icons8-marker-48-blue1.png"),
                ifelse(
                  statemapdf$status == "Access: 25-30 miles",
                  here::here("misc", "icons", "icons8-marker-48-blue2.png"),
                  ifelse(
                    statemapdf$status == "Access: 30-35 miles",
                    here::here("misc", "icons", "icons8-marker-48-blue3.png"),
                    here::here("misc", "icons", "icons8-marker-48-blue4.png")
                  )
                )
              )
            )
          )
        )
      ),
      iconWidth = 48,
      iconHeight = 48,
      iconAnchorX = 24,
      iconAnchorY = 47
    )
    
    state_m <- statemapdf %>%
      leaflet() %>%
      addTiles() %>%
      setView(lng = -94.200833, lat = 46.358056, 6) %>%
      addMarkers(
        lng = ~ lon,
        lat = ~ lat,
        popup = lapply(statelabs, HTML),
        icon = statemapIcons
      )
    state_m
  })
  
  output$access_table = DT::renderDataTable({
    access_df <- access_data_react()
    
    access_df %>%
      rename_all(list(~ str_replace_all(., "\\_", " ")))
  })
  
  output$clinic_table = DT::renderDataTable({
    clinic_df <- clinic_data_react()
    
    clinic_df %>%
      rename_all(list(~ str_replace_all(., "\\_", " ")))
  })
  
  output$worksite_table = DT::renderDataTable({
    worksite_df <- worksite_data_react()
    
    worksite_df %>%
      rename_all(list(~ str_replace_all(., "\\_", " ")))
  })
  
  output$peip_table = DT::renderDataTable({
    peip_df <- peip_data_react()
    
    DT::datatable(peip_df %>%
                    rename_all(list(~ str_replace_all(., "\\_", " "))),
                  options = list(pageLength = 400))
  })
  
  output$county_table = DT::renderDataTable({
    county_df <- county_data_react()
    
    DT::datatable(county_df %>%
                    rename_all(list(~ str_replace_all(., "\\_", " "))),
                  options = list(pageLength = 100))
  })
})