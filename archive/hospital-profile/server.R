shinyServer(function(input, output, session) {
    
    costreports_df_react <- reactive({
        costreports_df %>%
            filter(Number == substr(input$hospital, 1, 6))
    })
    
    observeEvent(input$state, {
        city_list <- input_list_df %>% filter(State == input$state) %>% pull(City) %>% unique() %>% sort()
        hospital_list <- input_list_df %>% filter(State == input$state) %>% pull(Hospital) %>% sort()
        
        updatePickerInput(
            session,
            inputId = "city",
            label = "Choose a city:",
            choices = city_list
        )
        
        updatePickerInput(
            session,
            inputId = "hospital",
            label = "Choose a hospital:",
            choices = hospital_list
        )
    }, ignoreInit = TRUE)
    
    observeEvent(input$city, {
        if (length(input$city) == 0){
            hospital_list <- input_list_df %>% filter(State == input$state) %>% pull(Hospital) %>% sort()
        } else {
            hospital_list <- input_list_df %>% filter(State == input$state & City %in% input$city) %>% pull(Hospital) %>% sort()
        }

        updatePickerInput(
            session,
            inputId = "hospital",
            label = "Choose a hospital:",
            choices = hospital_list
        )
    }, ignoreInit = TRUE)
    
    financial_tbl_gt <- reactive({
        costreports_df_react() %>%
            select(
                one_of(vars_table)
            ) %>%
            mutate_at(
                vars(
                    one_of(vars_dollar)
                ),
                ~ scales::dollar(.)
            ) %>%
            mutate_at(
                vars(
                    one_of(vars_percent)
                ),
                ~ scales::percent(.)) %>%
            mutate_at(
                vars(
                    one_of(vars_comma)
                ),
                ~ scales::comma(.)
            ) %>%
            gather(var, value, -Year) %>%
            spread(Year, value) %>%
            rename(Indicator = var) %>%
            inner_join(costreport_var_df %>% select(var_name, position), by = c("Indicator" = "var_name")) %>% 
            arrange(position) %>% 
            select(-position) %>% 
            gt() %>%
            tab_row_group(group = "Facility and Volume",
                          rows = Indicator %in% c("Beds", "Employees", "IP Bed Days", "IP Discharges")) %>% 
            tab_row_group(group = "Financial Summary",
                          rows = Indicator %in% c("Net Patient Revenue", "Net Income", "Operating Margin", "Excess Margin")) %>% 
            tab_row_group(group = "Safety Net",
                          rows = Indicator %in% c("DSH Funding", "Uncompensated Care Cost")) %>% 
            tab_row_group(group = "Medical Education",
                          rows = Indicator %in% c("Teaching Status", "Residents/Interns", "Medical Education Funding")) %>% 
            tab_options(
                table.background.color = "lightcyan"
            )
    })
    
    output$financial_table <- 
        render_gt(
            expr = financial_tbl_gt(),
            height = pct(100),
            width = pct(100)
        )
    
    output$ui_output <- renderUI({
        # generate ui based on input$species from ui.R
        
        costreports_df_react() %>% 
            tail(n = 1) %>% 
            select(name = Name,
                   number = Number,
                   type = Type,
                   address = Address) %>% 
            mutate(display = pmap(list(name, number, type, address),
                                  hospital_name_fcn)
            ) %>% 
            pull(display) 
    })
    
    output$map <- renderLeaflet({
        costreports_df_map <- costreports_df_react() %>% 
            tail(n = 1) %>% 
            select(`Hospital ID` = Number,
                   Year,
                   Hospital = Name,
                   Lat = Lat,
                   Long = Lon)
        
        map_lat <- costreports_df_map$Lat
        map_lon <- costreports_df_map$Long
        
        map_labs <- lapply(seq(nrow(costreports_df_map)), function(i) {
            paste0(
                '<b>',
                costreports_df_map[i, "Hospital"],
                '</b>',
                '<br/>',
                'CCN: ',
                costreports_df_map[i, "Hospital ID"],
                '<br/>'
            )
        })
        
        leaflet_map <- costreports_df_map %>%
            leaflet() %>%
            addTiles() %>%
            setView(lng = map_lon, lat = map_lat, zoom = 10) %>%
            addMarkers(
                lng = ~ Long,
                lat = ~ Lat,
                popup = lapply(map_labs, HTML)
            )
        leaflet_map
    })
    
})
