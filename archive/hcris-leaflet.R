library(tidyverse)
library(glue)
library(leaflet)
library(htmltools)
library(shiny)
library(DT)
library(here)

cost_reports <- readRDS(file.path("data","costreports.rds"))
state_data <- readRDS(file.path("data","state-data.rds"))

# Set value for the minZoom and maxZoom settings.
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

ui <- fluidPage(
  fluidRow(
    column(2, 
           selectInput("state", "Choose a state:",
                       list(`New England` = list("CT", "MA", "ME", "NH", "RI", "VT"),
                            `Mid-Atlantic` = list("NJ", "NY", "PA"),
                            `East North Central` = list("IL", "IN", "MI", "OH", "WI"),
                            `West North Central` = list("IA", "KS", "MN", "MO", "NE", "ND", "SD"),
                            `South Atlantic` = list("DC", "DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV"),
                            `East South Central` = list("AL", "KY", "MS", "TN"),
                            `West South Central` = list("AR", "LA", "OK", "TX"),
                            `Mountain` = list("AZ", "CO", "ID", "MT", "NM", "NV", "UT", "WY"),
                            `Pacific` = list("AK", "CA", "HI", "OR", "WA")),
                            selected = NULL
           ),
           selectInput("year", "Choose a year:",
                       2012:2017,
                       selected = 2017)
  ),
  column(10,
         leafletOutput("mymap")
         )
  ),
  fluidRow(
    DT::dataTableOutput("mytable")
  )
)

server <- function(input,output, session){
  
  data_react <- reactive({
    cost_reports %>% dplyr::filter(state == input$state & year == input$year)
  })
  
  map_bounds_react <- reactive({
    state_data %>% dplyr::filter(state == input$state)
  })
  
  output$mymap <- renderLeaflet({
    df <- data_react()
    
    bounds <- map_bounds_react()
    
    labs <- lapply(seq(nrow(df)), function(i) {
      paste0( '<b>', str_to_title(df[i, "hospital_name"]), '</b>', '<br/>', 
              'Total Beds: ', df[i, "beds_total"], '<br/>', 
              'Teaching Status: ', df[i, "teaching"], '<br/>', 
              'CMS Number: ', df[i, "prvdr_num"] ) 
    })
    
    m <- df %>% 
      leaflet() %>%
      addTiles() %>% 
      fitBounds(bounds$west, bounds$south, bounds$east, bounds$north) %>% 
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       stroke = FALSE, 
                       radius = ~sqrt(beds_total),
                       fillOpacity = 0.5,
                       popup = lapply(labs, HTML))
    m
  })
  
  output$mytable = DT::renderDataTable({
    df <- data_react()
    
    df %>% 
      select(hospital_name, city, state, year, type, teaching)
    
  })
}

shinyApp(ui, server)

labs <- lapply(seq(nrow(cost_reports)), function(i) {
  paste0( '<b>', str_to_title(cost_reports[i, "hospital_name"]), '</b>', '<br/>', 
          'Total Beds: ', cost_reports[i, "beds_total"], '<br/>', 
          'Teaching Status: ', cost_reports[i, "teaching"], '<br/>', 
          'CMS Number: ', cost_reports[i, "prvdr_num"] ) 
})

cost_reports %>% 
  mutate(labs = paste(str_to_title(hospital_name),"<br>",
                       beds_total," beds")) %>% 
  leaflet() %>% 
  addTiles() %>% 
  setView(-77.0147, 38.9101, 12) %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat,
                   stroke = FALSE, 
                   radius = ~sqrt(beds_total),
                   fillOpacity = 0.5,
                   popup = lapply(labs, HTML))

64.0685	-152.2782 AK 4
38.9101	-77.0147 DC 12

#https://stackoverflow.com/questions/43144596/r-and-leaflet-how-to-arrange-label-text-across-multiple-lines
#http://rpubs.com/bhaskarvk/electoral-Map-2016
#https://www.r-graph-gallery.com/4-tricks-for-working-with-r-leaflet-and-shiny/
#https://stackoverflow.com/questions/50731853/how-to-change-circle-marker-attributes-e-g-opacity-fillopacity-within-mapvi?rq=1

