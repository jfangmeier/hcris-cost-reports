# Define the user interface for the Shiny app

shinyUI(navbarPage(
  "Access Tool",
  tabPanel("Access Adjuster",
           fluidPage(
             fluidRow(
               column(
                 3,
                 p(""),
                 materialSwitch(
                   inputId = "id",
                   label = "View all worksites",
                   status = "primary"
                 ),
                 pickerInput(
                   inputId = "worksite",
                   label = "Choose a worksite: ",
                   choices = worksite_list_25_miles,
                   options = list(
                     `actions-box` = TRUE,
                     size = 10,
                     `selected-text-format` = "count > 3"
                   ),
                   multiple = FALSE
                 ),
                 pickerInput(
                   inputId = "clinic",
                   label = "Choose a clinic to adjust: ",
                   choices = clinic_list_30_miles,
                   options = list(
                     `actions-box` = TRUE,
                     size = 10,
                     `selected-text-format` = "count > 3"
                   ),
                   multiple = TRUE
                 ),
                 downloadButton("downloadData", "Download Adjustments")
               ),
               column(9,
                      leafletOutput("workmap"))
             ),
             fluidRow(tabsetPanel(
               tabPanel(
                 "Worksite-Clinic Combinations",
                 DT::dataTableOutput("access_table")
               ),
               tabPanel("Clinic Detail", DT::dataTableOutput("clinic_table")),
               tabPanel("Worksite Detail", DT::dataTableOutput("worksite_table"))
             ))
           )),
  tabPanel("Statewide Map",
           fluidPage(
             sidebarLayout(sidebarPanel(
               pickerInput(
                 inputId = "statemap",
                 label = "Choose markers to display:",
                 choices = statewide_map_options,
                 options = list(
                   `actions-box` = TRUE,
                   size = 10,
                   `selected-text-format` = "count > 3"
                 ),
                 multiple = TRUE,
                 selected = statewide_map_options
               )
             ),
             mainPanel(
               leafletOutput("statemap", width = "100%", height = 700)
             ))
           )),
  tabPanel("County Table",
           DT::dataTableOutput("county_table")),
  tabPanel("PEIP Table",
           DT::dataTableOutput("peip_table"))
))
