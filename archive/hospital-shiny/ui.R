# Define the user interface for the Shiny app

shinyUI(navbarPage("Hospital Dashboard",
                   tabPanel("Browser",
                            fluidPage(
                              fluidRow(
                                column(
                                  3,
                                  p(""),
                                  pickerInput(
                                    inputId = "state",
                                    label = "Choose a state: ",
                                    choices = state_list,
                                    selected = "MN",
                                    options = list(
                                      `actions-box` = TRUE,
                                      size = 10,
                                      `selected-text-format` = "count > 3"
                                    ),
                                    multiple = TRUE
                                  ),
                                  pickerInput(
                                    inputId = "year",
                                    label = "Choose a year",
                                    choices = year_list,
                                    selected = tail(year_list, 1),
                                    options = list(
                                      `actions-box` = TRUE,
                                      size = 10,
                                      `selected-text-format` = "count > 3"
                                    ),
                                    multiple = TRUE
                                  )
                                ),
                                column(9,
                                       leafletOutput('map'))),
                              fluidRow(
                                column(12,
                                       DT::dataTableOutput('table'))
                              )
                            )),
                   tabPanel("Pivot",
                            fluidPage(
                                  rpivotTableOutput("pivot")
                                )
                            )))