# Define the user interface for the Shiny app

shinyUI(fluidPage(
    fluidRow(
        column(
            3,
            p(""),
            pickerInput(
                inputId = "type",
                label = "Select reporting format:",
                choices = c("Hospital Fiscal Year (as reported)", "Synthetic Calendar Year (experimental)"),
                selected = "Hospital Fiscal Year (as reported)",
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                ),
                multiple = FALSE
            ),
            pickerInput(
                inputId = "state",
                label = "Choose a state:",
                choices = state_list,
                selected = "MN",
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                ),
                multiple = FALSE
            ),
            pickerInput(
                inputId = "city",
                label = "Choose a city (optional):",
                choices = city_list,
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                ),
                multiple = TRUE
            ),
            pickerInput(
                inputId = "hospital",
                label = "Choose a hospital:",
                choices = hospital_list,
                selected = head(hospital_list, 1),
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                ),
                multiple = FALSE
            )
        ),
        column(4,
               shiny::uiOutput("ui_output")),
        column(5,
               leafletOutput('map', height = "300px"))
    ),
    fluidRow(column(
        12,
        gt::gt_output('financial_table')
    ))
))
