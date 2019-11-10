#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application
ui = fluidPage(
    titlePanel("Basic DataTable"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(4,
               selectInput("state",
                           "State:",
                           c("All",
                             unique(as.character(teaching$State))))
        ),
        column(4,
               selectInput("city",
                           "City:",
                           c("All",
                             unique(as.character(teaching$City))))
        ),
        column(4,
               selectInput("year",
                           "Year:",
                           c("All",
                             unique(as.character(teaching$Year))))
        )
    ),
    # Create a new row for the table.
    DT::dataTableOutput("table")
)

# Define server logic
server = function(input, output) {
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- teaching
        if (input$state != "All") {
            data <- data[data$State == input$state,]
        }
        if (input$city != "All") {
            data <- data[data$City == input$city,]
        }
        if (input$year != "All") {
            data <- data[data$Year == input$year,]
        }
        data
    }))
}

# Run the application 
shinyApp(ui = ui, server = server)
