library(ggplot2)

function(input, output) {
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- cr_append
        if (input$control != "All") {
            data <- data[data$control == input$control,]
        }
        if (input$teaching != "All") {
            data <- data[data$teaching == input$teaching,]
        }
        if (input$fy != "All") {
            data <- data[data$fy == input$fy,]
        }
        data
    }))
    
}
