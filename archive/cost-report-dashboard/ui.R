library(ggplot2)

fluidPage(
    titlePanel("Basic DataTable"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(4,
               selectInput("control",
                           "Control:",
                           c("All",
                             unique(as.character(cr_append$control))))
        ),
        column(4,
               selectInput("teaching",
                           "Teaching:",
                           c("All",
                             unique(as.character(cr_append$teaching))))
        ),
        column(4,
               selectInput("fy",
                           "Fiscal Year:",
                           c("All",
                             unique(as.character(cr_append$fy))))
        )
    ),
    # Create a new row for the table.
    DT::dataTableOutput("table")
)
