require(shiny)
require(shinythemes)
require(shinyBS)
require(DT)
source('global.R')

# Define UI for dataset viewer application
shinyUI(
    fluidPage(theme = shinytheme('spacelab'),
    titlePanel("Arabidopsis thaliana data"),
    sidebarLayout(
        sidebarPanel(
            column(3, actionButton('selcol', 'Select Columns')),
            column(3, actionButton('summarize', 'Summarize')),
            column(3, downloadButton('downloadData', 'Download'))
        ),
        
        # Show a summary of the dataset and an HTML table with the
        # requested number of observations. Note the use of the h4
        # function to provide an additional header above each output
        # section.
        mainPanel(
            h4("Observations"),
            DT::dataTableOutput("table"),
            bsModal("summaryBox", "Summary", "summarize", size="large", verbatimTextOutput("summary")),
            bsModal("columnBox", "Select Columns", "selcol", size="large",
                checkboxGroupInput("columns",
                                   "Choose columns to include", 
                                   cols,
                                   selected=c('locus', 'model', 'start', 'end', 'GC')))
        )
    )
))
