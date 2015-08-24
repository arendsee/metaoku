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
            column(4, actionButton('selcol', 'Select Columns')),
            column(4, actionButton('summarize', 'Summarize')),
            column(4, downloadButton('downloadData', 'Download')),
            br(), 
            br(),
            column(12, plotOutput('plot'))
        ),

        # Show a summary of the dataset and an HTML table with the
        # requested number of observations. Note the use of the h4
        # function to provide an additional header above each output
        # section.
        mainPanel(
            DT::dataTableOutput("main_table"),
            bsModal("summaryBox", "Summary", "summarize", size="large", verbatimTextOutput("summary")),
            bsModal("columnBox", "Select Columns", "selcol", size="large",
                checkboxGroupInput("columns",
                                   "Choose columns to include", 
                                   global$columns,
                                   selected=c('GC',
                                              'gene_length',
                                              'short_description',
                                              'location',
                                              'stratum_name')))
        )
    )
))
