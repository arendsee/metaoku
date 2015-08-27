require(shiny)
require(shinythemes)
require(shinyBS)
require(DT)
source('global.R')

selected.columns <- c('GC', 'gene_length', 'confidence_overall', 'short_description', 'location', 'stratum_name')

# Define UI for dataset viewer application
shinyUI(
    fluidPage(theme = shinytheme('spacelab'),
    titlePanel("Arabidopsis thaliana data"),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(4, actionButton('selcol', 'Select Columns')),
                column(4, actionButton('summarize', 'Summarize')),
                column(4, downloadButton('downloadData', 'Download'))
            ),
            fluidRow(column(12, plotOutput('plot'))),
            fluidRow(column(3, checkboxInput('logx', 'log2 x-axis')),
                     column(6, selectInput('compare.to', 'Compare to', c('None', selected.columns)))),
            fluidRow(
                column(6, tableOutput('selection_summary_1')),
                column(6, tableOutput('selection_summary_2'))),
            fluidRow(column(8, textInput('user_ids', 'Enter ids (e.g. "AT5G28465 AT5G54910 AT5G58170")')),
                     column(4, radioButtons('key', 'Choose key',
                                            choices=list('Locus'='locus', 'Model'='model'),
                                            selected='locus')))
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
                                   selected=selected.columns))
        )
    )
))
