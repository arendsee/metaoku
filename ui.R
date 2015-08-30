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
            plotOutput('plot'),
            fluidRow(
                column(2, checkboxInput('logx', 'log2 x-axis')),
                column(2, checkboxInput('logy', 'log2 y-axis')),
                column(6, selectInput('compare.to', 'Compare to', c('None', global$selected.columns)))),
            fluidRow(
                column(6, tableOutput('column_summary')),
                column(6, tableOutput('comparison_summary'))),
            fluidRow(column(8, textInput('user_ids', 'Enter ids (e.g. "AT5G28465 AT5G54910 AT5G58170")')),
                     column(4, radioButtons('key', 'Choose key',
                                            choices=list('Locus'='locus', 'Model'='model'),
                                            selected='locus'))),
            downloadButton('downloadData', 'Download')
        ),
        

        # Show a summary of the dataset and an HTML table with the
        # requested number of observations. Note the use of the h4
        # function to provide an additional header above each output
        # section.
        mainPanel(
            tabsetPanel(
                    tabPanel('Columns', DT::dataTableOutput("column_table")),
                    tabPanel('Data', DT::dataTableOutput("main_table"))
            )
        )
    )
))
