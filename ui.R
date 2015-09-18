require(shiny)
require(shinythemes)
require(shinyBS)
require(DT)
source('global.R')

if(file.exists('home.html')){
    home.html <- paste(readLines('home.html'), collapse='')
} else {
    home.html <- "'home.html' not found"
}

# Define UI for dataset viewer application
shinyUI(
    fluidPage(
        theme = shinytheme('spacelab'),
        tabsetPanel(
            tabPanel('Home', tags$div(shiny::HTML(home.html))),
            tabPanel('Columns', DT::dataTableOutput("column_table")),
            tabPanel('Data', sidebarLayout(
                sidebarPanel(
                    plotOutput('plot'),
                    fluidRow(
                        column(2, checkboxInput('logx', 'log2 x-axis')),
                        column(2, checkboxInput('logy', 'log2 y-axis')),
                        column(6, selectInput('compare.to', 'Compare to', choices='None'))),
                    fluidRow(
                        column(6, tableOutput('column_summary')),
                        column(6, tableOutput('comparison_summary'))),
                    fluidRow(column(12, textInput('user_ids', 'Enter ids (e.g. "AT5G28465.1 AT5G54910.1 AT5G58170.1")'))),
                    downloadButton('downloadData', 'Download')
                ),
                mainPanel(
                    DT::dataTableOutput("main_table"))
                ))
            )
        )
)
