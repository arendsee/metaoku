require(shiny)
require(shinythemes)
require(shinyBS)
require(DT)
source('global.R')

homehtml <- '
<h1>Welcome to my <i>Arabidopsis thaliana</i> data portal</h1>
<ul>
    <li>To choose data for analysis and view column info, go to <strong>Columns</strong> tab</li>
    <li>To view, subset and plot data, go to <strong>Data</strong> tab. In this window you can
    <ul>
        <li> View a table of raw data </li>
        <li> Click on a column to plot it </li>
        <li> Create subsets of the data by adding filters to the columns or loading a list of identifiers.
        The subset of the data will be contrasted to the full data in every plot.</li>
    </ul>
</ul>
If you have any questions write Zebulun Arendsee at <a href=arendsee@iastate.edu>arendsee@iastate.edu</a>'

# Define UI for dataset viewer application
shinyUI(
    fluidPage(
        theme = shinytheme('spacelab'),
        tabsetPanel(
            tabPanel('Home', tags$div(HTML(homehtml))),     
            tabPanel('Columns', DT::dataTableOutput("column_table")),
            tabPanel('Data', sidebarLayout(
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
                mainPanel(
                    DT::dataTableOutput("main_table"))
                ))
            )
        )
)
