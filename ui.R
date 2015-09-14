require(shiny)
require(shinythemes)
require(shinyBS)
require(DT)
source('global.R')

homehtml <- '
<h1>Welcome to my data portal (not yet named)</h1>
<h2>Usage</h2>
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
<h2>Contact</h2>
<div>Zebulun Arendsee</div>
<div>Iowa State University</div>
<div>532 Science II</div>
<div>Ames, IA 50010</div>
<div>Email: <a href=arendsee@iastate.edu>arendsee@iastate.edu</a></div>
<h2>Citation</h2>
Zebulun Arendsee, Ling Li, and Eve Syrkin Wurtele. "Coming of age: orphan genes in plants." Trends in plant science 19.11 (2014): 698-708.'

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
                        column(6, selectInput('compare.to', 'Compare to', choices='None'))),
                    fluidRow(
                        column(6, tableOutput('column_summary')),
                        column(6, tableOutput('comparison_summary'))),
                    fluidRow(column(12, textInput('user_ids', 'Enter ids (e.g. "AT5G28465 AT5G54910 AT5G58170")'))),
                    downloadButton('downloadData', 'Download')
                ),
                mainPanel(
                    DT::dataTableOutput("main_table"))
                ))
            )
        )
)
