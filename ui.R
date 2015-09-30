require(shiny)
require(shinythemes)
require(DT)
require(markdown)

if(file.exists('data/home.md')){
    home <- 'data/home.md'
} else {
    home <- 'defaults/home.md'
}

# Define UI for dataset viewer application
shinyUI(
    fluidPage(
        theme = shinytheme('spacelab'),
        tabsetPanel(
            tabPanel('Home', shiny::includeMarkdown(home)),
            tabPanel('Columns', sidebarLayout(
                sidebarPanel(
                    radioButtons('selected.dataset', 'Select a dataset', c('None' = 'none'))
                ),
                mainPanel(
                    DT::dataTableOutput("column_table")
                )
            )),
            tabPanel('Data', sidebarLayout(
                sidebarPanel(
                    plotOutput('plot'),
                    fluidRow(
                        column(2, checkboxInput('logx', 'log2 x-axis')),
                        column(2, checkboxInput('logy', 'log2 y-axis')),
                        column(4, selectInput('compare.to', 'Compare to', choices='None')),
                        column(4, selectInput('group.by', 'Group by', choices='None'))),
                    fluidRow(column(12, textInput(inputId='user_ids',
                                                  label='Enter ids (e.g. "AT5G28465.1 AT5G54910.1 AT5G58170.1")',
                                                  value=NULL))),
                    downloadButton('downloadData', 'Download')
                ),
                mainPanel(DT::dataTableOutput("main_table"))
            ))
        )
    )
)
