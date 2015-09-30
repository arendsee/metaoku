require(shiny)
require(shinythemes)
require(DT)
require(markdown)

if(file.exists('data/home.md')){
    home_tab <- 'data/home.md'
} else {
    home_tab <- 'defaults/home.md'
}

if(file.exists('data/help.md')){
    help_tab <- 'data/help.md'
} else {
    help_tab <- 'defaults/help.md'
}

# Define UI for dataset viewer application
shinyUI(
    fluidPage(
        theme = shinytheme('spacelab'),
        tabsetPanel(
            tabPanel('Home', shiny::includeMarkdown(home_tab)),
            tabPanel('Columns', sidebarLayout(
                sidebarPanel(
                    radioButtons('selected.dataset', 'Select a dataset', c('None' = 'none')),
                    uiOutput('dataset_description')
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
                                                  label='Enter ids',
                                                  value=NULL))),
                    downloadButton('downloadData', 'Download')
                ),
                mainPanel(DT::dataTableOutput("main_table"))
            )),
            tabPanel('Help', shiny::includeMarkdown(help_tab))
        )
    )
)
