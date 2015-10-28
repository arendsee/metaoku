require(shiny)
require(shinythemes)
require(DT)
require(markdown)

source('config')

# Define UI for dataset viewer application
shinyUI(
    fluidPage(
        theme = shinytheme('spacelab'),
        tabsetPanel(
            tabPanel('Home', shiny::includeMarkdown(config$home_tab)),
            tabPanel('Select', sidebarLayout(
                sidebarPanel(
                    radioButtons('selected.dataset', 'Select a dataset', c('None' = 'none')),
                    uiOutput('dataset_description'),
                    downloadButton('downloadProject', 'Download')
                ),
                mainPanel(
                    DT::dataTableOutput("column_table")
                )
            )),
            tabPanel('View', sidebarLayout(
                sidebarPanel(
                    plotOutput('view_data_plot'),
                    fluidRow(
                        column(2, checkboxInput('logx', 'log2 x-axis')),
                        column(2, checkboxInput('logy', 'log2 y-axis')),
                        column(4, selectInput('compare.to', 'Compare to', choices='None')),
                        column(4, selectInput('group.by', 'Group by', choices='None'))),
                    fluidRow(
                        column(8, textInput(inputId='user_ids',
                                            label='Enter ids',
                                            value=NULL)),
                        column(4, selectInput('user_key', 'Select Key', choices='None'))),
                    fluidRow(
                        column(4, downloadButton('downloadData', 'Download Data')),
                        column(4, downloadButton('downloadMagicPlot', 'Download Plot'))
                    )
                ),
                mainPanel(DT::dataTableOutput("main_table"))
            )),
            tabPanel('Plot', sidebarLayout(
                sidebarPanel(
                    uiOutput('plot.sidebar'),
                    fluidRow(
                        column(4, actionButton('build.plot', 'Plot')),
                        column(4, downloadButton('downloadPlot', 'Download Plot'))
                    )
                ),
                mainPanel(
                    plotOutput('plot_data_plot', height='800px')
                )
            )),
            tabPanel('Upload', sidebarLayout(
                sidebarPanel(
                    fileInput('upload.file', 'Filename', multiple=TRUE),
                    radioButtons('upload.type', 'Select upload type',
                                 choices=list('Single Table'='single', 'Dataset'='dataset')),
                    textOutput('upload.status')
                ),
                mainPanel(
                    uiOutput('upload.instructions')
                )
            )),
            tabPanel('Help', shiny::includeMarkdown(config$help_tab)),
            tabPanel('About',
                shiny::includeMarkdown('VERSION'),
                shiny::includeMarkdown(config$about_tab))
        )
    )
)
