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

if(file.exists('data/about.md')){
    about_tab <- 'data/about.md'
} else {
    about_tab <- 'defaults/about.md'
}

# Define UI for dataset viewer application
shinyUI(
    fluidPage(
        theme = shinytheme('spacelab'),
        tabsetPanel(
            tabPanel('Home', shiny::includeMarkdown(home_tab)),
            tabPanel('Select', sidebarLayout(
                sidebarPanel(
                    radioButtons('selected.dataset', 'Select a dataset', c('None' = 'none')),
                    uiOutput('dataset_description')
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
                    downloadButton('downloadData', 'Download')
                ),
                mainPanel(DT::dataTableOutput("main_table"))
            )),
            tabPanel('Plot', sidebarLayout(
                sidebarPanel(
                    fluidRow(
                        column(4, selectInput('x.axis', 'x-axis', choices='None')),
                        column(4, selectInput('y.axis', 'y-axis', choices='None')),
                        column(4, selectInput('z.axis', 'z-axis', choices='None'))
                    )
                ),
                mainPanel(
                    plotOutput('plot_data_plot', height='800px')
                )
            )),
            tabPanel('Analyze', sidebarLayout(
                sidebarPanel(
                    h1('STUB')
                ),
                mainPanel(
                    h1('STUB')
                )
            )),
            tabPanel('Upload', sidebarLayout(
                sidebarPanel(
                    h1('STUB')
                ),
                mainPanel(
                    h1('STUB')
                )
            )),
            tabPanel('Help', shiny::includeMarkdown(help_tab)),
            tabPanel('About',
                textOutput('version'),
                shiny::includeMarkdown(about_tab))
        )
    )
)
