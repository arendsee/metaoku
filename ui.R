source('config')

home_panel <- tabPanel(
    'Home',
    shiny::includeMarkdown(config$home_tab)
)

select_panel <- tabPanel(
    'Select',
    sidebarLayout(
        sidebarPanel(
            radioButtons('selected.dataset', 'Select a dataset', c('None' = 'none')),
            uiOutput('dataset_description'),
            downloadButton('downloadProject', 'Download')
        ),
        mainPanel(DT::dataTableOutput("column_table"))
    )
)

view_panel <- tabPanel(
    'View',
    sidebarLayout(
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
                column(4, actionButton('viewtab_plotly_popup', 'use plotly')),
                column(4, downloadButton('downloadData', 'Download Data')),
                column(4, downloadButton('downloadMagicPlot', 'Download Plot')),
                mainPanel(
                  bsModal(
                    id = "viewtab_plotly_bsModal",
                    title = "Your Plotly Graphic",
                    trigger = "viewtab_plotly_popup",
                    size = "large",
                    plotlyOutput("view_plot_plotly"),
                    downloadButton('downloadPlot', 'Download')
                  )
                )
            )
        ),
        mainPanel(DT::dataTableOutput("main_table"))
    )
)

plot_panel <- tabPanel(
    'Plot',
    sidebarLayout(
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
    )
)

upload_panel <- tabPanel(
    'Upload',
    sidebarLayout(
        sidebarPanel(
            fileInput('upload.file', 'Filename', multiple=TRUE),
            radioButtons('upload.type', 'Select upload type',
                         choices=list('Single Table'='single', 'Dataset'='dataset'),
                         selected='single'),
            textOutput('upload.status')
        ),
        mainPanel(
            uiOutput('upload.instructions')
        )
    )
)

help_panel <- tabPanel(
    'Help',
    shiny::includeMarkdown(config$help_tab)
)

about_panel <- tabPanel(
    'About',
    shiny::includeMarkdown('VERSION'),
    shiny::includeMarkdown(config$about_tab)
)

tabs <- list(
    home_panel,
    select_panel,
    view_panel,
    upload_panel,
    help_panel,
    about_panel
)

if(config$access == 'archive') {
  # No upload panel for archive instances
  tabs[[5]] = NULL
}

cat(config$access, '----\n')
if(config$access == 'anarchy'){
    tabs$upload <- NULL
}
cat(config$access, '2----\n')
tabset.panel <- do.call(tabsetPanel, tabs)

# Define UI for dataset viewer application
shinyUI(
    fluidPage(
        theme = shinytheme('spacelab'),
        tabset.panel
    )
)
