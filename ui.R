require(shiny)
source('global.R')

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title.
  titlePanel("Arabidopsis thaliana data"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view. The helpText function is
  # also used to include clarifying text. Most notably, the
  # inclusion of a submitButton defers the rendering of output
  # until the user explicitly clicks the button (rather than
  # doing it immediately when inputs change). This is useful if
  # the computations required to render output are inordinately
  # time-consuming.
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("columns",
                         "Choose columns to include", 
                         colnames(model.data),
                         selected=colnames(model.data)),
      
      numericInput("obs", "Number of observations to view:", 10),
      
      fluidRow(
        column(6, submitButton("Update View")),
        column(6, downloadButton('downloadData', 'Download'))
      )
      #helpText("Note: ")
    ),
    
    # Show a summary of the dataset and an HTML table with the
    # requested number of observations. Note the use of the h4
    # function to provide an additional header above each output
    # section.
    mainPanel(
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      h4("Observations"),
      tableOutput("view")
    )
  )
))
