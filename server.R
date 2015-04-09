require(shiny)
source('global.R')

shinyServer(function(input, output) {

  dat <- reactive({ model.data[, input$columns, with=FALSE] })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    summary(dat())
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(dat(), n=input$obs)
  })

  output$downloadData <- downloadHandler(
    filename = 'arabidopsis-data.tsv',
    content = function(file) {
      write.table(dat(), file)
    }
  )

})
