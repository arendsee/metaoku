require(shiny)
require(DT)
source('global.R')

mergeByName <- function(dat, by.colname){
    # iterate through every dataset
    # datalist is a global variable defined in merge.R

    if(is.null(by.colname)){
        return(dat)
    }

    for(d in datalist){
        idx = colnames(d)[1]
        if(by.colname %in% colnames(d)){
            if('model' %in% colnames(d)){
                key = 'model'
            } else if ('locus' %in% colnames(d)) {
                key = 'locus'
            } else {
                return(dat) 
            }
               dat <- merge(dat, d[, c(key, by.colname)], by=key) 
        }
    }
    return(dat)
}

shinyServer(function(input, output) {
dat <- reactive({
    out <- data.frame(model=models, locus=sub('\\.\\d+', '', models))
    for(column in input$columns){
        out <- mergeByName(out, column)
    }
    return(out)
})

# Generate a summary of the dataset
output$summary <- renderPrint({
    summary(dat()[input$main_table_rows_all, ])
})

output$main_table <- DT::renderDataTable(
    dat,
    rownames=FALSE,
    filter='bottom',
    options = list(
        autoWidth=TRUE
    )
)


output$downloadData <- downloadHandler(
    filename = 'arabidopsis-data.tsv',
    content = function(file) {
        write.table(dat()[input$main_table_rows_all, ], file, row.names=FALSE, sep="\t")
    })
})
