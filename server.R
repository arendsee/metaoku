require(shiny)
require(ggplot2)
require(DT)
source('global.R')

mergeByName <- function(dat, by.colname){
    if(is.null(by.colname)){
        return(dat)
    }

    # iterate through every dataset
    for(d in global$datasets){
        if(by.colname %in% colnames(d)){
            if('model' %in% colnames(d)){
                key = 'model'
            } else if ('locus' %in% colnames(d)) {
                key = 'locus'
            } else {
                return(dat) 
            }
            write(paste(key, colnames(d)), 'log', append=TRUE)
            setkeyv(dat, key)
            # dat <- dat[d[, c(key, by.colname), with=FALSE]]
            dat <- merge(dat, d[, c(key, by.colname), with=FALSE], by=key)
        }
    }
    return(dat)
}

shinyServer(function(input, output){
    dat <- reactive({
        out <- data.table(model=as.vector(global$models), locus=as.vector(global$loci))
        for(column in input$columns){
            out <- mergeByName(out, column)
        }
        return(out)
    })

    # Generate a summary of the dataset
    output$summary <- renderPrint({
        summary(dat()[input$main_table_rows_all, ])
    })

    output$plot <- renderPlot({
        columns = input$main_table_columns_selected + 1
        rows = input$main_table_rows_all
        if(! is.null(columns)){
            d = data.frame(dat()[rows, columns, with=FALSE])
            if(ncol(d) == 1){
                x = d[, 1]
                if(is.numeric(x)){
                    ggplot(data.frame(value=x)) +
                        geom_histogram(aes(value))
                } else {
                    if(!is.factor(x)){
                        x = as.factor(x)
                    }
                    s = sort(summary(x))
                    if(length(s) > 20){
                        s[21] = sum(s[21:length(s)])
                        names(s) = c(names(s)[1:20], 'other')
                    }
                    s.dt = data.frame(variables=rep(names(s), times=as.numeric(s)))
                    ggplot(s.dt) +
                        geom_bar(aes(factor(variables))) +
                        theme(axis.text.x = element_text(angle=270, hjust=0, vjust=1))
                }
            }
        }
    })

    output$main_table <- DT::renderDataTable(
        dat(),
        rownames=FALSE,
        filter='top',
        selection=list(
            mode='single',
            target='column',
            selected=5
        ),
        options = list(
            autoWidth=TRUE,
            scrollX=TRUE,
            orderMulti=TRUE,
            searching=TRUE,
            search.regex=TRUE
        )
    )

    output$downloadData <- downloadHandler(
        filename = 'arabidopsis-data.tsv',
        content = function(file) {
            write.table(dat()[input$main_table_rows_all, ], file, row.names=FALSE, sep="\t")
        }
    )
})
