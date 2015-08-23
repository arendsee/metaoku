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
        out = data.frame(out)
        # factorize non-numeric strings if less than 20 unique elements
        for(i in 1:ncol(out)){
            x = out[, i]
            if(! is.numeric(x) && length(unique(x)) <= 20){
                out[, i] = as.factor(x)
            }
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
        if(! is.null(columns) && length(columns) > 0){
            x = dat()[rows, columns]
            write(paste(class(x), is.numeric(x), is.character(x), is.factor(x)), 'log', append=TRUE)
            if(is.numeric(x)){
                g <- ggplot(data.frame(values=x)) +
                    geom_histogram(aes(x=values))
                return(g)
            }

            if(is.character(x) && ! is.factor(x)){
                s = sort(summary(factor(x)))
                if(length(s) > 20){
                    s[21] = sum(s[21:length(s)])
                    names(s) = c(names(s)[1:20], 'other')
                }
                x = factor(rep(names(s), times=as.numeric(s)))
            }

            if(is.factor(x)){
                g <- ggplot(data.frame(values=x)) +
                    geom_bar(aes(x=values)) +
                    theme(axis.text.x = element_text(angle=270, hjust=0, vjust=1))
                return(g)
            }
        }
    })

    output$main_table <- DT::renderDataTable(
        dat(),
        rownames=FALSE,
        filter='top',
        style='bootstrap',
        selection=list(
            mode='single',
            target='column',
            selected=5
        ),
        options = list(
            autoWidth=TRUE,
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
