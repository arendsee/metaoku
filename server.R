require(shiny)
require(plyr)
require(ggplot2)
require(DT)
require(wordcloud) 
require(tm)
require(reshape2)

source('global.R')
source('statistics.R')
source('plotting-functions.R')

mergeByName <- function(dat, by.colname){
    cat('entering mergeByName()\n', stderr())
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
            setkeyv(dat, key)
            dat <- merge(dat, d[, c(key, by.colname), with=FALSE], by=key)
        }
    }
    return(dat)
}

shinyServer(function(input, output){
    dat <- reactive({
        cat('entering dat()\n', stderr())
        out <- data.table(model=as.vector(global$models), locus=as.vector(global$loci))
        for(column in input$columns){
            out <- mergeByName(out, column)
        }
        out <- data.frame(out)
        # This factoring is vital for filtering columns in DT
        for(i in 1:ncol(out)){
            if(length(unique(out[, i])) <= 20){
                out[, i] <- factor(out[, i])
            }
        }
        return(out)
    })

    # observe({
    #     updateSelectInput(session, "compare.to", choices = c('None', input$columns))
    # })


    # Read input from textInput box, parse out ids, and if they are present in
    # the key column of the main dataset, return them
    user.rows <- reactive({
        cat('entering user.rows()\n', stderr())
        txt <- input$user_ids
        ids <- gsub('[,;\\\'"\\\t|<>]+', ' ', txt) 
        ids <- unlist(strsplit(ids, '\\s+', perl=TRUE))
        if(input$key == "model"){
            all.ids = dat()$model
        } else if(input$key == "locus"){
            all.ids = dat()$locus
        }
        rows = which(all.ids %in% ids)
        return(rows)
    })

    sel.nonreactive <- function(cols=NULL, column.names=NULL, rows=input$main_table_rows_all){
        cat('entering sel.nonreactive()\n', stderr())
        if(is.null(cols) && ! is.null(column.names)){
            cols <- which(colnames(dat()) %in% column.names)
        }
        stopifnot(is.numeric(cols))
        d <- data.table(dat())
        if(!is.null(cols) && length(cols) > 0 && ncol(d) >= max(cols)){
            d <- d[, cols, with=FALSE]
            d$selected       <- FALSE
            d$selected[rows] <- TRUE
            d <- data.frame(d)
            d <- melt(d, id.vars=c('selected'), value.name='value')
            d$group = ifelse(d$selected, 'selected', 'non-selected')
            return(d)
        }
    }

    sel <- reactive({
        cat('entering sel()\n', stderr())
        cols  <- input$main_table_columns_selected + 1
        rows  <- input$main_table_rows_all
        return(sel.nonreactive(cols=cols, rows=rows))
    })


    # Generate a summary of the dataset
    output$summary <- renderPrint({
        cat('entering output.summary.rendePrint()\n', stderr())
        summary(dat()[input$main_table_rows_all, ])
    })

    refactor <- function(x, column.name){
        cat(sprintf('entering refactor() with (x=%s, column.name=%s)\n', class(x), column.name), stderr())
        if(is.factor(dat()[, column.name])){
            x = factor(x, levels=levels(dat()[, column.name]))
        }
        return(x)
    }

    output$plot <- renderPlot({
        cat('entering renderPlot()\n', stderr())

        # Dataframe for selected column
        x = sel()
        # Dataframe for the column selected from the 'Compare to' dropdown, may be NULL
        y = sel.nonreactive(column.names=input$compare.to)
        fmt.opts <- list(
            logy=input$logy,
            logx=input$logx
        )

        x.name <- levels(x$variable)[1]
        y.name <- levels(y$variable)[1]

        x$value <- refactor(x$value, x.name)
        if(!is.null(y)){
            y$value <- refactor(y$value, y.name)
        }
        g <- plotAnything(x=x, y=y,
                          x.name=x.name, y.name=y.name,
                          fmt.opts=fmt.opts,
                          corpa=global$corpa)
        return(g)
    })

    output$column_summary <- renderTable(
        columnSummary(sel()),
        include.rownames=FALSE
    )

    output$comparison_summary <- renderTable(
        comparisonSummary(d=sel()),
        include.rownames=FALSE
    )

    get_user_data <- function(){
        cat('entering get_user_data()\n', stderr())
        if(length(user.rows()) > 0){
            return(dat()[user.rows(), ])
        } else {
            return(dat())
        }
    }
    output$main_table <- DT::renderDataTable(
        get_user_data(),
        rownames=FALSE,
        filter='top',
        style='bootstrap',
        selection=list(
            mode='single',
            target='column',
            selected=7
        ),
        options = list(
            autoWidth=TRUE,
            orderMulti=TRUE,
            searching=TRUE,
            search.regex=TRUE
    ))

    output$downloadData <- downloadHandler(
        filename = 'arabidopsis-data.tsv',
        content = function(file) {
            write.table(dat()[input$main_table_rows_all, ], file, row.names=FALSE, sep="\t")
        }
    )
})
