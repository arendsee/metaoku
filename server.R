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
    cat('entering mergeByName()\n')
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
        cat('entering dat()\n')
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

    # Read input from textInput box, parse out ids, and if they are present in
    # the key column of the main dataset, return them
    user.rows <- reactive({
        cat('entering user.rows()\n')
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

    sel.nonreactive <- function(col.name, rows=input$main_table_rows_all){
        cat('entering sel.nonreactive()\n')

        if(length(col.name) > 0 && col.name %in% colnames(dat())){
            out <- data.frame(
                value=dat()[, col.name],
                selected=rep(FALSE, nrow(dat()))
            )
            out$selected[rows] <- TRUE
            out$group = ifelse(out$selected, 'selected', 'non-selected')
            cat('\tdim(out):', dim(out), '\n')
            cat('\tcolnames(out):', colnames(out), '\n')
            return(out)
        } else {
            return(NULL)
        }
    }

    sel <- reactive({
        cat('entering sel()\n')
        col.id  <- input$main_table_columns_selected + 1
        col.name <- colnames(dat())[col.id]
        rows  <- input$main_table_rows_all
        cat('leaving sel(), col.name = ', col.name, '\n')
        return(sel.nonreactive(col.name=col.name, rows=rows))
    })

    output$plot <- renderPlot({
        cat('entering renderPlot()\n')

        # Dataframe for selected column
        x = sel()
        x.name <- colnames(dat())[input$main_table_columns_selected + 1]

        if(is.null(x)){
            cat('\tnull in renderPlot\n')
            return()
        }

        cat('\there\n')

        # Dataframe for the column selected from the 'Compare to' dropdown, may be NULL
        y = sel.nonreactive(col.name=input$compare.to)
        y.name <- input$compare.to
        fmt.opts <- list(
            logy=input$logy,
            logx=input$logx
        )

        cat('\talso here\n')

        if(!any(x$selected)){
            cat('\tnothing selected:', nrow(x), length(input$main_table_rows_all), '\n')
            return()
        }

        cat('\tplotting\n')

        g <- plotAnything(
            x=x$value,
            y=y$value,
            group=x$group,
            selected=x$selected,
            x.name=x.name,
            y.name=y.name,
            fmt.opts=fmt.opts,
            corpa=global$corpa
        )
        return(g)
    })

    # Generate a summary of the dataset
    output$summary <- renderPrint({
        cat('entering output.summary.rendePrint()\n')
        summary(dat()[input$main_table_rows_all, ])
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
        cat('entering get_user_data()\n')
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
            select=7
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
