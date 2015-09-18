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

eligible_for_compare <- function(x) {
    is.numeric(x) || length(unique(x)) <= 20
}

shinyServer(function(input, output, session){
    dat <- reactive({
        cat('entering dat()\n')
        columns <- global$metadata$column_name[input$column_table_rows_selected]
        columns <- unique(c(global$key, columns))
        out <- data.frame(global$table[, columns, with=FALSE])

        # This factoring is vital for filtering columns in DT
        for(i in 1:ncol(out)){
            if(length(unique(out[, i])) <= 20){
                out[, i] <- factor(out[, i])
            }
        }
        return(out)
    })

    observe({
        columns <- global$metadata$column_name[input$column_table_rows_selected]
        if(length(columns) > 0){
            for (i in length(columns):1){
                if(! eligible_for_compare(dat()[, columns[i]])){
                    columns <- columns[-i]
                }
            }
        }
        updateSelectInput(session, 'compare.to', choices=c('None', as.character(columns)))
    })
    

    # Read input from textInput box, parse out ids, and if they are present in
    # the key column of the main dataset, return them
    user.rows <- reactive({
        cat('entering user.rows()\n')
        if(length(input$user_ids) > 0){
            txt <- input$user_ids
            ids <- gsub('[,;\\\'"\\\t|<>]+', ' ', txt) 
            ids <- unlist(strsplit(ids, '\\s+', perl=TRUE))
            rows = which(dat()[, global$key] %in% ids)
            return(rows)
        } else {
            return(c())
        }
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
        return(sel.nonreactive(col.name=col.name, rows=rows))
    })

    output$plot <- renderPlot({
        cat('entering renderPlot()\n')

        # Dataframe for selected column
        x = sel()
        x.name <- colnames(dat())[input$main_table_columns_selected + 1]

        if(is.null(x)){ return() }

        # Dataframe for the column selected from the 'Compare to' dropdown, may be NULL
        y = sel.nonreactive(col.name=input$compare.to)
        y.name <- input$compare.to
        fmt.opts <- list(
            logy=input$logy,
            logx=input$logx
        )

        if(!any(x$selected)){ return() }

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
            target='column'
        ),
        options = list(
            autoWidth=TRUE,
            orderMulti=TRUE,
            searching=TRUE,
            search.regex=TRUE
    ))

    output$column_table <- DT::renderDataTable(
        {
            cat('entering column_table()\n')
            global$metadata
        },
        filter="none",
        rownames=FALSE,
        selection=list(
            mode='multiple',
            target='row'
        ),
        options=list(
            paging=FALSE,
            autoWidth=FALSE,
            scrollX=TRUE,
            scrollCollapse=FALSE,
            scrollY=FALSE,
            searching=FALSE,
            sorting=FALSE
    ))

    output$downloadData <- downloadHandler(
        filename = 'arabidopsis-data.tsv',
        content = function(file) {
            write.table(dat()[input$main_table_rows_all, ], file, row.names=FALSE, sep="\t")
        }
    )
})
