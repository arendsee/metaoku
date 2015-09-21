require(shiny)
require(DT)

source('load.R')
source('statistics.R')
source('plot.R')
source('dispatch.R')

shinyServer(function(input, output, session){
    dat <- reactive({
        cat('entering dat()\n')
        columns <- global$metadata$column_name[input$column_table_rows_selected]
        columns <- unique(c(global$key, columns))
        # assert all fields in the metadata are in the actual data
        stopifnot(columns %in% names(global$table))
        out <- data.frame(global$table[, columns, with=FALSE])
        return(out)
    })

    observe({
        # set compare.to (y) choices
        columns <- global$metadata$column_name[input$column_table_rows_selected]
        updateSelectInput(session, 'compare.to', choices=c('None', as.character(columns)))
        # set group.by (z) choices (this may not be cor or seq)
        columns <- columns[global$type[columns] %in% c('cor', 'seq')]
        updateSelectInput(session, 'group.by', choices=c('None', 'Selection', as.character(columns)))
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

    selected.column.name <- reactive({
        cat('entering selected.column.name()\n')
        cols <- colnames(dat())
        i <- input$main_table_columns_selected
        if(is.null(i)){
            return(NULL)
        } else {
            return(cols[i + 1])
        }
    })

    selection <- reactive({
        cat('entering selected.row.indices()\n')
        selection <- rep(FALSE, nrow(dat()))
        selection[input$main_table_rows_all] <- TRUE
        return(selection)
    })

    output$plot <- renderPlot({
        cat('entering renderPlot()\n')

        prepare.axis <- function(aname){
            a <- list()
            a$name    <- aname
            a$values  <- dat()[[a$name]]
            a$defined <- length(a$values > 0)
            if(aname %in% names(global$type)){
                a$type <- global$type[aname]
            } else {
                a$type <- '-'
            }
            if(a$type == 'cor'){
                a$mat <- global$corpa[[a$name]]
            }
            return(a)
        }

        get.column.selection <- function(a){
            cat('entering get.column.selection()\n')
            if(a$type == 'cor'){
                a$mat <- a$mat[selection(), ]
            }
            a$values <- a$values[selection()]
            return(a)
        }

        cname <- selected.column.name()
        if(is.null(cname)){
            return(NULL)
        }
        
        # x-axis comes from the selected column
        x <- prepare.axis(cname)

        # y-axis currently comes from 'Compare to' dropdown, may be NULL
        y <- prepare.axis(input$compare.to)

        # z-axis from 'Group by' dropdown
        z <- prepare.axis(input$group.by)

        if(z$name == 'Selection'){
            k <- sum(selection())
            if(k > 0 && k < nrow(dat())){
                z$values <- as.factor(ifelse(selection(), 'selected', 'unselected'))
            } else {
                z$type = '-'
            }
        } else if(sum(selection()) > 0){
            x <- get.column.selection(x)
            y <- get.column.selection(y)
            z <- get.column.selection(z)
        }

        fmt.opts <- list(
            logy=input$logy,
            logx=input$logx
        )

        plotAnything(x=x, y=y, z=z, fmt.opts=fmt.opts, corpa=global$corpa)
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
