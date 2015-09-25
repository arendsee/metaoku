require(shiny)
require(DT)

source('load.R')
source('dispatch.R')

shinyServer(function(input, output, session){

    # =========================================================================
    # Update working dataset when columns are selected from the Column Table
    # 1. get the column names from the rows of the Column Table (which is built
    #    based on the METADATA file)
    # 2. return the UNIQUE rows
    #    * NOTE: this breaks the 1-to-1 correlation between row number and row
    #      content, so rows must be accessed by KEY when linking out.
    # =========================================================================
    dat <- reactive({
        cat('entering dat()\n')
        columns <- global$metadata$column_name[input$column_table_rows_selected]
        # the key column should always be the first column in the data table
        columns <- unique(c(global$key, columns))
        # assert all fields in the metadata are in the actual data
        stopifnot(columns %in% names(global$table))
        out <- unique(global$table[, columns, with=FALSE])
        return(out)
    })



    # =========================================================================
    # Record column names as they are selected from the Column Table
    #  * Update choices in the "Compare to" menu
    #  * Update choices in the "Group by" menu
    #    - limit options to datatypes that can be cast as categorical
    # =========================================================================
    observe({
        # set compare.to (y) choices
        columns <- global$metadata$column_name[input$column_table_rows_selected]
        updateSelectInput(session, 'compare.to', choices=c('None', as.character(columns)))
        # set group.by (z) choices (this may not be cor or seq)
        columns <- columns[global$type[columns] %in% c('cat', 'num', 'longcat')]
        updateSelectInput(session, 'group.by', choices=c('None', 'Selection', as.character(columns)))
    })
    


    # =========================================================================
    # Read input from textInput box, extract ids, and return if they are
    # present in the key column of the main dataset
    # =========================================================================
    user.keys <- reactive({
        cat('entering user.rows()\n')
        # this prevents whitespace in the box from stopping plotting
        txt <- gsub('\n', ' ', input$user_ids)
        txt <- sub('^\\s+', '', txt, perl=TRUE)
        if(nchar(txt) > 0){
            cat(' * >', txt, '<\n')
            ids <- gsub('[,;\\\'"\\\t|<>]+', ' ', txt) 
            ids <- unlist(strsplit(ids, '\\s+', perl=TRUE))
            keys <- dat()[ids, nomatch=0][[global$key]]
            return(keys)
        } else {
            return(dat()[[global$key]])
        }
    })



    # =========================================================================
    # Get the name of the column selected in the main_table
    # Return NULL if no columns are selected
    # =========================================================================
    selected.column.name <- reactive({
        cat('entering selected.column.name()\n')
        cols <- names(dat())
        i <- input$main_table_columns_selected
        if(is.null(i)){
            return(NULL)
        } else {
            return(cols[i + 1])
        }
    })



    # =========================================================================
    # Return a logical vector recording the rows in the table present after
    # application of the filters
    # =========================================================================
    selection <- reactive({
        cat('entering selected.row.indices()\n')
        selection <- rep(FALSE, nrow(dat()))
        selection[input$main_table_rows_all] <- TRUE
        return(selection)
    })



    # =========================================================================
    # Render plots
    # Axes:
    #   x-axis: The selected column in main_table.
    #   y-axis: The column selected from the 'Compare to' menu.
    #   z-axis: The column selected from the 'Group by' menu. This variable
    #           must be categorical, since it is used to facet, boxplot, or
    #           barplot the data.
    # =========================================================================
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
            if(a$type == 'seq'){
                a$seq <- global$seq[[a$name]]
            }
            return(a)
        }

        get.column.selection <- function(a){
            cat('entering get.column.selection()\n')
            if(a$type == 'cor'){
                a$mat <- a$mat[selection(), ]
            }
            if(a$type == 'seq'){
                m <- a$seq[[1]] %in% dat()[selection()][[global$key]]
                a$seq <- a$seq[m, ]
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



    # =========================================================================
    # The DataTable shown in the Data tab
    # Columns selected from this table are plotted in the sidebar
    # =========================================================================
    output$main_table <- DT::renderDataTable(
        dat()[user.keys()],
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



    # =========================================================================
    # The DataTable shown in the Columns tab
    # * The rows selected determine which columns of the global dataset are
    #   dislayed in the Data tab (main_table).
    # * This table also displays the metadata for each field. There are no
    #   rules as to what columns may be included.
    # =========================================================================
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



    # =========================================================================
    # Download the data in main_table after filters are applied
    # =========================================================================
    output$downloadData <- downloadHandler(
        filename = 'shinyapp-datadump.tsv',
        content = function(file) {
            write.table(dat()[input$main_table_rows_all], file, row.names=FALSE, sep="\t")
        }
    )
})
