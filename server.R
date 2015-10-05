require(shiny)
require(DT)
require(markdown)

source('dispatch.R')
source('axes.R')

`%|%` <- function(x,y) { if(is.null(x)) y else x }

# =========================================================================
# Initialize a dataset as a list of data and metadata
# includes:
#  * table    - the full data.table
#  * key      - the data.table key
#  * metadata - the associated metadata
#  * type     - contains the type of each column in table
#  * corpa    - matrices for building wordclouds from text
#  * seqs     - character counts for each sequence column
# =========================================================================
source('load.R')
datasets <- build.all.datasets()



shinyServer(function(input, output, session){

    
    # =========================================================================
    # Retrieve selected dataset
    #   - Cache every dataset the first time it is loaded. This could cause
    #     memory issues if there are too many large datasets. But for now,
    #     it makes everything way snappier.
    # =========================================================================
    cache <- list()
    update.datasets <- function(datasets){
        cat('entering update.datasets\n')
        updateRadioButtons(session,
                           'selected.dataset',
                           choices=names(datasets),
                           selected=names(datasets)[1])
        # Empty the cache after updating
        cache <<- list()
    }
    update.datasets(datasets)
    global <- reactive({
        cat('entering global() ... loading', input$selected.dataset, '\n')
        if(input$selected.dataset == 'none'){
            datafile <- datasets[1]
        } else {
            datafile <- datasets[input$selected.dataset]
        }
        if(is.null(cache[[datafile]])){
            load(datafile)
            cache[[datafile]] <<- global
        } else {
            global <- cache[[datafile]]
        }
        return(global)
    })



    # =========================================================================
    # Read the version from VERSION
    # =========================================================================
    output$version <- renderText({
        paste0('v', readLines('VERSION'))
    })



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
        columns <- global()$metadata$column_name[input$column_table_rows_selected]
        # the key column should always be the first column in the data table
        columns <- unique(c('KEY', columns))
        # assert all fields in the metadata are in the actual data
        stopifnot(columns %in% names(global()$table))
        out <- global()$table[, columns, with=FALSE]
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
        columns <- global()$metadata$column_name[input$column_table_rows_selected]
        updateSelectInput(session,
                          'compare.to',
                          choices=c('None', as.character(columns)))
        # set x.axis on 'Plot Data' tab
        updateSelectInput(session,
                          'x.axis',
                          choices=c('None', as.character(columns)))
        # set y.axis on 'Plot Data' tab
        updateSelectInput(session,
                          'y.axis',
                          choices=c('None', as.character(columns)))
        # set z.axis on 'Plot Data' tab
        updateSelectInput(session,
                          'z.axis',
                          choices=c('None', as.character(columns)))
        # set group.by (z) choices (this may not be cor or seq)
        neat_columns <- columns[global()$type[columns] %in% c('cat', 'num', 'longcat')]
        updateSelectInput(session,
                          'group.by',
                          choices=c('None', 'Selection', as.character(neat_columns)))
        # update keys in 'Select ids'
        updateSelectInput(session,
                          'user_key',
                          choices=as.character(neat_columns))
    })



    # =========================================================================
    # Load instructions given the type of upload the user chooses
    # =========================================================================
    observe({
        umode <- input$upload.type
        if(umode == 'dataset'){
            desc <- 'defaults/upload-dataset-instructions.md'
        } else {
            desc <- 'defaults/upload-single-instructions.md'
        }
        output$upload.instructions <- renderUI({shiny::includeMarkdown(desc)})
    })



    # =========================================================================
    # Update x- and y-axis ranges when axes change on plot
    # =========================================================================
#   observe({
#       xname <- input$x.axis
#       xtype <- global()$types[xname]
#       if(xtype == 'num'){
#           xmin  <- min(dat()[[xname]])
#           xmax  <- max(dat()[[xname]])
#           cat('xmin:', xmin, 'xmax', xmax, '\n')
#       }
#   })
#   observe({
#       yname <- input$y.axis
#       ymin  <- min(dat()[[yname]])
#       ymax  <- max(dat()[[yname]])
#       updateSliderInput(session, 'plot.yrange', min=ymin, max=ymax, value=c(ymin, ymax))
#   })

    output$xformat <- renderUI({
        axis.name <- input$x.axis
        if(axis.name == 'None'){
            el = h1('No axis selected')           
        } else if(global()$type[axis.name] == 'num'){
            axis.min <- min(dat()[[axis.name]], na.rm=TRUE)
            axis.max <- max(dat()[[axis.name]], na.rm=TRUE)
            cat('min/max', axis.min, axis.max, '\n')
            el <- fluidRow(
               column(2, checkboxInput('plot.logx', 'log2 x-axis')),
               column(8, sliderInput('plot.xrange', 'x-axis range', min=axis.min, max=axis.max, value=c(axis.min, axis.max)))
            )
        } else {
            el <- h1('No options')
        }
        return(el)
    })
    output$yformat <- renderUI({
        axis.name <- input$y.axis
        if(axis.name == 'None'){
            el = h1('No axis selected')           
        } else if(global()$type[axis.name] == 'num'){
            axis.min <- min(dat()[[axis.name]], na.rm=TRUE)
            axis.max <- max(dat()[[axis.name]], na.rm=TRUE)
            cat('min/max', axis.min, axis.max, '\n')
            el <- fluidRow(
               column(2, checkboxInput('plot.logx', 'log2 y-axis')),
               column(8, sliderInput('plot.yrange', 'y-axis range', min=axis.min, max=axis.max, value=c(axis.min, axis.max)))
            )
        } else {
            el <- h1('No options')
        }
        return(el)
    })
    output$zformat <- renderUI({
        axis.name <- input$z.axis
        if(axis.name == 'None'){
            el = h1('No axis selected')           
        } else if(global()$type[axis.name] == 'num'){
            axis.min <- min(dat()[[axis.name]], na.rm=TRUE)
            axis.max <- max(dat()[[axis.name]], na.rm=TRUE)
            cat('min/max', axis.min, axis.max, '\n')
            el <- fluidRow(
               column(2, checkboxInput('plot.logz', 'log2 z-axis')),
               column(8, sliderInput('plot.zrange', 'z-axis range', min=axis.min, max=axis.max, value=c(axis.min, axis.max)))
            )
        } else {
            el <- h1('No options')
        }
        return(el)
    })



    # =========================================================================
    # Update dataset description when new dataset is selected
    # =========================================================================
    observe({
        dataset <- input$selected.dataset

        # set the dataset description
        desc <- paste0('data/', dataset, '/README.md')
        if(!file.exists(desc)){
            desc <- 'defaults/dataset-description.md'
        }
        output$dataset_description <- renderUI({shiny::includeMarkdown(desc)})
    })



    # =========================================================================
    # Make relevant changes when a dataset is selected
    # =========================================================================
    observe({
        dataset <- input$selected.dataset
        key <- input$user_key
        # set the label for the user selected id textInput box
        if(key %in% colnames(global()$table)){
            sample_ids <- sample(unique(global()$table[[key]]), size=3)
            label <- sprintf('Enter ids (e.g. "%s")', paste(sample_ids, collapse=", "))
            updateTextInput(session, 'user_ids', label=label)
        }
    })
    


    # =========================================================================
    # Read input from textInput box, extract ids, and return if they are
    # present in the key column of the main dataset
    # =========================================================================
    user.keys <- reactive({
        cat('entering user.keys()\n')
        # this prevents whitespace in the box from stopping plotting
        txt <- gsub('\n', ' ', input$user_ids)
        txt <- sub('^\\s+', '', txt, perl=TRUE)
        if(nchar(txt) > 0){
            ids <- gsub('[,;\\\'"\\\t|<>]+', ' ', txt) 
            ids <- unlist(strsplit(ids, '\\s+', perl=TRUE))
            keys <- dat()[dat()[[input$user_key]] %in% ids, KEY]
            return(keys)
        } else {
            return(dat()$KEY)
        }
    })



    # =========================================================================
    # Get the name of the column selected in the main_table
    # Return NULL if no columns are selected
    # NOTE: dat() contains the internal KEY column, but main_table does not. So
    #       when we must add 1 when mapping column numbers from main_table to
    #       the dat()
    # =========================================================================
    selected.column.name <- reactive({
        cat('entering selected.column.name()\n')
        cols <- names(dat())
        i <- input$main_table_columns_selected + 1
        if(length(i) > 0){
            return(cols[i + 1])
        } else {
            return(NULL)
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
    # Render plot on 'View Data' tab
    # Axes:
    #   x-axis: The selected column in main_table.
    #   y-axis: The column selected from the 'Compare to' menu.
    #   z-axis: The column selected from the 'Group by' menu. This variable
    #           must be categorical, since it is used to facet, boxplot, or
    #           barplot the data.
    # =========================================================================
    output$view_data_plot <- renderPlot({
        cat('entering renderPlot()\n')

        cname <- selected.column.name()
        if(is.null(cname)){
            return(NULL)
        }

        # x-axis comes from the selected column
        x <- cname

        # y-axis currently comes from 'Compare to' dropdown, may be NULL
        y <- input$compare.to

        # z-axis from 'Group by' dropdown
        z <- input$group.by

        axes <- list(x=x, y=y, z=z)
        axes <- dataAxis(axes, global=global(), selection=selection())

        fmt.opts <- list(
            logy=input$logy,
            logx=input$logx
        )

        plotAnything(x=axes[['x']],
                     y=axes[['y']],
                     z=axes[['z']],
                     fmt.opts=fmt.opts)
    })



    # =========================================================================
    # Render plot on 'Plot Data' tab
    # Axes:
    #   x-axis: From 'x-axis' menu
    #   y-axis: From 'y-axis' menu
    #   z-axis: From 'z-axis' menu
    # =========================================================================
    output$plot_data_plot <- renderPlot({
        cat('entering renderPlot()\n')

        axes <- list(x=input$x.axis, y=input$y.axis, z=input$z.axis)
        axes <- dataAxis(axes, global=global(), selection=selection())

        cat('\txrange:', input$plot.xrange, '\n')

        fmt.opts <- list(
            logy=input$plot.logy %|% FALSE,
            logx=input$plot.logx %|% FALSE,
            xrange=input$plot.xrange,
            yrange=input$plot.yrange
        )

        plotAnything(x=axes[['x']],
                     y=axes[['y']],
                     z=axes[['z']],
                     fmt.opts=fmt.opts)
    })



    # =========================================================================
    # The DataTable shown in the Data tab
    # Columns selected from this table are plotted in the sidebar
    # =========================================================================
    output$main_table <- DT::renderDataTable(
        {
            cat('entering main_table\n')
            d <- dat()[user.keys()]
            d$KEY <- NULL
            return(d)
        },
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
            global()$metadata
        },
        filter="none",
        rownames=FALSE,
        selection=list(
            mode='multiple',
            target='row',
            selected=1:(nrow(global()$metadata))
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
    # Upload a file or dataset
    # =========================================================================
    upload.type <- 'single'
    observe({
        upload.type <<- input$upload.type
    })
    observe({
        cat('entering file upload\n')
        files <- input$upload.file
        success <- FALSE
        if(upload.type == 'single'){
            for(i in nrow(files)){
                datapath <- files[i, 'datapath']
                data.basename <- basename(files[i, 'name'])
                data.name <- gsub('\\..*', '', data.basename)
                newdir <- file.path(getwd(), 'data', data.name)
                newpath <- file.path(newdir, data.basename)
                newsave <- file.path(getwd(), 'saved', paste0(data.name, '.Rdat'))
                if(!dir.exists(newdir)){
                    dir.create(newdir)
                }
                if(file.exists(newpath)){
                    cat(sprintf('WARNING: I refuse to overwrite file "%s"\n', newpath))
                } else {
                    if(file.size(datapath) > 0){
                        success <- TRUE
                        file.copy(datapath, newpath)
                    }
                }
            }
        } else if(upload.type == 'dataset'){
            cat('\tdataset upload not yeat implemented')
        }
        if(success){
            cat('Updating database\n')
            datasets <<- build.all.datasets()
            cat('Datasets: ', datasets, '\n')
            update.datasets(datasets)
        }
    })



    # =========================================================================
    # Download the data in main_table after filters are applied
    # =========================================================================
    output$downloadData <- downloadHandler(
        filename = 'metaoku-data.tab',
        content = function(file) {
            write.table(dat()[input$main_table_rows_all], file, row.names=FALSE, sep="\t")
        },
        contentType='text/csv'
    )

})
