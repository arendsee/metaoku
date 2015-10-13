require(shiny)
require(DT)
require(markdown)
require(magrittr)
require(Matrix)

# ====================================================================
# TODO: build an organized OO system for my datatypes
# Here is a really hacky way to avoid polluting the global environment
# ====================================================================
getGlobal_R   <- function(){source('global.R',     local=TRUE); config}
getLoad_R     <- function(){source('R/load.R',     local=TRUE); build.all.datasets}
getDispatch_R <- function(){source('R/dispatch.R', local=TRUE); dispatch}
getAxis_R     <- function(){source('R/axes.R',     local=TRUE); dataAxis}
config             <- getGlobal_R()
build.all.datasets <- getLoad_R()
dispatch           <- getDispatch_R()
dataAxis           <- getAxis_R()

# this approach doesn't work here
source('R/dataClass.R')
source('R/plotUI.R')
source('R/plot.R')

`%ifunset%` <- function(a, b) if(is.null(a) || is.na(a) || length(a) == 0 || a == 'None') b else a
`%|%` <- function(x,y) if(is.null(x)) y else x

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
datasets <- build.all.datasets(config)
if(length(datasets) > 0){
    load(datasets[1])
}



shinyServer(function(input, output, session){

    plotui <- PlotUI$new()
    cache  <- list()
    
    # =========================================================================
    # Retrieve selected dataset
    #   - Cache every dataset the first time it is loaded. This could cause
    #     memory issues if there are too many large datasets. But for now,
    #     it makes everything way snappier.
    # =========================================================================
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
    observeEvent(
        input$selected.dataset, 
        {
            cat('entering observe:selected.dataset ... loading', input$selected.dataset, '\n')
            if(input$selected.dataset == 'None'){
                datafile <- datasets[1]
            } else {
                datafile <- datasets[input$selected.dataset]
            }
            if(is.null(cache[[datafile]]) && !is.na(datafile)) {
                load(datafile)
                cache[[datafile]] <- dataset
            } else {
                dataset <- cache[[datafile]]
            }
        }, priority=10
    )



    # =========================================================================
    # Read the version from VERSION
    # =========================================================================
    output$version <- renderText({
        paste0('v', readLines('VERSION'))
    })



    # =========================================================================
    # Update working dataset when columns are selected from the Column Table
    # =========================================================================
    dat <- reactive({
        cat('dat()\n')
        input$selected.dataset # run if chanage datasets
        out <- dataset$getDF()
        csel <- input$column_table_rows_selected
        if(length(csel) > 0){
            out <- out[, csel]
        }
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
        cat(sprintf('LENGTH:dat()=(%s), \n', length(dat())))
        columns <- names(dat())
        updateSelectInput(session,
                          'compare.to',
                          choices=c('None', as.character(columns)))
        # set group.by (z) choices (this may not be cor or seq)
        neat_columns <- dataset$getNameByType(c('cat', 'num', 'longcat'))
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
            desc <- file.path('doc', 'upload-dataset-instructions.md')
        } else {
            desc <- file.path('doc', 'upload-single-instructions.md')
        }
        output$upload.instructions <- renderUI({shiny::includeMarkdown(desc)})
    })


    build <- function(){
        output$plot.sidebar <- renderUI({ plotui$buildUI() })
    }
    observeEvent(
        input$plot.x,
        {
            x <- dataset$get(input$plot.x)
            cat(sprintf('EVENT plot.x: (%s,%s)\n', x$name, x$type))
            plotui$setX(x)
            build()
        }, priority=-1
    )
    observeEvent(
        input$plot.y,
        {
            y <- dataset$get(input$plot.y)
            cat(sprintf('EVENT y: (%s,%s)\n', y$name, y$type))
            plotui$setY(y)
            build()
        }, priority=-1
    )
    observeEvent(
        input$plot.geom,
        {
            geom <- input$plot.geom
            cat(sprintf('EVENT geom: (%s)\n', geom))
            plotui$setGeom(geom)
            build()
        }, priority=-1
    )
    observeEvent(
        input$selected.dataset,
        {
            cat('entering plotui: INITIALIZE PLOTUI\n')
            plotui$init(dataset, empty=Empty())
            build()
        }
    )

    output$plot_data_plot <- renderPlot({
        source('R/plotBuild.R', local=TRUE)
        input$build.plot
        if(input$plot.x != 'None') {
            isolate(buildPlot(dat(), input))
        } else {
            NULL
        }
    })



    # =========================================================================
    # Update dataset description when new dataset is selected
    # =========================================================================
    observe({
        dataset <- input$selected.dataset

        # set the dataset description
        desc <- file.path(config$data_dir, dataset, 'README.md')
        if(!file.exists(desc)){
            desc <- file.path('doc', 'dataset-description.md')
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
        if(key %in% colnames(dat())){
            sample_ids <- sample(unique(dat()[[key]]), size=3)
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
        cat('entering selection()\n')
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
        x <- dataset$get(cname) %ifunset% Empty()

        # y-axis currently comes from 'Compare to' dropdown, may be NULL
        y <- dataset$get(input$compare.to) %ifunset% Empty()

        # z-axis from 'Group by' dropdown
        z <- dataset$get(input$group.by) %ifunset% Empty()

        fmt.opts <- list(
           logy=input$logy,
           logx=input$logx
        )

        dispatch(x, y, z, fmt.opts)
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
            target='column',
            selected=0
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
            dataset$getMetadataDF()
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
        source('../config')
        if(upload.type == 'single'){
            for(i in nrow(files)){
                datapath <- files[i, 'datapath']
                data.basename <- basename(files[i, 'name'])
                data.name <- gsub('\\..*', '', data.basename)
                newdir <- file.path(DATA_DIR, data.name)
                newpath <- file.path(newdir, data.basename)
                newsave <- file.path(SAVE_DIR, paste0(data.name, '.Rdat'))
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
            datasets <<- build.all.datasets(config)
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
