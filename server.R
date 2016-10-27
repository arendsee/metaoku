require(shiny)
require(DT)
require(markdown)
require(magrittr)
require(Matrix)

# ====================================================================
# Here is a really hacky way to avoid polluting the global environment
# ====================================================================
getGlobal_R   <- function(){source('global.R',     local=TRUE); config}
getLoad_R     <- function(){source('R/load.R',     local=TRUE); build.all.datasets}
getDispatch_R <- function(){source('R/dispatch.R', local=TRUE); dispatch}
getAxis_R     <- function(){source('R/axes.R',     local=TRUE); dataAxis}
config             <- getGlobal_R()
build.all.datasets <- getLoad_R()
dispatch           <- getDispatch_R()

# this approach doesn't work here
source('R/dataClass.R')
source('R/plotUI.R')
source('R/plot.R')

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
        cat('-> update.datasets\n')
        updateRadioButtons(session,
                           'selected.dataset',
                           choices=names(datasets),
                           selected=names(datasets)[1])
        # Empty the cache after updating
        cache <<- list()
    }
    update.datasets(datasets)

    dataset <- eventReactive(
        {input$selected.dataset},
        {
            cat('-> reactive:dataset - attempting to load dataset\n')
            if(input$selected.dataset == 'none'){
                cat(' - dataset=none\n')
                dataset <- DataSet$new()
                dataset$build()
                return(dataset)
            } else {
                cat(' - dataset =', input$selected.dataset, '\n')
                datafile <- datasets[input$selected.dataset]
            }
            if(is.null(cache[[datafile]]) && !is.na(datafile)) {
                cat(' - loading file and adding to cache\n')
                load(datafile)
                cache[[datafile]] <<- dataset
            } else {
                cat(' - retrieving file from cache\n')
                dataset <- cache[[datafile]]
            }
            return(dataset)
        },
        ignoreNULL=TRUE
    )



    # =========================================================================
    # Update working dataset when columns are selected from the Column Table
    # =========================================================================
    dat <- reactive({
        cat('-> dat()\n')
        out <- dataset()$getDF()
        csel <- input$column_table_rows_selected
        if(length(csel) > 0){
            out <- out[, csel]
        }
        if(!is.null(user.keys())){
            out <- out[user.keys(), ]
        }
        return(out)
    })

    setFilter <- reactive({
        cat('-> updating dataset filters\n')
        rsel <- input$main_table_rows_all
        rows.are.selected <- length(rsel) > 0
        if(is.null(user.keys())){
            if(rows.are.selected){
                cat(' - applying only DT row filter\n')
                filt <- rsel
            } else {
                cat(' - applying no filter\n')
                filt <- NULL
            }
        } else {
            if(rows.are.selected){
                cat(' - applying DT and user.key filter\n')
                cat(sprintf(' - rsel:(%s)\n', paste0(user.keys(), collapse=', ')))
                cat(sprintf(' - rsel:(%s)\n', paste0(rsel, collapse=', ')))
                filt <- user.keys()[rsel]
            } else {
                cat(' - applying only user.key filter\n')
                filt <- user.keys()
            }
        }

        if(is.null(filt)){
            cat(' - clearFilter\n')
            dataset()$clearFilter()
        } else {
            cat(' - setFilter\n')
            dataset()$setFilter(filt)
        }
        invisible()
    })

    user.keys <- eventReactive(
        {input$user_ids},
        {
            cat('-> user.keys()\n')
            # this prevents whitespace in the box from stopping plotting
            txt <- gsub('\n', ' ', input$user_ids)
            txt <- sub('^\\s+', '', txt, perl=TRUE)
            keys <- NULL

            # All three conditions are necessary, change them and weep blood
            parse_keys <- length(txt) > 0 &&
                          nchar(txt) > 0  &&
                          any(input$user_key %in% dataset()$names)
            if(parse_keys){
                cat(sprintf(' - user.key=(%s)\n', input$user_key))
                ids <- gsub('[,;\\\'"\\\t|<>]+', ' ', txt) 
                ids <- unlist(strsplit(ids, '\\s+', perl=TRUE))
                keys <- which(dataset()$get(input$user_key)$.value %in% ids)
                cat(sprintf(' - user.keys ids: (%s)\n', paste0(ids, collapse=', ')))
                cat(sprintf(' - user.keys #ids: %s\n', length(keys)))
                return(keys)
            } else {
                cat(' <- user.keys returning no keys\n')
                return(keys)
            }
        },
        ignoreNULL=TRUE
    )






    # =========================================================================
    # Record column names as they are selected from the Column Table
    #  * Update choices in the "Compare to" menu
    #  * Update choices in the "Group by" menu
    #    - limit options to datatypes that can be cast as categorical
    # =========================================================================
    observe({
        # set compare.to (y) choices
        cat(sprintf('-> observe - Building View tab menu\n'))
        columns <- dataset()$names
        updateSelectInput(session,
                          'compare.to',
                          choices=c('None', as.character(columns)))
        # set group.by (z) choices (this may not be cor or seq)
        neat_columns <- dataset()$getNameByType(c('cat', 'num', 'longcat'))
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
        cat('-> observe - Building Upload tab\n')
        umode <- input$upload.type
        if(!is.null(umode) && umode == 'dataset'){
            desc <- file.path('pages', 'upload-dataset-instructions.md')
        } else if((!is.null(umode) && umode == 'single')) {
            desc <- file.path('pages', 'upload-single-instructions.md')
        } else {
            return()
        }
        output$upload.instructions <- renderUI({shiny::includeMarkdown(desc)})
    })


    build <- function(){
        output$plot.sidebar <- renderUI({ isolate(plotui$buildUI(reactiveValuesToList(input))) })
    }
    observeEvent(
        input$plot.x,
        {
            cat('-> event:plot.x\n')
            x <- dataset()$get(input$plot.x)
            cat(sprintf(' - (name=%s, type=%s)\n', x$name, x$type))
            plotui$setX(x)
            build()
        },
        ignoreNULL=TRUE
    )
    observeEvent(
        input$plot.y,
        {
            cat('-> event:plot.y\n')
            y <- dataset()$get(input$plot.y)
            cat(sprintf(' - (name=%s, type=%s)\n', y$name, y$type))
            plotui$setY(y)
            build()
        },
        ignoreNULL=TRUE
    )
    observeEvent(
        input$plot.geom,
        {
            cat('-> event:plot.geom\n')
            geom <- input$plot.geom
            cat(sprintf(' - geom=(%s)\n', geom))
            plotui$setGeom(geom)
            build()
        },
        ignoreNULL=TRUE
    )
    observeEvent(
        input$selected.dataset,
        {
            cat('-> Initialize plotUI object\n')
            plotui$init(dataset(), empty=Empty())
            build()
        },
        ignoreNULL=TRUE
    )


    bigPlot <- reactive({
        cat('-> plot_data_plot\n')
        input$build.plot
        source('R/plotBuild.R', local=TRUE)
        isolate(buildPlot(dataset(), reactiveValuesToList(input)))
    })
    output$plot_data_plot <- renderPlot({
        bigPlot()
    })
    # Unfortunately, downloadHandler doesn't like reactives, so I have to wrap it
    # in a normal function
    getBigPlot <- function(){ bigPlot() }



    # =========================================================================
    # Update dataset description when new dataset is selected
    # =========================================================================
    observeEvent(
        input$selected.dataset,     
        {
            cat('-> observe:selected.dataset - update dataset description\n')
            # set the dataset description
            desc <- file.path(config$data_dir, input$selected.dataset, config$descriptions)
            if(!file.exists(desc)){
                desc <- file.path('pages', 'dataset-description.md')
            }
            output$dataset_description <- renderUI({shiny::includeMarkdown(desc)})
        },
        ignoreNULL=TRUE
    )



    # =========================================================================
    # Make relevant changes when a dataset is selected
    # =========================================================================
    observe({
        cat('-> observe:user_key - update key examples\n')
        key <- input$user_key
        # set the label for the user selected id textInput box
        if(key %in% colnames(dat())){
            unique_keys <- unique(dat()[[key]])
            nuniq <- min(3, length(unique_keys))
            sample_ids <- sample(unique_keys, size=nuniq)
            label <- sprintf('Enter ids (e.g. "%s")', paste(sample_ids, collapse=", "))
            updateTextInput(session, 'user_ids', label=label)
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
        cat('-> selected.column.name()\n')
        cols <- names(dat())
        i <- input$main_table_columns_selected + 1
        if(length(i) > 0){
            cat(sprintf('  <- returning %s\n', cols[i]))
            return(cols[i])
        } else {
            cat('  <- returning NULL\n')
            return(NULL)
        }
    })



    # =========================================================================
    # Return a logical vector recording the rows in the table present after
    # application of the filters
    # =========================================================================
    selection <- reactive({
        cat('-> selection()\n')
        setFilter()
        selection <- Empty()
        filt <- dataset()$row_filter
        if(is.null(filt)){
            cat(' no filter in dataset\n')
            return(NULL)
        }
        else {
            cat(' making DataCat for selection object\n')
            value <- 1:dataset()$nrow %in% filt %>%
                         ifelse('Selected', 'Not Selected')
            sel <- DataCat$new()
            sel$init(value=value)
            cat(str(sel))
            return(sel)
        }
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
    view_plot <- reactive({
        cat('-> renderPlot()\n')

        setFilter()

        cname <- selected.column.name()
        if(is.null(cname)){
            return(NULL)
        }

        # x-axis comes from the selected column
        # y-axis comes from 'Compare to' dropdown, may be NULL
        # z-axis from 'Group by' dropdown
        axes <- lapply(list(x=cname, y=input$compare.to, z=input$group.by),
            function(x) {
                if(x == 'Selection') {
                    temp.filt <- dataset()$row_filter
                    sel <- selection()
                    dataset()$clearFilter()
                    sel
                } else {
                    dataset()$get(x)
                }
             }
        )
        cat(sprintf(' - x set to "%s"\n', axes$x$name))
        cat(sprintf(' - y set to "%s"\n', axes$y$name))
        cat(sprintf(' - z set to "%s"\n', axes$z$name))

        fmt.opts <- list(
           logy=input$logy,
           logx=input$logx
        )

        g <- dispatch(axes$x, axes$y, axes$z, fmt.opts)

        if(exists('temp.filt')) dataset()$setFilter(temp.filt)

        # Types may be temporarily recast in the dispatch function (e.g.
        # numeric columns may be recast as categorical ranges via cut(). To
        # revert these to the original types, refresh.
        dataset()$refresh()

        return(g)
    })
    output$view_data_plot <- renderPlot({ view_plot()})
    getViewPlot <- function(){
        w=session$clientData$output_view_data_plot_width
        h=session$clientData$output_view_data_plot_height
        cat(sprintf(' -> getViewPlot w=(%s), h=(%s)\n', w, h))
        view_plot()
    }



    # =========================================================================
    # The DataTable shown in the Data tab
    # Columns selected from this table are plotted in the sidebar
    # =========================================================================
    output$main_table <- DT::renderDataTable(
        dat(),
        rownames=FALSE,
        filter='top',
        style='bootstrap',
        selection=list(
            mode='single',
            target='column',
            selected=0
        ),
        extensions = c('Buttons', 'ColReorder'),
        options = list(
            dom = 'RCT<"clear">lfrtip',
            buttons = I('colvis'),
            autoWidth=TRUE,
            orderMulti=TRUE,
            searching=TRUE,
            search.regex=TRUE
        )
    )



    # =========================================================================
    # The DataTable shown in the Columns tab
    # * The rows selected determine which columns of the global dataset are
    #   dislayed in the Data tab (main_table).
    # * This table also displays the metadata for each field. There are no
    #   rules as to what columns may be included.
    # =========================================================================
    output$column_table <- DT::renderDataTable(
        {
            cat('-> column_table()\n')
            dataset()$getMetadataDF()
        },
        filter="none",
        rownames=FALSE,
        options=list(
            paging=FALSE,
            autoWidth=FALSE,
            scrollX=TRUE,
            scrollCollapse=FALSE,
            scrollY=FALSE,
            searching=FALSE,
            sorting=FALSE
        )
    )



    # =========================================================================
    # Upload a file or dataset
    # =========================================================================
    upload.type <- 'single'
    observe({
        cat('-> observe:upload.type - radio button\n')
        upload.type <<- input$upload.type
    })
    observe({
        files=input$upload.file
        if(!is.null(files)){isolate({
            cat('-> observe:upload.file - most things\n')
            files <- input$upload.file
            cat('files:\n')
            cat(str(files))
            kill_dir <- FALSE
            success <- FALSE
            if(upload.type == 'single'){
                for(i in nrow(files)){
                    datapath <- files[i, 'datapath']
                    data.basename <- basename(files[i, 'name'])
                    data.name <- gsub('\\..*', '', data.basename)
                    newdir <- file.path(config$data_dir, data.name)
                    newpath <- file.path(newdir, data.basename)
                    if(!dir.exists(newdir)){
                        kill_dir <- TRUE
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
        })}
    })



    # =========================================================================
    # Download the data in main_table after filters are applied
    # =========================================================================
    output$downloadData <- downloadHandler(
        filename = 'metaoku-data.tab',
        content = function(file) {
            out <- dataset()$getDF(filterRows=TRUE)[, names(dat())]
            write.table(out, file, row.names=FALSE, sep="\t")
        },
        contentType='text/csv'
    )

    # download the small automagic plot from the View tab
    output$downloadMagicPlot <- downloadHandler(
        filename = 'metaoku-plot.png',
        content = function(file){
            g <- getViewPlot()
            cat(str(g))
            png(file, width=1920, height=1920, res=288)
            print(g)
            dev.off()
        }
    )

    # download the large plot from the Plot tab
    output$downloadPlot <- downloadHandler(
        filename = 'metaoku-plot.pdf',
        content = function(file){
            ggsave(filename=file, plot=getBigPlot(), w=12, h=9)
        }
    )

    output$downloadProject <- downloadHandler(
        filename = 'metaoku-project.zip',
        content = function(file){
            zip(zipfile=file, files=basename(config$data_dir))
        }
    )

    # =========================================================================
    # Read the version from VERSION
    # =========================================================================
    output$version <- renderText({
        paste0('v', readLines('VERSION'))
    })

    cancel.onSessionEnded <- session$onSessionEnded(
        function() {
            if(config$access == 'sandbox'){
                setwd(config$data_dir)
                for(d in dir('.')){
                    unlink(d, recursive=TRUE)
                }
                setwd(config$save_dir)
                for(f in list.files('.')){
                    unlink(f)
                }
                setwd('..')
            }
        }
    )
})
