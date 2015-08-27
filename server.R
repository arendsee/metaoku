require(shiny)
require(plyr)
require(ggplot2)
require(DT)
require(wordcloud) 
require(tm)
require(reshape2)

source('global.R')
source('plotting-functions.R')

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
            setkeyv(dat, key)
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
        cols  <- input$main_table_columns_selected + 1
        rows  <- input$main_table_rows_all
        return(sel.nonreactive(cols=cols, rows=rows))
    })


    # Generate a summary of the dataset
    output$summary <- renderPrint({
        summary(dat()[input$main_table_rows_all, ])
    })

    refactor <- function(x, column.name){
        if(is.factor(dat()[, column.name])){
            x = factor(x, levels=levels(dat()[, column.name]))
        }
        return(x)
    }

    output$plot <- renderPlot({
        # exit if more than one column is selected
        s <- sel()

        if(is.null(s) || nrow(s) < 1 || nlevels(s$variable) != 1){
            return()
        }

        column.name <- levels(s$variable)[1]

        # ensure this column has the same levels as the original (this gets scrambled easily)
        s$value <- refactor(s$value, column.name)

        is_txt <- column.name %in% names(global$corpa)
        is_com <- input$compare.to != "None"
        is_num <- is.numeric(s$value)
        is_fac <- is.factor(s$value)
        is_all <- all(s$selected)

        if(is_txt){
            return(plotText(s, column.name))
        }

        if(is_com){
            other <- sel.nonreactive(column.names=input$compare.to)
            other$value <- refactor(other$value, input$compare.to)
            other.is_num <- is.numeric(other$value)
            other.is_fac <- is.factor(other$value)
            selection <- if(is_all) NULL else ifelse(s$selected, 'selected', 'not-selected')
            if(is_num && other.is_num){
                g <- plotPairedNumericNumeric(x=s$value, y=other$value, group=selection)
            } else if (is_num && other.is_fac){
                g <- plotPairedFactorNumeric(other$value, s$value, group=selection)
            } else if (is_fac && other.is_num){
                g <- plotPairedFactorNumeric(s$value, other$value, group=selection)
            } else if (is_fac && other.is_fac){
                g <- plotPairedFactorFactor(s$value, other$value)
            } else {
               return() 
            }
        } else {
            if(is_num && is_all){
                g <- plotNumeric(s, logx=input$logx)
            } else if(is_num && !is_all){
                g <- plotSampledNumeric(s, logx=input$logx)
            } else if(is_fac && is_all){
                g <- plotFactor(s)
            } else if(is_fac && !is_all){
                g <- plotSampledFactor(s)
            } else {
                return()
            }
        }

        g <- addTheme(g, title=column.name)

        return(g)
    })

    output$selection_summary_1 <- renderTable({
        if(is.null(sel()) || nlevels(sel()$variable) != 1){ return() }

        luniq <- length(unique(sel()$value))

        if(is.numeric(sel()$value)){
            if(all(sel()$selected)){
                s <- with(sel(), data.frame( 
                        N=length(value),
                        mean=mean(value),
                        sd=sd(value)
                    ))
            } else {
                s <- ddply(sel(), 'group', summarize,
                           N=length(value),
                           mean=mean(value),
                           sd=sd(value))
            }
        } else if(luniq <= 20){
            if(all(sel()$selected)){
                return()
            } else {
                s <- ddply(sel(), 'group', summarize,
                           N=length(value),
                           sd=mean(summary(value)))
            }
        } else {
            return()
        }
        return(s)
    }, include.rownames=FALSE)

    output$selection_summary_2 <- renderTable({
        if(is.null(sel()) || nlevels(sel()$variable) < 1){ return() }

        if(is.numeric(sel()$value)){
            if(!all(sel()$selected)){
                x = subset(sel(), selected)$value
                y = subset(sel(), !selected)$value
                s = data.frame(
                    wilcoxon_test=wilcox.test(x, y)$p.value    
                )
            } else {
                return()
            }
        } else if(is.factor(sel()$value)){
           return() 
        } else {
            return()
        }
        return(s)

    }, include.rownames=FALSE)

    get_user_data <- function(){
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
