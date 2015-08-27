require(shiny)
require(plyr)
require(ggplot2)
require(DT)
require(wordcloud) 
require(tm)
require(reshape2)

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
            setkeyv(dat, key)
            dat <- merge(dat, d[, c(key, by.colname), with=FALSE], by=key)
        }
    }
    return(dat)
}

makeWordCloud <- function(mat, selection){
    obs.sel <- sort(colSums(mat[selection, ]), decreasing=TRUE)

    # # eventually I should do more statistics with this
    # v.ori <- sort(colSums(mat), decreasing=TRUE)
    # exp.sel <- sum(v.sel) * (v.ori / sum(v.ori))
    # sel <- data.frame(exp=exp.sel, obs=obs.sel, rat=log(obs.sel / exp.sel))

    d <- data.frame(word = names(obs.sel), freq=obs.sel)
    pal2 <- brewer.pal(8, "Dark2")
    g = wordcloud(d$word, d$freq,
                  min.freq=3, max.words=100,
                  random.order=FALSE,
                  rot.per=.15,
                  colors=pal2)
    return(g)
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

    sel <- reactive({
        columns  <- input$main_table_columns_selected + 1
        rows     <- input$main_table_rows_all
        d <- data.table(dat())
        if(length(columns) > 0 && ncol(d) >= max(columns)){
            d <- d[, columns, with=FALSE]
            d$selected       <- FALSE
            d$selected[rows] <- TRUE
            d <- data.frame(d)
            d <- melt(d, id.vars=c('selected'), value.name='value')
            d$group = ifelse(d$selected, 'selected', 'non-selected')
            return(d)
        }
        return(NULL)
    })


    # Generate a summary of the dataset
    output$summary <- renderPrint({
        summary(dat()[input$main_table_rows_all, ])
    })

    output$plot <- renderPlot({
        # exit if more than one column is selected
        s <- sel()

        if(is.null(s) || nrow(s) < 1 || nlevels(s$variable) != 1){
            return()
        }

        column.name <- levels(s$variable)[1]
        luniq       <- length(unique(s$value))

        # ensure this column has the same levels as the original (this gets scrambled easily)
        if(is.factor(dat()[, column.name])){
            s$value = factor(s$value, levels=levels(dat()[, column.name]))
        }

        if(column.name %in% names(global$corpa)){
            m = global$corpa[[column.name]]
            return(makeWordCloud(m, which(s$selected)))
        } else if(is.numeric(s$value)){
            if(all(s$selected)){
                g <- ggplot(s) +
                    geom_histogram(aes(x=value))
            } else {
                g <- ggplot(s) +
                    geom_histogram(
                        aes(
                            x=value,
                            y=..density..,
                            fill=group
                        ),
                        alpha=.75,
                        position='identity'
                    ) + theme(axis.text.y=element_blank(),
                              axis.ticks.y=element_blank())
            }
            if(input$logx){
                g <- g + scale_x_continuous(trans='log2')
            }
        } else if(is.factor(s$value)){
            if(all(s$selected)){
                g <- ggplot(s) +
                    geom_bar(aes(x=value))
                # make x-lables vertical is they are longer than 2 characters
            } else {
                s <- ddply(s, 'selected', mutate, N.selected=length(selected))
                s <- ddply(s, c('value', 'selected'), mutate, N.value=length(value))
                s$proportion = s$N.value / s$N.selected
                
                g <- ggplot(s) +
                    geom_bar(
                        aes(
                            x=value,
                            y=proportion,
                            fill=group
                        ),
                        position='dodge',
                        stat='identity'
                    )
            }
            longest.line <- max(nchar(as.character(s$value)))
            if(longest.line > 2) {
                g <- g + theme(axis.text.x = element_text(angle=270, hjust=0, vjust=1))
            }
        } else {
            return()
        }

        g <- g +
            ggtitle(column.name) +
            theme(
               axis.text.x       = element_text(size=14), 
               axis.text.y       = element_text(size=14),
               plot.title        = element_text(size=24, face='bold'),
               axis.title.x      = element_blank(), 
               axis.title.y      = element_blank(),
               legend.title      = element_blank(),
               legend.background = element_blank()
            )
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
        } else if(luniq < 21){
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
        )
    )

    output$downloadData <- downloadHandler(
        filename = 'arabidopsis-data.tsv',
        content = function(file) {
            write.table(dat()[input$main_table_rows_all, ], file, row.names=FALSE, sep="\t")
        }
    )
})
