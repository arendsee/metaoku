require(shiny)
require(plyr)
require(ggplot2)
require(DT)
require(wordcloud) 
require(tm)

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

# toSmallFactor <- function(x, maxN=20){
#     # if is non-factored character array, reduce to most common maxN
#     # strings and replace all others with "other"
#     if(is.character(x) && ! is.factor(x)){
#         s = sort(summary(factor(x)))
#         if(length(s) > 20){
#             s[21] = sum(s[21:length(s)])
#             names(s) = c(names(s)[1:20], 'other')
#         }
#         x = factor(rep(names(s), times=as.numeric(s)))
#     } else if(is.numeric(x) && length(unique(x)) <= 20){
#         x = factor(x)
#     }
#     return(x)
# }

shinyServer(function(input, output){
    dat <- reactive({
        out <- data.table(model=as.vector(global$models), locus=as.vector(global$loci))
        for(column in input$columns){
            out <- mergeByName(out, column)
        }
        out <- data.frame(out)
        for(i in 1:ncol(out)){
            if(length(unique(out[, i])) <= 20){
                out[, i] <- factor(out[, i])
            }
        }
        return(out)
    })

    # Generate a summary of the dataset
    output$summary <- renderPrint({
        summary(dat()[input$main_table_rows_all, ])
    })

    output$plot <- renderPlot({
        columns      <- input$main_table_columns_selected + 1
        rows         <- input$main_table_rows_all

        if(is.na(columns[1])){ return() }

        full.data   <- dat()
        column.name <- colnames(full.data)[columns][1]

        x = full.data[rows, column.name]

        longest.line <- max(nchar(as.character(x)))
        is.full      <- (length(rows) == nrow(full.data))
        luniq        <- length(unique(x))

        if(luniq == 1){ return() }

        # === Handle long string data column
        if(column.name %in% names(global$corpa)){
            m = global$corpa[[column.name]]
            return(makeWordCloud(m, rows))
        } else if(!is.numeric(x) && luniq > 20){
            return()
        }

        # === Handle well-behaved factors and numeric columns

        if(is.full){
            d <- data.frame(values=x)
        } else {
            d <- data.frame(
                values=c(x, full.data[ , column.name]),
                group=c(rep('selected', length(x)), rep('all', nrow(full.data))))
        }
        
        # === Handle numeric data column
        if(is.numeric(d$values)){
            if(is.full){
                g <- ggplot(d) +
                    geom_histogram(aes(x=values))
            } else {
                g <- ggplot(d) +
                    geom_histogram(
                        aes(
                            x=values,
                            y=..density..,
                            fill=group
                        ),
                        alpha=.5,
                        position='identity'
                    ) + theme(axis.text.y=element_blank(),
                              axis.ticks.y=element_blank())
            }
        }

        if(is.factor(d$values)){
            if(is.full){
                g <- ggplot(d) +
                    geom_bar(aes(x=values))
                # make x-lables vertical is they are longer than 2 characters
            } else {
                d <- ddply(d, 'group', mutate, N.group=length(group))
                d <- ddply(d, c('values', 'group'), mutate, N.values=length(values))
                d$proportion = d$N.values / d$N.group
                
                g <- ggplot(d) +
                    geom_bar(
                        aes(
                            x=values,
                            y=proportion,
                            fill=group
                        ),
                        position='dodge',
                        stat='identity'
                    )
            }
            if(longest.line > 2) {
                g <- g + theme(axis.text.x = element_text(angle=270, hjust=0, vjust=1))
            }
        }

        g <- g +
            ggtitle(column.name) +
            theme(
               axis.text.x  = element_text(size=14), 
               axis.text.y  = element_text(size=14),
               axis.title.x = element_blank(), 
               axis.title.y = element_blank(),
               plot.title   = element_text(size=24, face='bold'),
               legend.title = element_blank(),
               legend.background = element_blank()
            )
        return(g)
    })

    output$main_table <- DT::renderDataTable(
        dat(),
        rownames=FALSE,
        filter='top',
        style='bootstrap',
        selection=list(
            mode='single',
            target='column',
            selected=6
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
