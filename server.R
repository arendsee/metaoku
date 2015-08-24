require(shiny)
require(ggplot2)
require(DT)
require(wordcloud) 
require(tm)
require(SnowballC)

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

makeWordCloud <- function(m){
    v <- sort(colSums(m), decreasing=TRUE)
    d <- data.frame(word = names(v), freq=v)
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
        out = data.frame(out)
        # factorize non-numeric strings if less than 20 unique elements
        for(i in 1:ncol(out)){
            x = out[, i]
            if(length(unique(x)) <= 20){
                out[, i] = as.factor(x)
            }
        }
        return(out)
    })

    # Generate a summary of the dataset
    output$summary <- renderPrint({
        summary(dat()[input$main_table_rows_all, ])
    })

    output$plot <- renderPlot({
        columns = input$main_table_columns_selected + 1
        column.names = colnames(dat())[columns]
        rows = input$main_table_rows_all
        if(! is.null(columns) && length(columns) > 0){
            x = dat()[rows, columns]
            column.name = column.names[1]
            if(is.numeric(x)){
                g <- ggplot(data.frame(values=x)) +
                    geom_histogram(aes(x=values))
                return(g)
            }

            if(column.name %in% names(global$corpa)){
                m = global$corpa[[column.name]]
                return(makeWordCloud(m[rows, ]))
            }

            # if is non-factored character array, reduce to most common 20
            # strings and replace all others with "other"
            if(is.character(x) && ! is.factor(x)){
                s = sort(summary(factor(x)))
                if(length(s) > 20){
                    s[21] = sum(s[21:length(s)])
                    names(s) = c(names(s)[1:20], 'other')
                }
                x = factor(rep(names(s), times=as.numeric(s)))
            }

            if(is.factor(x)){
                g <- ggplot(data.frame(values=x)) +
                    geom_bar(aes(x=values))
                # make x-lables vertical is they are longer than 2 characters
                if(longest.line > 2) {
                    g <- g + theme(axis.text.x = element_text(angle=270, hjust=0, vjust=1))
                }
                return(g)
            }
        }
    })

    output$main_table <- DT::renderDataTable(
        dat(),
        rownames=FALSE,
        filter='top',
        style='bootstrap',
        selection=list(
            mode='single',
            target='column',
            selected=5
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
