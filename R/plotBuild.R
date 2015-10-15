buildPlot <- function(dataset, input){
    cat('\tplotBuild.R::buildPlot\n')

    d <- build.dataframe(dataset, input)
    if(length(d) == 0 || nrow(d) == 0){
        return(NULL)
    }
    g.aes <- build.aes(input)

    cat('\t GEOM: ', input$plot.geom, '\n')
    cat(str(g.aes))

    switch(input$plot.geom,
        'barplot' = {
            cat('\t - barplot\n')
            cat(str(g.aes))
            ggplot(d) + geom_bar(mapping=g.aes)
        },
        'boxplot' = {
            cat('\t - boxplot\n')
            # clear unused aesthetics
            g.aes$color     <- NULL
            g.aes$shape     <- NULL
            g.aes$group     <- NULL
            g.aes$linetype  <- NULL
            g.aes$size      <- NULL
            ggplot(d) +
                geom_boxplot(
                    mapping=g.aes,
                    notch=input$plot.notch,
                    alpha=input$plot.alpha,
                    width=input$plot.width
               )
        },
        'histogram' = {
            cat('\t - histogram\n')
            cat(str(g.aes))
            # todo BUG here
            ggplot(d) + geom_histogram(mapping=g.aes)
        },
        'point' = {
            cat('\t - point\n')
            ggplot(d) + geom_point(mapping=g.aes)
        },
        'path' = {
            cat('\t - path\n')
            ggplot(d) + geom_path(mapping=g.aes)
        },
        'heatmap' = {
            cat('\t - heatmap\n')
            g.aes$fill <- NULL
            ggplot(d) + geom_tile(mapping=g.aes)
        },
        'density2d' = {
            cat('\t - density2d - NOT IMPLEMENTED\n')
            NULL
        },
        'wordcloud' = {
            cat('\t - wordcloud - NOT IMPLEMENTED\n')
            NULL
        },
        'seq' = {
            cat('\t - seq - NOT IMPLEMENTED\n')
            NULL
        },
        'numseq' = {
            cat('\t - numseq - NOT IMPLEMENTED\n')
            NULL
        },
        'network' = {
            cat('\t - network - NOT IMPLEMENTED\n')
            NULL
        },
        'catseq' = {
            cat('\t - catseq - NOT IMPLEMENTED\n')
            NULL
        },
        NULL
    )
}

get.aes.terms <- function(input){
    cat('\t - plotBuild.R::get.aes.terms\n')
    a <- list()
    a$y        <- input$plot.y
    a$x        <- input$plot.x
    a$color    <- input$plot.aes.color
    a$fill     <- input$plot.aes.fill
    a$size     <- input$plot.aes.size
    a$shape    <- input$plot.aes.shape
    a$group    <- input$plot.aes.group
    a$linetype <- input$plot.aes.linetype
    a <- a[a != 'None']
    a
}

build.aes <- function(input, additional=list()){
    cat('\t - plotBuild.R::build.aes\n')
    a <- get.aes.terms(input)
    if(input$plot.geom %in% c('histogram', 'barplot')){
        a$y <- input$plot.biny
    }
    a <- append(a, additional)
    do.call(aes_string, a)
}

build.dataframe <- function(dataset, input){
    cat('\t - plotBuild.R::build.dataframe\n')
    cols <- unlist(get.aes.terms(input))
    dataset$getDF(cols=cols)
}
