buildPlot <- function(d, input){
    cat('\tplotBuild.R::buildPlot\n')

    d <- build.dataframe(d, input)
    g.aes <- build.aes(input)

    `%|%` <- function(a, b) if(is.null(a)) b else a

    switch(input$plot.geom,
        'barplot' = {
            cat(str(g.aes))
            ggplot(d) + geom_bar(mapping=g.aes)
        },
        'boxplot' = {
            # clear unused aesthetics
            g.aes$color     <- NULL
            g.aes$fill      <- NULL
            g.aes$shape     <- NULL
            g.aes$group     <- NULL
            g.aes$linetype  <- NULL
            ggplot(d) +
                geom_boxplot(
                    mapping=g.aes,
                    notch=input$plot.notch,
                    alpha=input$plot.alpha,
                    size=input$plot.wdith
                )
        },
        'histogram' = {
            ggplot(d) + geom_histogram(mapping=g.aes)
        },
        'point' = {
            ggplot(d) + geom_point(mapping=g.aes)
        },
        'path' = {
            ggplot(d) + geom_path(mapping=g.aes)
        },
        'heatmap' = {
            ggplot(d) + geom_tile(mapping=g.aes)
        },
        'density2d' = {
            NULL
        },
        'wordcloud' = {
            NULL
        },
        'seq' = {
            NULL
        },
        'numseq' = {
            NULL
        },
        'network' = {
            NULL
        },
        'catseq' = {
            NULL
        },
        NULL
    )
}

get.aes.terms <- function(input){
    cat('\t - plotBuild.R::get.aes.terms ', class(input), '\n')
    a <- list()
    a$y <- input$plot.y
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

build.aes <- function(input){
    cat('\t - plotBuild.R::build.aes\n')
    a <- get.aes.terms(input)
    if(input$plot.geom %in% c('histogram', 'barplot')){
        a$y <- input$plot.biny
    }
    do.call(aes_string, a)
}

build.dataframe <- function(d, input){
    cat('\t - plotBuild.R::build.dataframe\n')
    cols <- unlist(get.aes.terms(input))
    d[, cols, with=FALSE]
}
