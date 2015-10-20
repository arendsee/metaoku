buildPlot <- function(dataset, input){
    cat('\tplotBuild.R::buildPlot\n')

    d <- build.dataframe(dataset, input)
    if(length(d) == 0 || nrow(d) == 0){
        return(NULL)
    }
    g.aes <- build.aes(input)

    cat('\t GEOM: ', input$plot.geom, '\n')
    cat(str(g.aes))

    ok <- function(x) if(!is.null(x) && x) TRUE else FALSE
    `%!=%` <- function(x, y) if(!is.null(x) && x != y) TRUE else FALSE
    `%==%` <- function(x, y) if(!is.null(x) && x == y) TRUE else FALSE
    transx <- NULL
    transy <- NULL
    if(input$plot.trans.x %!=% 'none'){
        transx <- scale_x_continuous(trans=input$plot.trans.x)
    }
    if(input$plot.trans.y %!=% 'none'){
        transy <- scale_y_continuous(trans=input$plot.trans.y)
    }

    if(input$plot.facet.x %!=% 'None' && input$plot.facet.y %==% 'None'){
        cat(sprintf('\t - faceting on "%s"\n', input$plot.facet.x))
        cmd <- sprintf('facet_wrap(~ %s, scale = "%s")',
                       input$plot.facet.x, input$plot.facet.scale)
    } else if(input$plot.facet.y %!=% 'None' && input$plot.facet.x %==% 'None'){
        cat(sprintf('\t - faceting on "%s"\n', input$plot.facet.y))
        cmd <- sprintf('facet_wrap(~ %s, scale = "%s")',
                       input$plot.facet.y, input$plot.scale)
    } else if(input$plot.facet.y %!=% 'None' && input$plot.facet.x %!=% 'None'){
        cat(sprintf('\t - faceting on "%s" and "%s"\n', input$plot.facet.x, input$plot.facet.y))
        cmd <- sprintf('facet_grid(%s ~ %s, scale = "%s")',
                       input$plot.facet.x, input$plot.facet.y, input$plot.facet.scale)
    } else {
        cmd = ''
    }
    facet <- eval(parse(text=cmd))

    the <- list()
    if(ok(input$plot.blank.xlab)){
        the$axis.title.x <- element_blank()
    } else if(!is.null(input$plot.xlab.fontsize)){
        the$axis.title.x <- element_text(size=input$plot.xlab.fontsize)
    }
    if(ok(input$plot.blank.ylab)){
        the$axis.title.y <- element_blank()
    } else if(!is.null(input$plot.ylab.fontsize)){
        the$axis.title.y <- element_text(size=input$plot.ylab.fontsize)
    }
    if(ok(input$plot.blank.title)){
        the$plot.title <- element_blank()
    } else if(!is.null(input$plot.title.fontsize)){
        the$plot.title <- element_text(size=input$plot.title.fontsize)
    }
    theme <- do.call(theme, the)
    cat(str(theme))
    ztn <- function(a) if(nchar(a) == 0) NULL else a
    labs       <- list()
    labs$x     <- ztn(input$plot.xlab)
    labs$y     <- ztn(input$plot.ylab)
    labs$title <- ztn(input$plot.title)
    class(labs) <- 'labels'
    labs       <- if(length(labs) == 0) NULL else labs
    cat(str(labs))


    switch(input$plot.geom,
        'barplot' = {
            cat('\t - barplot\n')
            cat(str(g.aes))
            ggplot(d) + geom_bar(mapping=g.aes) + labs + theme
        },
        'boxplot' = {
            cat('\t - boxplot\n')
            # clear unused aesthetics
            g.aes$color     <- NULL
            g.aes$shape     <- NULL
            g.aes$group     <- NULL
            g.aes$linetype  <- NULL
            g.aes$size      <- NULL
            g <- ggplot(d) +
                geom_boxplot(
                    mapping=g.aes,
                    notch=input$plot.notch,
                    alpha=input$plot.alpha,
                    width=input$plot.width
                ) +
                facet + transy + labs + theme
            g
        },
        'histogram' = {
            cat('\t - histogram\n')
            cat(str(g.aes))
            # todo BUG here
            ggplot(d) + geom_histogram(mapping=g.aes) + facet + transx + labs + theme
        },
        'point' = {
            cat('\t - point\n')
            g <- ggplot(d) +
                geom_point(
                    mapping=g.aes,
                    alpha=input$plot.alpha
                ) +
                facet + transx + transy + labs + theme
            g
        },
        'path' = {
            cat('\t - path\n')
            ggplot(d) + geom_path(mapping=g.aes) + labs + theme
        },
        'heatmap' = {
            cat('\t - heatmap\n')
            g.aes$fill <- NULL
            ggplot(d) + geom_tile(mapping=g.aes) + facet + labs + theme
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

get.facet.terms <- function(input){
    a <- c(input$plot.facet.x, input$plot.facet.y)
    a[a != 'None']
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
    cols <- c(unlist(get.aes.terms(input)), get.facet.terms(input))
    dataset$getDF(cols=cols)
}
