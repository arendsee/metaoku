`%|%` <- function(x,y) { if(is.null(x)) y else x }

build.dt <- function(x, y=NULL, z=NULL){
    cat('\t - build.dt: ', x$name, y$name, z$name, '\n')
    d <- data.frame(x=x$value)
    if(!is.null(y)){
        d$y <- y$value
    }
    if(!is.null(z)){
        d$z <- z$value
    }
    d
}

# common word clouds for each category (n wordclouds)
cor.cor.cat.plot <- function(x, y, z, fmt.opts){

}

# common words between the categories (n*m wordclouds)
cor.cat.cat.plot <- function(x, y, z, fmt.opts){

}

# z heatmaps OR z dodged barplots
cat.cat.cat.plot <- function(x, y, z, fmt.opts){
    cat('\t - cat.cat.cat plot\n')
    d <- build.dt(x, y, z)
    d <- ddply(d, colnames(d), summarize, obs=length(x))
    d <- ddply(d, 'x', mutate, X=sum(obs))
    d <- ddply(d, 'y', mutate, Y=sum(obs))
    d$exp <- d$X * d$Y / sum(d$obs)
    d$lograt <- log(d$obs / d$exp)
    g <- ggplot(d) +
        geom_tile(aes(x=x, y=y, fill=lograt)) +
        facet_wrap(~z)
    fmt.opts$xlab <- fmt.opts$xlab %|% x$name
    fmt.opts$ylab <- fmt.opts$ylab %|% y$name
    format.plot(g, fmt.opts, x=x, y=y, z=z)
}

# z boxplots (coord flip)
num.cat.cat.plot <- function(x, y, z, fmt.opts){
    cat('\t - num.cat.cat plot\n')
    g <- ggplot(build.dt(x, y, z)) +
        geom_boxplot(aes(x=y, y=x)) +
        coord_flip() +
        facet_wrap(~z)
    xlab <- fmt.opts$xlab %|% x$name
    ylab <- fmt.opts$ylab %|% y$name
    fmt.opts$xlab <- ylab
    fmt.opts$ylab <- xlab
    fmt.opts$logy <- fmt.opts$logx
    format.plot(g, fmt.opts, x=x, y=y, z=z)
}

# z boxplots
cat.num.cat.plot <- function(x, y, z, fmt.opts){
    cat('\t - cat.num.cat plot\n')
    g <- ggplot(build.dt(x, y, z)) +
        geom_boxplot(aes(x=x, y=y)) +
        facet_wrap(~z)
    fmt.opts$xlab <- fmt.opts$xlab %|% x$name
    fmt.opts$ylab <- fmt.opts$ylab %|% y$name
    format.plot(g, fmt.opts, x=x, y=y, z=z)
}

# z scatter plots OR colored scatter plot
num.num.cat.plot <- function(x, y, z, fmt.opts){
    cat('\t - num.num.cat plot\n')
    d <- build.dt(x, y, z)
    if(length(x$value) < 2000){
        g <- ggplot(d) +
            geom_point(aes(x=x, y=y)) +
            facet_wrap(~z)
    } else {
        g <- ggplot(d) +
            stat_density2d(
                aes(x=x, y=y, fill=..density..),
                geom='tile',
                contour=FALSE) +
            scale_fill_gradientn(colours = rainbow(7)) +
            facet_wrap(~z)
    }
    fmt.opts$xlab <- fmt.opts$xlab %|% x$name
    fmt.opts$ylab <- fmt.opts$ylab %|% y$name
    fmt.opts$legend.position <- fmt.opts$legend.position %|% 'none'
    format.plot(g, fmt.opts, x=x, y=y, z=z)
}

# z dodged barplots (comparing sequence composition)
seq.seq.cat.plot <- function(x, y, fmt.opts){

}

# y*z barplots
seq.cat.cat.plot <- function(x, y, fmt.opts){

}

# dodged barplot of sequence composition
seq.seq.plot <- function(x, y, fmt.opts){

}

# scatter/density map for character in seq versus num
seq.num.plot <- function(x, y, fmt.opts){
    cat('\t - seq.num plot\n')
    d <- x$char.table
    d$y <- y$value
    g <- ggplot(d) +
        stat_density2d(
            aes(x=prop, y=y, fill=..density..),
            geom='tile',
            contour=FALSE
        ) +
        scale_fill_gradientn(colours = rainbow(7)) +
        facet_wrap(~char)
    fmt.opts$xlab <- fmt.opts$xlab %|% 'Letter'
    fmt.opts$ylab <- fmt.opts$ylab %|% y$name
    fmt.opts$legend.position <- fmt.opts$legend.position %|% 'none'
    format.plot(g, fmt.opts, x=x, y=y, z=z)
}

# as above with coord flip
num.seq.plot <- function(x, y, fmt.opts){
    cat('\t - num.seq plot\n')
    d <- y$char.table
    d$x <- x$value
    g <- ggplot(d) +
        stat_density2d(
            aes(x=x, y=prop, fill=..density..),
            geom='tile',
            contour=FALSE
        ) +
        scale_fill_gradientn(colours = rainbow(7)) +
        facet_wrap(~char)
    fmt.opts$xlab <- fmt.opts$xlab %|% x$name
    fmt.opts$ylab <- fmt.opts$ylab %|% 'Letter'
    fmt.opts$legend.position <- fmt.opts$legend.position %|% 'none'
    format.plot(g, fmt.opts, x=x, y=y)
}

# y barplots
seq.cat.plot <- function(x, y, fmt.opts){
    stopifnot(c('key', 'char', 'count', 'prop') %in% names(x$char.table))
    d <- x$char.table
    d$y <- y$value
    d <- d[, list(median(prop),
                  quantile(prop, probs=0.25, na.rm=TRUE),
                  quantile(prop, probs=0.75, na.rm=TRUE)), by=.(char, y)]
    setnames(d, c('V1', 'V2', 'V3'), c('prop', 'q25', 'q75'))
    g <- ggplot(d) +
        geom_pointrange(aes(x=char, y=prop, ymin=q25, ymax=q75)) +
        facet_wrap(~y)
    fmt.opts$xlab <- fmt.opts$xlab %|% 'Letter'
    fmt.opts$ylab <- fmt.opts$ylab %|% 'Percent composition'
    fmt.opts$title <- fmt.opts$title %|% 'Sequence composition'
    format.plot(g, fmt.opts, x=x, y=y)
}

# 3 word clouds (x/y, x+y, y/x)
cor.cor.plot <- function(x, y, fmt.opts){

}

# y*y word clouds
cor.cat.plot <- function(x, y, fmt.opts){

}

# heatmap OR dodged barplot OR network
cat.cat.plot <- function(x, y, fmt.opts){
    cat('\t - cat.cat plot\n')
    d <- build.dt(x, y)
    if(nlevels(d$y) <= 3){
        d <- count(d) %>%
             ddply('y', mutate, p=freq/sum(freq))
        g <- ggplot(d) +
            geom_bar(aes(x=x, y=p, fill=y), position='dodge', stat='identity')

    } else {
        d <- ddply(d, colnames(d), summarize, obs=length(x))
        d <- ddply(d, 'x', mutate, X=sum(obs))
        d <- ddply(d, 'y', mutate, Y=sum(obs))
        d$exp <- d$X * d$Y / sum(d$obs)
        d$lograt <- log(d$obs / d$exp)
        g <- ggplot(d) +
            geom_tile(aes(x=x, y=y, fill=lograt))
    }
        fmt.opts$xlab <- fmt.opts$xlab %|% x$name
        fmt.opts$ylab <- fmt.opts$ylab %|% y$name
        format.plot(g, fmt.opts, x=x, y=y)
}

# boxplot
cat.num.plot <- function(x, y, fmt.opts){
    cat('\t - cat.num plot\n')
    if(nlevels(x$value) > 3){
        g <- ggplot(build.dt(x, y)) +
            geom_boxplot(aes(x=x, y=y))
        fmt.opts$xlab <- fmt.opts$xlab %|% x$name
        fmt.opts$ylab <- fmt.opts$ylab %|% y$name
        format.plot(g, fmt.opts, x=x, y=y)
    } else {
        if(fmt.opts$logy){
            fmt.opts$logx <- TRUE
            fmt.opts$logy <- FALSE
        }
        g <- num.cat.plot(y, x, fmt.opts)
    }
    return(g)
}

# boxplot coord flip
num.cat.plot <- function(x, y, fmt.opts){
    cat('\t - num.cat plot\n')
    d <- build.dt(x, y)
    if(nlevels(y$value) > 3){
        g <- ggplot(d) +
            geom_boxplot(aes(x=y, y=x)) +
            coord_flip()
        xlab <- fmt.opts$ylab %|% y$name
        ylab <- fmt.opts$xlab %|% x$name
        fmt.opts$xlab <- xlab
        fmt.opts$ylab <- ylab
        fmt.opts$logy <- fmt.opts$logx
        format.plot(g, fmt.opts, x=x, y=y)
    } else {
        g <- ggplot(d) +
            geom_histogram(
                aes(
                    x=x,
                    y=..density..,
                    fill=y
                ),
                alpha=.75,
                position='identity'
            ) + theme(axis.text.y=element_blank(),
                      axis.ticks.y=element_blank()) +
              labs(x=x$name)
        fmt.opts$xlab <- fmt.opts$xlab %|% x$name
        fmt.opts$ylab <- 'density'
        format.plot(g, fmt.opts, x=x, y=y)
    }
}

# scatter OR density map
num.num.plot <- function(x, y, fmt.opts){
    cat('\t - num.num plot\n')
    d <- build.dt(x, y)
    if(length(x$value) < 2000){
        g <- ggplot(d) +
            geom_point(aes(x=x, y=y))
    } else {
        g <- ggplot(d) +
            stat_density2d(
                aes(x=x, y=y, fill=..density..),
                geom='tile',
                contour=FALSE) +
            scale_fill_gradientn(colours = rainbow(7))
    }
    fmt.opts$xlab <- fmt.opts$xlab %|% x$name
    fmt.opts$ylab <- fmt.opts$ylab %|% y$name
    fmt.opts$legend.position <- fmt.opts$legend.position %|% 'none'
    format.plot(g, fmt.opts, x=x, y=y)
}

# barplot
cat.plot <- function(x, fmt.opts){
    cat('\t - cat plot\n')
    g <- ggplot(build.dt(x)) +
        geom_bar(aes(x=x)) +
        labs(x=x$name)
    fmt.opts$xlab <- fmt.opts$xlab %|% x$name
    fmt.opts$ylab <- fmt.opts$ylab %|% 'Counts'
    format.plot(g, fmt.opts, x=x)
}

# histogram
num.plot <- function(x, fmt.opts){
    cat('\t - num plot\n')
    g <- ggplot(build.dt(x)) +
        geom_histogram(aes(x=x))
    fmt.opts$xlab <- fmt.opts$xlab %|% x$name
    fmt.opts$ylab <- fmt.opts$ylab %|% NULL
    format.plot(g, fmt.opts, x=x)
}

# wordcloud
cor.plot <- function(x, fmt.opts){
    cat('\t - cor plot\n')
    obs.sel <- sort(colSums(x$mat), decreasing=TRUE)
    d <- data.frame(word = names(obs.sel), freq=obs.sel)
    pal2 <- brewer.pal(8, "Dark2")
    wordcloud(d$word, d$freq,
              min.freq=3, max.words=100,
              random.order=FALSE,
              rot.per=.15,
              colors=pal2)
}

# composition barplot
seq.plot <- function(x, fmt.opts){
    cat('\t - seq plot\n')
    # check my assumptions about column names
    stopifnot(c('key', 'char', 'count', 'prop') %in% names(x$char.table))
    d <- x$char.table
    d <- d[, list(median(prop),
                  quantile(prop, probs=0.25, na.rm=TRUE),
                  quantile(prop, probs=0.75, na.rm=TRUE)), by=char]
    setnames(d, c('V1', 'V2', 'V3'), c('prop', 'q25', 'q75'))
    g <- ggplot(d) +
        geom_pointrange(aes(x=char, y=prop, ymin=q25, ymax=q75))
    fmt.opts$xlab <- fmt.opts$xlab %|% 'Letter'
    fmt.opts$ylab <- fmt.opts$ylab %|% 'Percent composition'
    fmt.opts$title <- fmt.opts$title %|% 'Sequence composition'
    format.plot(g, fmt.opts, x=x)
}

format.plot <- function(g, fmt.opts, x=NULL, y=NULL, z=NULL){
    cat('\tformat.R::formatPlot()\n')
    if(!is.null(x) && x$type == 'num' && fmt.opts$logx){
        g <- g + scale_x_continuous(trans='log2')
    }

    if(!is.null(y) && y$type == 'num' && fmt.opts$logy){
        g <- g + scale_y_continuous(trans='log2')
    }

    if(x$type %in% c('cat', 'longcat')){
        # make x-lables vertical is they are longer than 2 characters
        longest.line <- max(nchar(levels(x$value)), rm.na=TRUE)
        if(longest.line > 2) {
            g <- g + theme(axis.text.x = element_text(angle=270, hjust=0, vjust=1))
        }
    }

    if(is.null(fmt.opts$legend.position)){
        fmt.opts$legend.position = 'bottom'
    }

    g <- g +
           labs(x=fmt.opts$xlab, y=fmt.opts$ylab, title=fmt.opts$title) +
         theme(
            axis.text.x       = element_text(size=14), 
            axis.text.y       = element_text(size=14),
            strip.text        = element_text(size=14),
            legend.text       = element_text(size=14),
            legend.title      = element_blank(),
            legend.background = element_blank(),
            legend.position   = fmt.opts$legend.position
        )
    if(is.null(fmt.opts$title)){
        g <- g + theme(plot.title = element_blank())
    } else {
        g <- g + theme(plot.title = element_text(size=24, face='bold'))
    }

    if(is.null(fmt.opts$xlab)){
        g <- g + theme(axis.title.x = element_blank())
    } else {
        g <- g + theme(axis.title.x = element_text(size=18))
    }

    if(is.null(fmt.opts$ylab)){
        g <- g + theme(axis.title.y = element_blank())
    } else {
        g <- g + theme(axis.title.y = element_text(size=18))
    }

    if(!is.null(fmt.opts$xrange)){
        g <- g + xlim(fmt.opts$xrange)
    }

    if(!is.null(fmt.opts$yrange)){
        g <- g + ylim(fmt.opts$yrange)
    }

    return(g)
}
