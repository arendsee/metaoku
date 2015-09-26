require(ggplot2)
require(wordcloud) 
require(tm)
require(reshape2)
require(plyr)

source('format.R')

build.dt <- function(x, y=NULL, z=NULL){
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
    d <- build.dt(x, y, z)
    d <- ddply(d, colnames(d), summarize, obs=length(x))
    d <- ddply(d, 'x', mutate, X=sum(obs))
    d <- ddply(d, 'y', mutate, Y=sum(obs))
    d$exp <- d$X * d$Y / sum(d$obs)
    d$lograt <- log(d$obs / d$exp)
    g <- ggplot(d) +
        geom_tile(aes(x=x, y=y, fill=lograt)) +
        facet_wrap(~z)
    format.plot(
        g,
        xlab=x$name,
        ylab=y$name,
        x.values=x$value
    )
}

# z boxplots (coord flip)
num.cat.cat.plot <- function(x, y, z, fmt.opts){
    g <- ggplot(build.dt(x, y, z)) +
        geom_boxplot(aes(x=y, y=x)) +
        coord_flip() +
        facet_wrap(~z)
    format.plot(
        g,
        xlab=x$name,
        ylab=y$name,
        logy=fmt.opts$logx,
        x.values=x$value
    )
}

# z boxplots
cat.num.cat.plot <- function(x, y, z, fmt.opts){
    g <- ggplot(build.dt(x, y, z)) +
        geom_boxplot(aes(x=x, y=y)) +
        facet_wrap(~z)
    format.plot(
        g,
        xlab=x$name,
        ylab=y$name,
        logy=fmt.opts$logy,
        x.values=x$value
    )
}

# z scatter plots OR colored scatter plot
num.num.cat.plot <- function(x, y, z, fmt.opts){
    g <- ggplot(build.dt(x, y, z)) +
        geom_point(aes(x=x, y=y)) +
        facet_wrap(~z)
    format.plot(
        g,
        xlab=x$name,
        ylab=y$name,
        logx=fmt.opts$logx,
        logy=fmt.opts$logy,
        x.values=x$value
    )
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
    d <- x$seq
    d$y <- y$value
    g <- ggplot(d) +
        stat_density2d(
            aes(x=prop, y=y, fill=..density..),
            geom='tile',
            contour=FALSE
        ) +
        scale_fill_gradientn(colours = rainbow(7)) +
        facet_wrap(~char)
    format.plot(
        g,
        xlab='Letter',
        ylab=y$name,
        logx=fmt.opts$logx,
        logy=fmt.opts$logy,
        x.values=x$value
    )
}

# as above with coord flip
num.seq.plot <- function(x, y, fmt.opts){
    d <- y$seq
    d$x <- x$value
    g <- ggplot(d) +
        stat_density2d(
            aes(x=x, y=prop, fill=..density..),
            geom='tile',
            contour=FALSE
        ) +
        scale_fill_gradientn(colours = rainbow(7)) +
        facet_wrap(~char)
    format.plot(
        g,
        xlab=x$name,
        ylab='Letter',
        logx=fmt.opts$logx,
        logy=fmt.opts$logy,
        x.values=x$value
    )
}

# y barplots
seq.cat.plot <- function(x, y, fmt.opts){

}

# 3 word clouds (x/y, x+y, y/x)
cor.cor.plot <- function(x, y, fmt.opts){

}

# y*y word clouds
cor.cat.plot <- function(x, y, fmt.opts){

}

# heatmap OR dodged barplot OR network
cat.cat.plot <- function(x, y, fmt.opts){
    d <- build.dt(x, y)
    d <- ddply(d, colnames(d), summarize, obs=length(x))
    d <- ddply(d, 'x', mutate, X=sum(obs))
    d <- ddply(d, 'y', mutate, Y=sum(obs))
    d$exp <- d$X * d$Y / sum(d$obs)
    d$lograt <- log(d$obs / d$exp)
    g <- ggplot(d) +
        geom_tile(aes(x=x, y=y, fill=lograt))
    format.plot(
        g,
        xlab=x$name,
        ylab=y$name,
        x.values=x$value
    )
}

# boxplot
cat.num.plot <- function(x, y, fmt.opts){
    cat('\tentering cat.num.plot()\n')
    if(nlevels(x$value) > 3){
        g <- ggplot(build.dt(x, y)) +
            geom_boxplot(aes(x=x, y=y))
    } else {
        if(fmt.opts$logy){
            fmt.opts$logx <- TRUE
        }
        return(num.cat.plot(y, x, fmt.opts))
    }
    format.plot(
        g,
        xlab=x$name,
        ylab=y$name,
        logy=fmt.opts$logy,
        x.values=x$value
    )
}

# boxplot coord flip
num.cat.plot <- function(x, y, fmt.opts){
    cat('\tentering num.cat.plot()\n')
    d <- build.dt(x, y)
    if(nlevels(y$value) > 3){
        g <- ggplot(d) +
            geom_boxplot(aes(x=y, y=x)) +
            coord_flip()
        return(format.plot(
            g,
            xlab=y$name,
            ylab=x$name,
            logy=fmt.opts$logx,
            x.values=y$value
        ))
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
        if(fmt.opts$logx){ g <- g + logx }
        return(format.plot(
            g,
            xlab=x$name,
            ylab=y$name,
            logx=fmt.opts$logx,
            x.values=x$value
        ))
    }
}

# scatter OR density map
num.num.plot <- function(x, y, fmt.opts){
    cat('\tentering num.num.plot()\n')
    g <- ggplot(build.dt(x, y)) +
        geom_point(aes(x=x, y=y))
    format.plot(
        g,
        xlab=x$name,
        ylab=y$name,
        logx=fmt.opts$logx,
        logy=fmt.opts$logy,
        x.values=x$value
    )
}

# barplot
cat.plot <- function(x, fmt.opts){
    cat('\tentering cat.plot()\n')
    g <- ggplot(build.dt(x)) +
        geom_bar(aes(x=x)) +
        labs(x=x$name)
    format.plot(
        g,
        xlab=x$name,
        logy=fmt.opts$logy,
        x.values=x$value
    )
}

# histogram
num.plot <- function(x, fmt.opts){
    cat('\tentering num.plot()\n')
    g <- ggplot(build.dt(x)) +
        geom_histogram(aes(x=x))
    format.plot(
        g,
        xlab=x$name,
        logx=fmt.opts$logx,
        x.values=x$value
    )
}

# wordcloud
cor.plot <- function(x, fmt.opts){
    cat('\tentering cor.plot()\n')
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
    # check my assumptions about column names
    stopifnot(c('key', 'char', 'count', 'prop') %in% names(x$seq))
    d <- x$seq
    d <- d[, list(median(prop),
                  quantile(prop, probs=0.25, na.rm=TRUE),
                  quantile(prop, probs=0.75, na.rm=TRUE)), by=char]
    setnames(d, c('V1', 'V2', 'V3'), c('prop', 'q25', 'q75'))
    g <- ggplot(d) +
        geom_pointrange(aes(x=char, y=prop, ymin=q25, ymax=q75))
    format.plot(
        g,
        xlab='Letter',
        ylab='Percent composition',
        ggtitle='Sequence composition',
        x.values=x$value
    )
}
