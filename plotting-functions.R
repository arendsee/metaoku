plotAnything <- function(x, y=NULL, x.name='x', y.name='y', fmt.opts=NULL, corpa=NULL){

    # exit if more than one column is selected
    if(is.null(x) || nrow(x) < 1 || nlevels(x$variable) != 1){
        return()
    }

    stopifnot(is.data.frame(x))
    stopifnot(c('value', 'variable', 'selected') %in% colnames(x))

    x.is_txt <- x.name %in% names(corpa)
    x.is_num <- is.numeric(x$value)
    x.is_fac <- is.factor(x$value)
    is_all <- all(x$selected)
    is_com <- !is.null(y)

    if(x.is_txt){
        m = corpa[[x.name]]
        return(plotText(m=m, rows=which(x$selected)))
    }

    ggtitle <- x.name
    xlab <- NULL
    ylab <- NULL

    if(is_com){
        stopifnot(is.data.frame(y))
        stopifnot(c('value', 'variable', 'selected') %in% colnames(y))
        y.is_num <- is.numeric(y$value)
        y.is_fac <- is.factor(y$value)
        selection <- if(is_all) NULL else ifelse(x$selected, 'selected', 'not-selected')
        ggtitle <- '2-column comparison'
        xlab <- x.name
        ylab <- y.name 
        if(x.is_num && y.is_num){
            g <- plotPairedNumericNumeric(x=x$value, y=y$value, group=selection, fmt.opts)
        } else if (x.is_num && y.is_fac){
            ylab <- x.name
            xlab <- y.name
            fmt.opts$logx = FALSE 
            g <- plotPairedFactorNumeric(y$value, x$value, group=selection, fmt.opts)
        } else if (x.is_fac && y.is_num){
            fmt.opts$logx = FALSE 
            g <- plotPairedFactorNumeric(x$value, y$value, group=selection, fmt.opts)
        } else if (x.is_fac && y.is_fac){
            g <- plotPairedFactorFactor(x$value, y$value, group=selection, fmt.opts)
        } else {
           return() 
        }
    } else {
        if(x.is_num && is_all){
            g <- plotNumeric(x, fmt.opts)
        } else if(x.is_num && !is_all){
            g <- plotSampledNumeric(x, fmt.opts)
        } else if(x.is_fac && is_all){
            g <- plotFactor(x, fmt.opts)
        } else if(x.is_fac && !is_all){
            g <- plotSampledFactor(x, fmt.opts)
        } else {
            return()
        }
    }

    g <- addTheme(g, ggtitle=ggtitle, xlab=xlab, ylab=ylab)
    return(g)
}

addTheme <- function(g, ggtitle=NULL, xlab=NULL, ylab=NULL){
    g <- g +
           labs(x=xlab, y=ylab, title=ggtitle) +
         theme(
            axis.text.x       = element_text(size=14), 
            axis.text.y       = element_text(size=14),
            legend.title      = element_blank(),
            legend.background = element_blank()
        )
    if(is.null(title)){
        g <- g + theme(plot.title = element_blank())
    } else {
        g <- g + theme(plot.title = element_text(size=24, face='bold'))
    }

    if(is.null(xlab)){
        g <- g + theme(axis.title.x = element_blank())
    } else {
        g <- g + theme(axis.title.x = element_text(size=18))
    }

    if(is.null(ylab)){
        g <- g + theme(axis.title.y = element_blank())
    } else {
        g <- g + theme(axis.title.y = element_text(size=18))
    }

    return(g)
}

makeWordCloud <- function(mat, rows){
    obs.sel <- sort(colSums(mat[rows, ]), decreasing=TRUE)

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

plotText <- function(m, rows){
    return(makeWordCloud(m, rows))
}

formatNumeric <- function(g, d, fmt.opts){
    if(fmt.opts$logx){
        g <- g + scale_x_continuous(trans='log2')
    }
    if(fmt.opts$logy){
        g <- g + scale_y_continuous(trans='log2')
    }
    return(g)
}

plotNumeric <- function(s, ...){
    g <- ggplot(s) +
        geom_histogram(aes(x=value))
    return(formatNumeric(g, s, ...))
}

plotSampledNumeric <- function(s, ...){
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
    return(formatNumeric(g, s, ...))
}



formatFactor <- function(g, labs){
    # make x-lables vertical is they are longer than 2 characters
    longest.line <- max(nchar(as.character(labs)))
    if(longest.line > 2) {
        g <- g + theme(axis.text.x = element_text(angle=270, hjust=0, vjust=1))
    }
    return(g)
}

plotFactor <- function(s, ...){
    g <- ggplot(s) +
        geom_bar(aes(x=value))
    return(formatFactor(g, s$value))
}

plotSampledFactor <- function(s, ...){
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
    return(formatFactor(g, s$value))
}

plotPairedNumericNumeric <- function(x, y, group=NULL, ...){
    stopifnot(is.numeric(x), is.numeric(y))
    stopifnot(length(x) == length(y))
    d <- data.frame(x=x, y=y)
    d$group <- group
    g <- ggplot(d) +
        geom_point(aes(x=x, y=y))
    if(!is.null(group)){
        g <- g + facet_grid(group~.)
    }
    return(formatNumeric(g, d, ...))
}

plotPairedFactorNumeric <- function(x, y, group=NULL, ...){
    stopifnot(is.factor(x), is.numeric(y))
    d <- data.frame(x=x, y=y)
    d$group <- group
    g <- ggplot(d) +
        geom_boxplot(aes(x=x, y=y))
    if(!is.null(group)){
        g <- g + facet_grid(group~.)
    }
    g <- formatFactor(g, d$x)
    g <- formatNumeric(g, d, ...)
    return(g)
}

plotPairedFactorFactor <- function(x, y, group=NULL, ...){
    stopifnot(is.factor(x), is.factor(y))
    d <- data.frame(x=x, y=y)
    d$group <- group
    d <- ddply(d, colnames(d), summarize, count=length(x))
    d <- ddply(d, 'x', mutate, rescaled=count / sum(count))
    g <- ggplot(d) +
        geom_tile(aes(x=x, y=y, fill=rescaled))
    if(!is.null(group)){
        g <- g + facet_grid(group~.)
    }
    return(formatFactor(g, d$x))
}
