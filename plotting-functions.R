formatPlot <- function(g,
                       logx=FALSE,
                       logy=FALSE,
                       x.values=NULL,
                       ggtitle=NULL,
                       xlab=NULL,
                       ylab=NULL){
    if(logx){
        g <- g + scale_x_continuous(trans='log2')
    }

    if(logy){
        g <- g + scale_y_continuous(trans='log2')
    }

    if(!is.null(x.values) && is.factor(x.values)){
        # make x-lables vertical is they are longer than 2 characters
        longest.line <- max(nchar(as.character(x.values)))
        if(longest.line > 2 && is.factor(x.values)) {
            g <- g + theme(axis.text.x = element_text(angle=270, hjust=0, vjust=1))
        }
    }

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

plotAnything <- function(x, y=NULL, group=NULL, selected=NULL,
                         x.name='x', y.name='y', fmt.opts=NULL, corpa=NULL){
    cat('entering plotAnything\n', stderr())

    if(is.null(x) || length(x) == 0){
        cat('\tstuff is null\n', stderr())
        return()
    }

    if(!is.null(group)) { group <- factor(group) }

    x.is_txt  <- x.name %in% names(corpa)
    x.is_num  <- is.numeric(x)
    x.is_fac  <- is.factor(x)
    is_single <- is.null(group) || nlevels(group) == 1
    is_com <- !is.null(y)

    logx <- fmt.opts$logx && is.numeric(x)
    logy <- fmt.opts$logy && is_com && is.numeric(y)

    if(x.is_txt){
        if(is.null(selected)){
            rows <- 1:length(x)
        } else {
            rows <- which(selected)
        }
        m = corpa[[x.name]]
        return(plotText(m=m, rows=rows))
    }
    
    # ignore huge factors
    if (is.factor(x) && nlevels(x) > 20){
        return()
    }

    ggtitle <- x.name
    xlab <- NULL
    ylab <- NULL

    g <- NULL
    if(is_com){
        y.is_num <- is.numeric(y)
        y.is_fac <- is.factor(y)
        y.is_txt <- y.name %in% names(corpa)
        ggtitle <- '2-column comparison'
        xlab <- x.name
        ylab <- y.name 
        if(y.is_txt){
            # cannot (yet) compare anything to text
            return()
        } else if(x.is_num && y.is_num){
            func <- plotPairedNumericNumeric
        } else if (x.is_num && y.is_fac){
            # I flip the coordinates, so need to swap values
            tmp  <- xlab
            xlab <- ylab
            ylab <- tmp
            tmp  <- logx
            logx <- logy
            logy <- tmp
            rm(tmp)
            func <- plotPairedNumericFactor
        } else if (x.is_fac && y.is_num){
            func <- plotPairedFactorNumeric
        } else if (x.is_fac && y.is_fac){
            func <- plotPairedFactorFactor
        } else {
           return() 
        }
        g <- func(x=x, y=y, group=group)
    } else {
        if(is_single){
            if(x.is_num){
                func <- plotNumeric
            } else if(x.is_fac){
                func <- plotFactor
            }
            g <- func(x=x)
        } else {
            if(x.is_num){
                func <- plotSampledNumeric
            } else if(x.is_fac){
                func <- plotSampledFactor
            }
            g <- func(x=x, group=group)
        }
    }

    if(!is.null(g)){
        g <- formatPlot(g, logx=logx, logy=logy, x.values=x,
                        ggtitle=ggtitle, xlab=xlab, ylab=ylab)
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

plotNumeric <- function(x){
    d <- data.frame(x=x)
    g <- ggplot(d) +
        geom_histogram(aes(x=x))
    return(g)
}

plotSampledNumeric <- function(x, group){
    d <- data.frame(x=x, group=group)
    g <- ggplot(d) +
        geom_histogram(
            aes(
                x=x,
                y=..density..,
                fill=group
            ),
            alpha=.75,
            position='identity'
        ) + theme(axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
    return(g)
}



plotFactor <- function(x){
    d <- data.frame(x=x)
    g <- ggplot(d) +
        geom_bar(aes(x=x))
    return(g)
}

plotSampledFactor <- function(x, group){
    d <- data.frame(x=x, group=group)
    d <- ddply(d, 'group', mutate, N.group=length(group))
    d <- ddply(d, c('x', 'group'), mutate, N.x=length(x))
    d$proportion = d$N.x / d$N.group
    
    g <- ggplot(d) +
        geom_bar(
            aes(
                x=x,
                y=proportion,
                fill=group
            ),
            position='dodge',
            stat='identity'
        )
    return(g)
}

plotPairedNumericNumeric <- function(x, y, group=NULL){
    stopifnot(is.numeric(x), is.numeric(y))
    stopifnot(length(x) == length(y))
    d <- data.frame(x=x, y=y)
    d$group <- group
    g <- ggplot(d) +
        geom_point(aes(x=x, y=y))
    if(!is.null(group)){
        g <- g + facet_grid(group~.)
    }
    return(g)
}

plotPairedFactorNumeric <- function(x, y, group=NULL){
    stopifnot(!((is.factor(x) && is.factor(y)) || (is.numeric(x) && is.numeric(y))))
    d <- data.frame(x=x, y=y)
    d$group <- group
    g <- ggplot(d) +
        geom_boxplot(aes(x=x, y=y))
    if(!is.null(group)){
        g <- g + facet_grid(group~.)
    }
    return(g)
}

plotPairedNumericFactor <- function(x, y, group=NULL){
    g <- plotPairedFactorNumeric(x=y, y=x, group=group) + coord_flip()
    return(g)
}

plotPairedFactorFactor <- function(x, y, group=NULL){
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
    return(g)
}
