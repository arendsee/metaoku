NUM2CAT_LEVELS <- 7
MAX_LEVELS <- 20

num2cat <- function(x){
    cut(x, breaks=NUM2CAT_LEVELS)
}

longcat2cat <- function(x){
    x <- as.factor(x)
    top.n <- names(summary(xs, maxsum=MAX_LEVELS))[-MAX_LEVELS]
    x <- as.character(x)
    x <- ifelse(x %in% top.n, x, 'other')
    x <- as.factor(x, levels=c(top.n, 'other'))
    x
}

as.cat <- function(x){
  if(x$type == 'num'){
      x$type <- 'cat'
      x$value <- num2cat(x)  
  } else if(x$type == 'longcat'){
      x$type = 'cat'
      x$value <- longcat2cat(x$value)
  } else if(x$type == 'seq'){
      # not currently supported
      x$type = '-'
  } else if(x$type == 'cor'){
      # not currently supported
      x$type = '-'
  }
  return(x)
}

build.dispatch.table <- function(){ 
    require(reshape2)
    types <- c('-', 'num', 'cat', 'longcat', 'cor', 'seq')
    d <- expand.grid(types, types, types)
    colnames(d) <- c('x', 'y', 'z')

    i <- c(1:6)
    names(i) <- types

    # initialiaze everything to NA
    d$fun <- NA

    d <- acast(d, x ~ y ~ z)

    # --- if z is present and not categorical, convert
    d[, , -i[c('-', 'cat')]] <- 'dispatch(x, y, as.cat(z), fmt.opts)' 

    # --- convert all longcat to cat
    d[,'longcat',] <- 'dispatch(x, as.cat(y), z, fmt.opts)'
    d['longcat',,] <- 'dispatch(as.cat(x), y, z, fmt.opts)'

    # --- if y is seq and x isn't, switch
    # d[,'seq',]     <- 'dispatch(x, as.cat(y), z, fmt.opts)'

    # --- if z is defined but not y, promote z
    d[, '-', -i['-']] <- 'dispatch(x, z, y, fmt.opts)'

    # --- do nothing if x is undefined
    d['-', , ] <- ''

    # --- convert num to cat if anything is cor
    d['num', 'cor', c('-', 'cat')] <- 'dispatch(as.cat(x), y, z, fmt.opts)'
    d['cor', 'num', c('-', 'cat')] <- 'dispatch(x, as.cat(y), z, fmt.opts)'

    # --- can't compare seq and cor
    d['seq', 'cor', ] <- ''
    d['cor', 'seq', ] <- ''

    # --- seq vs num plots already make nlevels of plots, can't do more
    d['seq', 'num', 'cat'] <- ''
    d['num', 'seq', 'cat'] <- ''

    # --- order doesn't matter in these cases
    d['cat', 'seq', ] <- 'dispatch(y, x, z, fmt.opts)'
    d['cat', 'cor', ] <- 'dispatch(y, x, z, fmt.opts)'


    # --- plotting operations --------------------------------------------
    # every other xyz combination is either NULL or points to one of these
    # --------------------------------------------------------------------

    # common word clouds for each category (n wordclouds)
    d['cor', 'cor', 'cat'] <- 'cor.cor.cat.plot(x, y, z, fmt.opts)'
    # common words between the categories (n*m wordclouds)
    d['cor', 'cat', 'cat'] <- 'cor.cat.cat.plot(x, y, z, fmt.opts)'
    # z heatmaps OR z dodged barplots
    d['cat', 'cat', 'cat'] <- 'cat.cat.cat.plot(x, y, z, fmt.opts)'
    # z boxplots (coord flip)
    d['num', 'cat', 'cat'] <- 'num.cat.cat.plot(x, y, z, fmt.opts)'
    # z boxplots
    d['cat', 'num', 'cat'] <- 'cat.num.cat.plot(x, y, z, fmt.opts)'
    # z scatter plots OR colored scatter plot
    d['num', 'num', 'cat'] <- 'num.num.cat.plot(x, y, z, fmt.opts)'
    # z dodged barplots (comparing sequence composition)
    d['seq', 'seq', 'cat'] <- 'seq.seq.cat.plot(x, y, fmt.opts)'
    # y*z barplots
    d['seq', 'cat', 'cat'] <- 'seq.cat.cat.plot(x, y, fmt.opts)'

    # dodged barplot of sequence composition
    d['seq', 'seq', '-'] <- 'seq.seq.plot(x, y, fmt.opts)'
    # scatter/density map for character in seq versus num
    d['seq', 'num', '-'] <- 'seq.num.plot(x, y, fmt.opts)'
    # as above with coord flip
    d['num', 'seq', '-'] <- 'num.seq.plot(x, y, fmt.opts)'
    # y barplots
    d['seq', 'cat', '-'] <- 'seq.cat.plot(x, y, fmt.opts)'
    # 3 word clouds (x/y, x+y, y/x)
    d['cor', 'cor', '-'] <- 'cor.cor.plot(x, y, fmt.opts)'
    # y*y word clouds
    d['cor', 'cat', '-'] <- 'cor.cat.plot(x, y, fmt.opts)'
    # heatmap OR dodged barplot OR network
    d['cat', 'cat', '-'] <- 'cat.cat.plot(x, y, fmt.opts)'
    # boxplot
    d['cat', 'num', '-'] <- 'cat.num.plot(x, y, fmt.opts)'
    # boxplot coord flip
    d['num', 'cat', '-'] <- 'num.cat.plot(x, y, fmt.opts)'
    # scatter OR density map
    d['num', 'num', '-'] <- 'num.num.plot(x, y, fmt.opts)'

    # barplot
    d['cat', '-', '-'] <- 'cat.plot(x, fmt.opts)'
    # histogram
    d['num', '-', '-'] <- 'num.plot(x, fmt.opts)'
    # wordcloud
    d['cor', '-', '-'] <- 'cor.plot(x, fmt.opts)'
    # composition barplot
    d['seq', '-', '-'] <- 'seq.plot(x, fmt.opts)'

    # assert all possibilities are accounted for
    stopifnot(!is.na(d))

    return(d)
}

formatPlot <- function(g,
                       logx=FALSE,
                       logy=FALSE,
                       x.values=NULL,
                       ggtitle=NULL,
                       xlab=NULL,
                       ylab=NULL){
    cat('\tentering formatPlot()\n')
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
            strip.text        = element_text(size=14),
            legend.text       = element_text(size=14),
            legend.title      = element_blank(),
            legend.background = element_blank(),
            legend.position   = 'bottom'
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

dispatch <- function(x, y, z, fmt.opts){

}

plotAnything <- function(x=x, y=y, z=z, fmt.opts=fmt.opts, corpa=global$corpa){
    cat('\tentering plotAnything()\n')

    g <- dispatch(x, y, z, fmt.opts)

    return(g)
}

makeWordCloud <- function(mat, rows){
    cat('\tentering makeWordCloud()\n')
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
    cat('\tentering plotText()\n')
    # For some reason, wordcloud segfaults when there are too few rows
    if(length(rows) > 5){
        return(makeWordCloud(m, rows))
    } else {
        return()
    }
}

plotNumeric <- function(x){
    cat('\tentering plotNumeric()\n')
    d <- data.frame(x=x)
    g <- ggplot(d) +
        geom_histogram(aes(x=x))
    return(g)
}

plotSampledNumeric <- function(x, group){
    cat('\tentering plotSampledNumeric()\n')
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
    cat('\tentering plotFactor()\n')
    d <- data.frame(x=x)
    g <- ggplot(d) +
        geom_bar(aes(x=x))
    return(g)
}

plotSampledFactor <- function(x, group){
    cat('\tentering plotSampledFactor()\n')
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
    cat('\tentering plotPairedNumericNumeric()\n')
    stopifnot(is.numeric(x), is.numeric(y))
    stopifnot(length(x) == length(y))
    d <- data.frame(x=x, y=y)
    d$group <- group
    g <- ggplot(d) +
        geom_point(aes(x=x, y=y))
    if(!is.null(group) && nlevels(group) > 1){
        g <- g + facet_grid(group~.)
    }
    return(g)
}

plotPairedFactorNumeric <- function(x, y, group=NULL){
    cat('\tentering plotPairedFactorNumeric()\n')
    stopifnot(!((is.factor(x) && is.factor(y)) || (is.numeric(x) && is.numeric(y))))
    d <- data.frame(x=x, y=y)
    d$group <- group
    g <- ggplot(d) +
        geom_boxplot(aes(x=x, y=y))
    if(!is.null(group) && nlevels(group) > 1){
        g <- g + facet_grid(group~.)
    }
    return(g)
}

plotPairedNumericFactor <- function(x, y, group=NULL){
    cat('\tentering plotPairedNumericFactor()\n')
    g <- plotPairedFactorNumeric(x=y, y=x, group=group) + coord_flip()
    return(g)
}

# === plot log(N_exp / N_obs)
plotPairedFactorFactor <- function(x, y, group=NULL){
    cat('\tentering plotPairedFactorFactor()\n')
    stopifnot(is.factor(x), is.factor(y))
    d <- data.frame(x=x, y=y)
    d$group <- group
    d <- ddply(d, colnames(d), summarize, obs=length(x))
    d <- ddply(d, 'x', mutate, X=sum(obs))
    d <- ddply(d, 'y', mutate, Y=sum(obs))
    d$exp <- d$X * d$Y / sum(d$obs)
    d$lograt <- log(d$obs / d$exp)
    g <- ggplot(d) +
        geom_tile(aes(x=x, y=y, fill=lograt))
    if(!is.null(group) && nlevels(group) > 1){
        g <- g + facet_grid(group~.)
    }
    return(g)
}
