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
