addTheme <- function(g, title="", xlab="", ylab=""){
    g <- g +
           labs(x=xlab, y=ylab, title=title) +
         theme(
            axis.text.x       = element_text(size=14), 
            axis.text.y       = element_text(size=14),
            plot.title        = element_text(size=24, face='bold'),
            axis.title.x      = element_blank(), 
            axis.title.y      = element_blank(),
            legend.title      = element_blank(),
            legend.background = element_blank()
        )
    return(g)
}

makeWordCloud <- function(mat, selection){
    obs.sel <- sort(colSums(mat[selection, ]), decreasing=TRUE)

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

plotText <- function(s, column.name){
    m = global$corpa[[column.name]]
    return(makeWordCloud(m, which(s$selected)))
}

formatNumeric <- function(g, logx){
    if(logx){
        g <- g + scale_x_continuous(trans='log2')
    }
    return(g)
}

plotNumeric <- function(s, ...){
    g <- ggplot(s) +
        geom_histogram(aes(x=value))
    return(formatNumeric(g, ...))
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
    return(formatNumeric(g, ...))
}



formatFactor <- function(g, s){
    # make x-lables vertical is they are longer than 2 characters
    longest.line <- max(nchar(as.character(s$value)))
    if(longest.line > 2) {
        g <- g + theme(axis.text.x = element_text(angle=270, hjust=0, vjust=1))
    }
    return(g)
}

plotFactor <- function(s, ...){
    g <- ggplot(s) +
        geom_bar(aes(x=value))
    return(formatFactor(g, s, ...))
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
    return(formatFactor(g, s, ...))
}

plotPairedNumericNumeric <- function(){}
plotPairedNumericFactor <- function(){}
plotPairedFactorFactor <- function(){}
