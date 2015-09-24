require(ggplot2)
require(wordcloud) 
require(tm)
require(reshape2)

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

logx <- scale_x_continuous(trans='log2')
logy <- scale_y_continuous(trans='log2')

# common word clouds for each category (n wordclouds)
cor.cor.cat.plot <- function(x, y, z, fmt.opts){

}

# common words between the categories (n*m wordclouds)
cor.cat.cat.plot <- function(x, y, z, fmt.opts){

}

# z heatmaps OR z dodged barplots
cat.cat.cat.plot <- function(x, y, z, fmt.opts){

}

# z boxplots (coord flip)
num.cat.cat.plot <- function(x, y, z, fmt.opts){

}

# z boxplots
cat.num.cat.plot <- function(x, y, z, fmt.opts){

}

# z scatter plots OR colored scatter plot
num.num.cat.plot <- function(x, y, z, fmt.opts){

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

}

# as above with coord flip
num.seq.plot <- function(x, y, fmt.opts){

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

}

# boxplot
cat.num.plot <- function(x, y, fmt.opts){
    cat('\tentering cat.num.plot()\n')
    if(nlevels(x$value) > 3){
        g <- ggplot(build.dt(x, y)) +
            geom_boxplot(aes(x=x, y=y)) +
            labs(x=x$name, y=y$name)
        if(fmt.opts$logy){ g <- g + logy }
    } else {
        if(fmt.opts$logy){ fmt.opts$logx <- TRUE }
        g <- num.cat.plot(y, x, fmt.opts)
    }
    g
}

# boxplot coord flip
num.cat.plot <- function(x, y, fmt.opts){
    cat('\tentering num.cat.plot()\n')
    d <- build.dt(x, y)
    if(nlevels(y$value) > 3){
        g <- ggplot(d) +
            geom_boxplot(aes(x=y, y=x)) +
            coord_flip() +
            labs(x=y$name, y=x$name)
        if(fmt.opts$logx){ g <- g + logy }
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
    }
    g
}

# scatter OR density map
num.num.plot <- function(x, y, fmt.opts){
    cat('\tentering num.num.plot()\n')
    g <- ggplot(build.dt(x, y)) +
        geom_point(aes(x=x, y=y)) +
        labs(x=x$name, y=y$name)
    if(fmt.opts$logx){ g <- g + logx }
    if(fmt.opts$logy){ g <- g + logy }
    g
}

# barplot
cat.plot <- function(x, fmt.opts){
    cat('\tentering cat.plot()\n')
    ggplot(build.dt(x)) +
        geom_bar(aes(x=x)) +
        labs(x=x$name)
}

# histogram
num.plot <- function(x, fmt.opts){
    cat('\tentering num.plot()\n')
    g <- ggplot(build.dt(x)) +
        geom_histogram(aes(x=x)) +
        labs(x=x$name)
    if(fmt.opts$logx){ g <- g + logx }
    g
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

}



# makeWordCloud <- function(mat, rows){
#     cat('\tentering makeWordCloud()\n')
#     obs.sel <- sort(colSums(mat[rows, ]), decreasing=TRUE)
# 
#     # # eventually I should do more statistics with this
#     # v.ori <- sort(colSums(mat), decreasing=TRUE)
#     # exp.sel <- sum(v.sel) * (v.ori / sum(v.ori))
#     # sel <- data.frame(exp=exp.sel, obs=obs.sel, rat=log(obs.sel / exp.sel))
# 
#     d <- data.frame(word = names(obs.sel), freq=obs.sel)
#     pal2 <- brewer.pal(8, "Dark2")
#     g = wordcloud(d$word, d$freq,
#                   min.freq=3, max.words=100,
#                   random.order=FALSE,
#                   rot.per=.15,
#                   colors=pal2)
#     return(g)
# }
# 
# plotText <- function(m, rows){
#     cat('\tentering plotText()\n')
#     # For some reason, wordcloud segfaults when there are too few rows
#     if(length(rows) > 5){
#         return(makeWordCloud(m, rows))
#     } else {
#         return()
#     }
# }
# 
# plotNumeric <- function(x){
#     cat('\tentering plotNumeric()\n')
#     d <- data.frame(x=x)
#     g <- ggplot(d) +
#         geom_histogram(aes(x=x))
#     return(g)
# }
# 
# plotSampledNumeric <- function(x, group){
#     cat('\tentering plotSampledNumeric()\n')
#     d <- data.frame(x=x, group=group)
#     g <- ggplot(d) +
#         geom_histogram(
#             aes(
#                 x=x,
#                 y=..density..,
#                 fill=group
#             ),
#             alpha=.75,
#             position='identity'
#         ) + theme(axis.text.y=element_blank(),
#                   axis.ticks.y=element_blank())
#     return(g)
# }
# 
# 
# 
# plotFactor <- function(x){
#     cat('\tentering plotFactor()\n')
#     d <- data.frame(x=x)
#     g <- ggplot(d) +
#         geom_bar(aes(x=x))
#     return(g)
# }
# 
# plotSampledFactor <- function(x, group){
#     cat('\tentering plotSampledFactor()\n')
#     d <- data.frame(x=x, group=group)
#     d <- ddply(d, 'group', mutate, N.group=length(group))
#     d <- ddply(d, c('x', 'group'), mutate, N.x=length(x))
#     d$proportion = d$N.x / d$N.group
#     
#     g <- ggplot(d) +
#         geom_bar(
#             aes(
#                 x=x,
#                 y=proportion,
#                 fill=group
#             ),
#             position='dodge',
#             stat='identity'
#         )
#     return(g)
# }
# 
# plotPairedNumericNumeric <- function(x, y, group=NULL){
#     cat('\tentering plotPairedNumericNumeric()\n')
#     stopifnot(is.numeric(x), is.numeric(y))
#     stopifnot(length(x) == length(y))
#     d <- data.frame(x=x, y=y)
#     d$group <- group
#     g <- ggplot(d) +
#         geom_point(aes(x=x, y=y))
#     if(!is.null(group) && nlevels(group) > 1){
#         g <- g + facet_grid(group~.)
#     }
#     return(g)
# }
# 
# plotPairedFactorNumeric <- function(x, y, group=NULL){
#     cat('\tentering plotPairedFactorNumeric()\n')
#     stopifnot(!((is.factor(x) && is.factor(y)) || (is.numeric(x) && is.numeric(y))))
#     d <- data.frame(x=x, y=y)
#     d$group <- group
#     g <- ggplot(d) +
#         geom_boxplot(aes(x=x, y=y))
#     if(!is.null(group) && nlevels(group) > 1){
#         g <- g + facet_grid(group~.)
#     }
#     return(g)
# }
# 
# plotPairedNumericFactor <- function(x, y, group=NULL){
#     cat('\tentering plotPairedNumericFactor()\n')
#     g <- plotPairedFactorNumeric(x=y, y=x, group=group) + coord_flip()
#     return(g)
# }
# 
# # === plot log(N_exp / N_obs)
# plotPairedFactorFactor <- function(x, y, group=NULL){
#     cat('\tentering plotPairedFactorFactor()\n')
#     stopifnot(is.factor(x), is.factor(y))
#     d <- data.frame(x=x, y=y)
#     d$group <- group
#     d <- ddply(d, colnames(d), summarize, obs=length(x))
#     d <- ddply(d, 'x', mutate, X=sum(obs))
#     d <- ddply(d, 'y', mutate, Y=sum(obs))
#     d$exp <- d$X * d$Y / sum(d$obs)
#     d$lograt <- log(d$obs / d$exp)
#     g <- ggplot(d) +
#         geom_tile(aes(x=x, y=y, fill=lograt))
#     if(!is.null(group) && nlevels(group) > 1){
#         g <- g + facet_grid(group~.)
#     }
#     return(g)
# }
