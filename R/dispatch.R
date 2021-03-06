as.cat <- function(x){
    cat(sprintf('\t  - as.cat() [name=(%s), type=(%s)]\n', x$name, x$type))
    if(x$type %in% c('num', 'longcat')){
        x$asCat()
    } else {
        x$type <- '-'
    }
    return(x)
}

build.dispatch.table <- function(){ 
    cat('\t - build.dispatch.table()\n')
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

dispatch.table <- build.dispatch.table()
dispatch <- function(x, y, z, fmt.opts){
    cat('\t - dispatch.R::dispatch()\n')
    action <- dispatch.table[x$type, y$type, z$type]
    cat(sprintf('\t  * x=[%s, %s], y=[%s, %s], z=[%s, %s]\n',
                x$name, x$type, y$name, y$type,  z$name, z$type))
    cat(sprintf('\t  * action: %s\n', action))
    return(eval(parse(text=action)))
}
