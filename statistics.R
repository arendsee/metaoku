comparisonSummary <- function(d){
    cat('entering comparisonSummary()\n', stderr())
    if(is.null(d) || nlevels(d$variable) < 1){
        cat('bad input\n', stderr())
        return()
    }

    stopifnot(c('variable', 'value', 'selected') %in% colnames(d))

    if(is.numeric(d$value)){
        cat('\tentering numeric\n', stderr())
        if(!all(d$selected)){
            x = subset(d, selected)$value
            y = subset(d, !selected)$value
            out = data.frame(
                wilcoxon_test=wilcox.test(x, y)$p.value    
            )
        } else {
            return()
        }
    } else if(is.factor(d$value)){
        cat('\tentering factor\n', stderr())
        return() 
    } else {
        cat('\tentering else\n', stderr())
        return()
    }
    return(out)
}

columnSummary <- function(d){
    cat('entering selection_summary_1()\n', stderr())
    if(is.null(d) || nlevels(d$variable) != 1){
        cat('bad input\n', stderr())
        return()
    }

    luniq <- length(unique(d$value))

    if(is.numeric(d$value)){
        cat('\tentering numeric\n', stderr())
        if(all(d$selected)){
            out <- with(d, data.frame( 
                    N=length(value),
                    mean=mean(value),
                    sd=sd(value)
                ))
        } else {
            out <- ddply(d, 'group', summarize,
                       N=length(value),
                       mean=mean(value),
                       sd=sd(value))
        }
    } else if(luniq <= 20){
        cat('\tentering factor\n', stderr())
        if(all(d$selected)){
            return()
        } else {
            out <- ddply(d, 'group', summarize,
                       N=length(value),
                       sd=mean(summary(value)))
        }
    } else {
        cat('\tentering else\n', stderr())
        return()
    }
    return(out)

}
