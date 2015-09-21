require(plyr)

comparisonSummary <- function(d){
    cat('\tentering comparisonSummary()\n')

    if(is.null(d)){ return() }

    stopifnot(c('value', 'selected', 'group') %in% colnames(d))

    if(is.numeric(d$value)){
        cat('\t  * entering numeric\n')
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
        cat('\t  * entering factor\n')
        return() 
    } else {
        cat('\t  * entering else\n')
        return()
    }
    return(out)
}

columnSummary <- function(d){
    cat('\tentering columnSummary()\n')

    if(is.null(d)){ return() }

    stopifnot(c('value', 'selected', 'group') %in% colnames(d))

    luniq <- length(unique(d$value))

    if(is.numeric(d$value)){
        cat('\t  * entering numeric\n')
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
        cat('\t  * entering factor\n')
        if(all(d$selected)){
            return()
        } else {
            out <- ddply(d, 'group', summarize,
                       N=length(value),
                       sd=mean(summary(value)))
        }
    } else {
        cat('\t  * entering else\n')
        return()
    }
    return(out)
}
