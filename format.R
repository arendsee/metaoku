format.plot <- function(g, x.values=NULL, fmt.opts){
    cat('\tformat.R::formatPlot()\n')
    if(fmt.opts$logx){
        g <- g + scale_x_continuous(trans='log2')
    }

    if(fmt.opts$logy){
        g <- g + scale_y_continuous(trans='log2')
    }

    if(!is.null(x.values) && is.factor(x.values)){
        # make x-lables vertical is they are longer than 2 characters
        longest.line <- max(nchar(as.character(x.values)))
        if(longest.line > 2 && is.factor(x.values)) {
            g <- g + theme(axis.text.x = element_text(angle=270, hjust=0, vjust=1))
        }
    }

    if(is.null(fmt.opts$legend.position)){
        fmt.opts$legend.position = 'bottom'
    }

    g <- g +
           labs(x=fmt.opts$xlab, y=fmt.opts$ylab, title=fmt.opts$title) +
         theme(
            axis.text.x       = element_text(size=14), 
            axis.text.y       = element_text(size=14),
            strip.text        = element_text(size=14),
            legend.text       = element_text(size=14),
            legend.title      = element_blank(),
            legend.background = element_blank(),
            legend.position   = fmt.opts$legend.position
        )
    if(is.null(fmt.opts$title)){
        g <- g + theme(plot.title = element_blank())
    } else {
        g <- g + theme(plot.title = element_text(size=24, face='bold'))
    }

    if(is.null(fmt.opts$xlab)){
        g <- g + theme(axis.title.x = element_blank())
    } else {
        g <- g + theme(axis.title.x = element_text(size=18))
    }

    if(is.null(fmt.opts$ylab)){
        g <- g + theme(axis.title.y = element_blank())
    } else {
        g <- g + theme(axis.title.y = element_text(size=18))
    }

    if(!is.null(fmt.opts$xrange)){
        g <- g + xlim(fmt.opts$xrange)
    }

    if(!is.null(fmt.opts$yrange)){
        g <- g + ylim(fmt.opts$yrange)
    }

    return(g)
}
