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
