require(magrittr)

# TODO - find a way to make these global
`%ifnul%` <- function(a, b) if(is.null(a)) b else a
`%ifnot%` <- function(a, b) if(is.null(a) || is.na(a) || length(a) == 0) b else a
`%ifok%` <- function(a, b) if(!(is.null(a) || is.na(a) || length(a) == 0)) b

getGeoMap <- function(){
    geomap <- matrix(rep(NA, 36), ncol=6)
    colnames(geomap) <- rownames(geomap) <- c('-', 'cat', 'longcat', 'cor', 'num', 'seq')

    geomap[ , '-'] <- NA

    geomap['cat', '-']   <- 'barplot'
    geomap['cat', 'cat'] <- 'heatmap,network'
    geomap['cat', 'cor'] <- NA
    geomap['cat', 'num'] <- 'boxplot,network'
    geomap['cat', 'seq'] <- 'catseq'

    geomap['longcat', '-']   <- 'barplot'
    geomap['longcat', 'cat'] <- 'heatmap,network'
    geomap['longcat', 'cor'] <- NA
    geomap['longcat', 'num'] <- 'boxplot,network'
    geomap['longcat', 'seq'] <- 'catseq'
    geomap['cat', 'longcat'] <- 'heatmap,network'
    geomap['cor', 'longcat'] <- NA
    geomap['num', 'longcat'] <- 'boxplot'
    geomap['seq', 'longcat'] <- 'catseq'

    geomap['cor', '-']   <- 'wordcloud'
    geomap['cor', 'cat'] <- NA
    geomap['cor', 'cor'] <- NA
    geomap['cor', 'num'] <- NA
    geomap['cor', 'seq'] <- NA

    geomap['num', '-']   <- 'histogram'
    geomap['num', 'cat'] <- 'boxplot'
    geomap['num', 'cor'] <- NA
    geomap['num', 'num'] <- 'point,density2d,path'
    geomap['num', 'seq'] <- 'numseq'

    geomap['seq', '-']   <- 'seq'
    geomap['seq', 'cat'] <- 'catseq'
    geomap['seq', 'cor'] <- NA
    geomap['seq', 'num'] <- 'numseq'
    geomap['seq', 'seq'] <- NA

    return(geomap)
}

getChoices <- function(types, accepted=types){
    c('None', names(types[types %in% accepted]))
}

getYChoices <- function(types, x){
    geomap <- getGeoMap()
    choices <- apply(!is.na(geomap), 1, function(val) colnames(geomap)[val]) %>%
               lapply(function(val) if(length(val) == 0) '-' else val)
    xtype <- types[x]
    choices <- names(types[types %in% choices[[xtype]]])
    return(choices)
}

getGeomChoices <- function(xtype, ytype='-'){
    cat(sprintf('\tplotUI::getGeomChoices xtype:(%s), ytype:(%s)\n', xtype, ytype))
    return(unlist(strsplit(getGeoMap()[xtype, ytype], ',')))
}

plot.select.x <- function(types, selected='None') {
    selectInput('plot.x', 'Independent variable', choices=names(types), selected=selected)
}

plot.build.ggplot.ui <- function(taglist, geom, elements){
    cat(sprintf('\t - plotUI::plot.build.ggplot.ui() geom: %s\n', geom))

    if(is.na(geom)) return(taglist)

    # Append a value to the end of a list
    `%a%` <- function(a, b) { a[[length(a) + 1]] <- b; a}

    dorow <- function(x, w){
        columns <- list(width=12)
        for(i in 1:length(x)){
            if(! x[i] %in% names(elements)){
                cat(sprintf('\t - WARNING: %s is not in elemenets\n', x[i]))
            }
            columns <- columns %a% column(w[i], elements[[x[i]]])
        }
        do.call(fluidRow, columns)
    }

    taglist <- switch(geom,
        'barplot' = {
            append(taglist, list(
                dorow(c('color.by', 'fill.by'), c(4,4,4)),
                elements$facet_wrap,
                elements$facet_grid
            ))
        },
        'boxplot' = {
            append(taglist, list(
                dorow(c('alpha', 'width', 'notch'), c(5,5,2)),
                elements$facet_wrap,
                elements$facet_grid
            ))
        },
        'histogram' = {
            append(taglist, list(
                dorow(c('color.by', 'fill.by', 'density'), c(5,5,2)),
                elements$facet_wrap,
                elements$facet_grid))
        },
        'point' = {
            append(taglist, list(
                dorow(c('color.by', 'shape.by', 'size.by'), c(4,4,4)),
                dorow(c('alpha', 'size'), c(6,6)),
                elements$facet_wrap,
                elements$facet_grid
            ))
        },
        'path' = {
            append(taglist, list(
                dorow(c('group.by', 'color.by', 'size.by', 'linetype.by'), c(3,3,3,3)),
                dorow(c('alpha', 'size'), c(6,6)),
                elements$facet_wrap,
                elements$facet_grid
            ))
        },
        'heatmap' = {
            append(taglist, list(
                dorow(c('fill'), c(12)),
                elements$facet_wrap,
                elements$facet_grid
            ))
        },
        'density2d' = {
            append(taglist, list(
                elements$facet_wrap,
                elements$facet_grid
            ))
        },
        'wordcloud' = {
            append(taglist, list(
                h3('No options yet supported')
            ))
        },
        'seq' = {
            append(taglist, list(
                h3('No options yet supported')
            ))
        },
        'numseq' = {
            append(taglist, list(
                h3('No options yet supported')
            ))
        },
        'network' = {
            append(taglist, list(
                h3('No options yet supported')
            ))
        },
        'catseq' = {
            append(taglist, list(
                h3('No options yet supported')
            ))
        },
        taglist
    )
    return(taglist)
}

plot.build.elements <- function(types){
    p <- list()
    cat.or.num <- getChoices(types, c('cat', 'num'))
    # aes
    p$color.by    <- selectInput('plot.color.by', 'Color by', choices=cat.or.num)
    p$fill.by     <- selectInput('plot.fill.by', 'Fill by', choices=cat.or.num)
    p$size.by     <- selectInput('plot.size.by', 'Size by', choices=cat.or.num)
    p$shape.by    <- selectInput('plot.shape.by', 'Shape by', choices=cat.or.num)
    p$group.by    <- selectInput('plot.group.by', 'Group by', choices=cat.or.num)
    p$linetype.by <- selectInput('plot.group.by', 'Linetype by', choices=cat.or.num)
    # constant visuals
    p$alpha     <- sliderInput('plot.alpha', 'Set alpha', min=0, max=1, value=1, step=0.05) 
    p$size      <- sliderInput('plot.size', 'Set size', min=0, max=5, value=1, step=0.01)
    p$notch     <- checkboxInput('plot.notch', 'Notch')
    p$width     <- sliderInput('plot.width', 'Set width', min=0, max=2, value=1, step=0.05)
    p$density   <- checkboxInput('plot.density', 'Density')
    # faceting
    p$facet_wrap <- fluidRow(
        column(4, selectInput('plot.facet_wrap', 'Facet wrap by', choices=cat.or.num)),
        column(4, sliderInput('plot.facet_wrap_ncol', 'Number of columns', min=0, max=10, value=0, step=1)),
        column(4, radioButtons('plot.facet_wrap_scale', 'Scale',
                               choices=c('fixed', 'free_x', 'free_y', 'free')))
    )
    p$facet_grid <- fluidRow(
        column(4, selectInput('plot.facet_grid_x', 'Facet X-axis', choices=cat.or.num)),
        column(4, selectInput('plot.facet_grid_y', 'Facet Y-axis', choices=cat.or.num)),
        column(2, radioButtons('plot.facet_grid_scale', 'Scale',
                               choices=c('fixed', 'free_x', 'free_y', 'free'))),
        column(2, checkboxInput('plot.facet_grid_margins', 'Margins'))
    )
    return(p)
}
PlotUI <- setRefClass('PlotUI',
    fields=c("types", "xel", "yel", "geomel", "x", "y", "geom"),
    methods=list(
        init = function(types){
            if(! 'None' %in% names(types)) { types['None'] <- '-' }
            types    <<- types
            xel      <<- plot.select.x(types, selected='None')
            yel      <<- plot.select.y('-', '-')
            geomel   <<- plot.select.geom('-', '-')
            x        <<- 'None'
            y        <<- 'None'
            geom     <<- NA
            elements <<- plot.build.elements(types)
        },
        setX = function(xname){
            cat('\tentering PlotUI::setX\n')
            types[xname] %ifnot% return()
            x <<- xname
            xel <<- plot.select.x(types, selected=xname)
            setY()
            setGeom()
        },
        setY = function(yname=y){
            cat('\tentering PlotUI::setY\n')
            types[yname] %ifnot% return()
            choices <- getYChoices(types, x)
            if(!yname %in% choices){
                yname <<- 'None'
            }
            yel <<- selectInput('plot.y', 'Dependent variable', choices=choices, selected=yname)
            y   <<- yname
            setGeom()
        },
        setGeom = function(g=geom){
            cat('\tentering PlotUI::setGeom\n')
            choices <- getGeomChoices(types[x], types[y])
            if(is.null(g) || is.na(g) || !g %in% choices){
                g <- choices[1]
            }
            geomel <<- selectInput('plot.geom', 'Plot type', choices=choices, selected=g)
            geom <<- g
        },
        buildUI = function(){
            taglist <- list()
            taglist[[1]] <- 12
            taglist[[2]] <- fluidRow(column(4, xel),
                                     column(4, yel),
                                     column(4, geomel))
            taglist <- plot.build.ggplot.ui(taglist, geom, elements)
            do.call(column, taglist)
        }
    )
)
