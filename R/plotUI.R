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

getChoices <- function(dataset, accepted=types){
    c('None', dataset$getNameByType(accepted))
}

getYChoices <- function(dataset, x){
    geomap <- getGeoMap()
    choices <- apply(!is.na(geomap), 1, function(val) colnames(geomap)[val]) %>%
               lapply(function(val) if(length(val) == 0) '-' else val)
    choices <- dataset$getNameByType(choices[[x$type]])
    return(choices)
}

getGeomChoices <- function(x, y=NULL){
    cat(sprintf('\tplotUI::getGeomChoices x:(%s), y:(%s)\n', class(x), class(y)))
    return(unlist(strsplit(getGeoMap()[x$type, y$type], ',')))
}

plot.select.x <- function(datasets, selected=NULL) {
    selectInput('plot.x', 'Independent variable', choices=names(datasets$getDF()), selected=selected$name)
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
                dorow(c('color.by', 'fill.by', 'biny'), c(4,4,2)),
                elements$facet
            ))
        },
        'boxplot' = {
            append(taglist, list(
                dorow(c('size.by', 'alpha.by'), c(6, 6)),
                dorow(c('alpha', 'width', 'size', 'notch'), c(3,3,3,3)),
                elements$facet
            ))
        },
        'histogram' = {
            append(taglist, list(
                dorow(c('color.by', 'fill.by', 'biny'), c(5,5,2)),
                elements$facet
            ))
        },
        'point' = {
            append(taglist, list(
                dorow(c('color.by', 'shape.by', 'size.by'), c(3,3,3)),
                dorow(c('alpha', 'size'), c(6,6)),
                elements$facet
            ))
        },
        'path' = {
            append(taglist, list(
                dorow(c('group.by', 'color.by', 'size.by', 'linetype.by'), c(3,3,3,3)),
                dorow(c('alpha', 'size'), c(6,6)),
                elements$facet
            ))
        },
        'heatmap' = {
            append(taglist, list(
                dorow(c('fill'), c(12)),
                elements$facet_1d
            ))
        },
        'density2d' = {
            append(taglist, list(
                elements$facet_1d
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
    cat('\t - leaving plot.build.ggplot.ui()\n') 
    return(taglist)
}

plot.build.elements <- function(dataset){
    p <- list()
    cat.or.num <- getChoices(dataset, c('cat', 'num'))
    # aes
    p$color.by     <- selectInput('plot.aes.color', 'Color by', choices=cat.or.num)
    p$fill.by      <- selectInput('plot.aes.fill', 'Fill by', choices=cat.or.num)
    p$size.by      <- selectInput('plot.aes.size', 'Size by', choices=cat.or.num)
    p$shape.by     <- selectInput('plot.aes.shape', 'Shape by', choices=cat.or.num)
    p$group.by     <- selectInput('plot.aes.group', 'Group by', choices=cat.or.num)
    p$linetype.by  <- selectInput('plot.aes.linetype', 'Linetype by', choices=cat.or.num)
    p$alpha.by     <- selectInput('plot.aes.alpha', 'Alpha by', choices=cat.or.num)
    p$biny         <- radioButtons('plot.biny', label=NULL,
                                  choices=list(count='..count..',
                                               density='..density..',
                                               proportion='..count../sum(..count..)'))
    p$binz        <- radioButtons('plot.binz', 'Z',
                                  choices=list(count='count',
                                               lograt='lograt',
                                               density='density'
                                 ))
    # constant visuals
    p$alpha     <- sliderInput('plot.alpha', 'Set alpha', min=0, max=1, value=1, step=0.05) 
    p$size      <- sliderInput('plot.size', 'Set size', min=0, max=5, value=1, step=0.01)
    p$notch     <- checkboxInput('plot.notch', 'Notch')
    p$width     <- sliderInput('plot.width', 'Set width', min=0, max=2, value=1, step=0.05)
    # faceting
    p$facet <- fluidRow(
        column(4, selectInput('plot.facet_x', 'Facet X-axis', choices=cat.or.num)),
        column(4, selectInput('plot.facet_y', 'Facet Y-axis', choices=cat.or.num)),
        column(2, radioButtons('plot.facet_scale', 'Scale',
                               choices=c('fixed', 'free_x', 'free_y', 'free'))),
        column(2, checkboxInput('plot.facet_margins', 'Margins'))
    )
    p$facet_1d <- fluidRow(
        column(4, selectInput('plot.facet_x', 'Facet X-axis', choices=cat.or.num)),
        column(8, radioButtons('plot.facet_scale', 'Scale',
                               choices=c('fixed', 'free_x', 'free_y', 'free')))
    )
    return(p)
}

PlotUI <- setRefClass('PlotUI',
    fields=c("dataset", "xel", "yel", "geomel", "x", "y", "geom", "empty"),
    methods=list(
        init = function(dataset, empty){
            cat(sprintf('PlotUI::init dim(dataset):(%s)\n', class(dataset)))
            dataset  <<- dataset 
            xel      <<- plot.select.x(dataset, selected=empty)
            yel      <<- selectInput('plot.y', 'Dependent variable', choices='None')
            geomel   <<- selectInput('plot.geom', 'Plot type', choices='None')
            x        <<- empty
            y        <<- empty
            geom     <<- empty
            elements <<- plot.build.elements(dataset)
        },
        setX = function(x){
            cat('\tentering PlotUI::setX\n')
            x <<- x
            xel <<- plot.select.x(dataset, selected=x)
            setY()
            setGeom()
        },
        setY = function(y=y){
            cat('\tentering PlotUI::setY\n')
            choices <- getYChoices(dataset, x)
            if(!y %in% choices){
                y <<- NA
            }
            yel <<- selectInput('plot.y', 'Dependent variable', choices=choices, selected=y)
            y   <<- y
            setGeom()
        },
        setGeom = function(g=geom){
            cat('\tentering PlotUI::setGeom\n')
            choices <- getGeomChoices(x, y)
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
