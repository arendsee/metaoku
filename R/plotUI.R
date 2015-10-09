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
    geomap['cat', 'num'] <- 'boxplot,network,path'
    geomap['cat', 'seq'] <- 'catseq'

    # NOTES: I plan on killing longcat, but for now treat it like other cats
        geomap['longcat', '-']   <- 'barplot'
        geomap['longcat', 'cat'] <- 'heatmap,network'
        geomap['longcat', 'cor'] <- NA
        geomap['longcat', 'num'] <- 'boxplot,network,path'
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
    geomap['num', 'num'] <- 'point,density2d'
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

getYChoices <- function(types, xtype){
    geomap <- getGeoMap()
    choices <- apply(!is.na(geomap), 1, function(x) colnames(geomap)[x]) %>%
               lapply(function(x) if(length(x) == 0) '-' else x)
    choices <- c('None', names(types[types %in% choices[[xtype]]]))
    return(choices)
}

getGeomChoices <- function(xtype, ytype='-'){
    cat(sprintf('\tplotUI::getGeomChoices xtype:(%s), ytype:(%s)\n', xtype, ytype))
    return(unlist(strsplit(getGeoMap()[xtype, ytype], ',')))
}

plot.select.x <- function(types, selected='None') {
    selectInput('plot.x', 'Independent variable', choices=names(types), selected=selected)
}

plot.select.y <- function(types, xtype, selected='None') {
    cat('\tentering PlotUI::plot.select.y\n')
    choices <- getYChoices(types, xtype)
    if(!selected %in% choices){
        selected = 'None'
    }
    selectInput('plot.y', 'Dependent variable', choices=choices, selected=selected)
}

plot.select.geom <- function(xtype, ytype, selected=NULL){
    choices <- getGeomChoices(xtype, ytype)
    if(is.null(selected) || !selected %in% choices){
        selected <- choices[1]
    }
    selectInput('plot.geom', 'Plot type', choices=choices, selected=selected)
}

plot.facet_wrap <- function(types) {
    choices <- getChoices(types, c('cat', 'num', 'longcat'))
    selectInput('plot.facet_wrap', 'Facet_wrap by', choices='None')
}

plot.geomUI <- function(geomtype){
    h2('Not Implemented')
}

PlotUI <- setRefClass('PlotUI',
    fields=c("xel", "yel", "geom", "geomUI", "xtype", "ytype", "yval", "geomval", "types"),
    methods=list(
        init = function(types){
            types   <<- types
            xel     <<- plot.select.x(types, selected='None')
            yel     <<- plot.select.y('-', '-')
            geom    <<- plot.select.geom('-', '-')
            geomUI  <<- h2('Select data and plot type')
            xtype   <<- '-'
            ytype   <<- '-'
            yval    <<- 'None'
            geomval <<- 'None'
        },
        setX = function(xtype, xname){
            cat('\tentering PlotUI::setX\n')
            xtype %ifnot% return()
            xtype <<- xtype
            xel   <<- plot.select.x(types, selected=xname)
            yel   <<- plot.select.y(types, xtype, selected=yval)
            geom  <<- plot.select.geom(xtype, ytype, selected=geomval)
        },
        setY = function(ytype, yname){
            cat('\tentering PlotUI::setY\n')
            ytype %ifnot% return()
            ytype <<- ytype
            yval  <<- yname
            yel   <<- plot.select.y(types, xtype, selected=yval)
            geom  <<- plot.select.geom(xtype, ytype, selected=geomval)
        },
        setGeom = function(geomtype){
            cat('\tentering PlotUI::setGeom\n')
            geomtype %ifnot% return()
            geomval <<- geomtype
            geom    <<- plot.select.geom(xtype, ytype, selected=geomval)
            geomUI  <<- plot.geomUI(geomtype)
        },
        buildUI = function(){
            column(12,
                fluidRow(
                    column(4, xel),
                    column(4, yel),
                    column(4, geom)
                ),
                geomUI
            )
        }
    )
)
