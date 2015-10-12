Dataset <- setRefClass(
    fields = list(
        'children'   = list,
        'row_filter' = logical,
        'row_order'  = integer,
        'key'        = character,
        'max_levels' = integer,
        'max_prop'   = numeric,
        'max_length' = integer
    ),
    methods = list(
        build = function(dataset, metadata=NULL, max_levels=20, max_prop=0.1, max_length=50, key=NULL){
            require(plyr)
            require(data.table)
            max_levels <<- max_levels
            max_prop   <<- max_prop
            max_length <<- max_length
            key        <<- key
            row_order  <<- 1:nrow(dataset)
            row_filter <<- rep(TRUE, nrow(dataset))

            children <- list()
            for (cname in names(dataset)){
                x = d[[cname]]
                u <- length(unique(x))
                N <- length(x)
                if(is.numeric(x)){
                    # determine whether to treat a numeric vector as num or cat
                    if(all(x %% 1 == 0, na.rm=TRUE) && u <= max_levels && u / N < max_prop){
                        child <- DataCat$new()
                    } else {
                        child <- DataNum$new()
                    }
                } else if(is.character(x)) {
                    # determine whether a character vector is cat, longcat, cor, or seq
                    if(max(nchar(x)) > max_length){
                        # if there are any spaces, treat the string as textual
                        # otherwise, consider it a sequence
                        if(any(grepl(' ', x))){
                            child <- DataCor$new()
                        } else {
                            child <- DataSeq$new()
                        }
                    } else {
                        # if there are lots of levels, it is a longcat 
                        if(u > max_levels){
                            child <- DataLongcat$new()
                        } else {
                            child <- DataCat$new()
                        }
                    }
                }
                children[[cname]] <- child
            }
        },
        getNameByType = function(type){

        },
        getDataByType = function(type){

        },
        get = function(name){

        },
        addFilter = function(filt){

        },
        clearFilter = function(){

        },
        getDF = function(cols=NULL, filt='as.col'){

        },
        mutate = function(func, name){

        },
        getMetadataDF = function(){

        }
    )
)

Data <- setRefClass(
    fields = c('parent', 'data', 'type', 'key', 'metadata'),
    methods = list(
        init = function(parent, data, type, key, metadata){
            require(plyr)
            parent   <<- parent
            data     <<- data
            type     <<- type
            key      <<- key
            metadata <<- metadat
            type_specific_init()
        },
        type_specific_init = function(){ },
        getData = function(filt=FALSE){ },
        asCat = function(){ },
        asNum = function(){ }
    )
)

DataNum <- setRefClass(
    fields = c('max', 'min'),
    contains = 'Data',
    methods = list(
        type_specific_init = function(){
            max <<- max(data) 
            min <<- min(data)
        },
        asCat = function(){ }
    )
)

DataCat <- setRefClass(
    fields = c('nchar', 'max.length', 'min.length', 'counts', 'max.count', 'min.count'),
    contains = 'Data',
    methods = list(
        type_specific_init = function(){
            nchar <<- nchar(data) 
            max.length <<- max(nchar)
            min.length <<- min(nchar)
            data <<- factor(data)
            counts <<- count(data)
            max.count <<- max(counts$freq)
            min.count <<- min(counts$freq)
        },
        asNum = function(){ }
    )
)

DataLongcat <- setRefClass(
    contains = 'DataCat',
    fields = c('truncated.counts')
    methods = list(
        type_specific_init = function(){

        },
        asCat = function(){ }
    )
)

DataSeq <- setRefClass(
    fields = c('char.frequency', 'seq.lengths', '2d.markov'),
    contains = 'Data',
    methods = list(
        type_specific_init = function(){ }
    )
)

DataCor <- setRefClass(
    fields = c('mat'),
    contains = 'Data',
    methods = list(
        type_specific_init = function(){ },
        calculateMatrix = function(){

        }
    )
)
