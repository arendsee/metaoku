DataSet <- setRefClass(
    Class = 'DataSet',
    fields = c(
        'children',
        'key',
        'max_levels',
        'max_prop',
        'max_length',
        'row_filter',
        'ncol',
        'nrow',
        'names'
    ),
    methods = list(
        build = function(dataset=NULL,
                         metadata=NULL,
                         max_levels=20,
                         max_prop=0.1,
                         max_length=50,
                         key=NULL){
            require(plyr)
            require(data.table)
            require(magrittr)
            max_levels <<- max_levels
            max_prop   <<- max_prop
            max_length <<- max_length
            key        <<- key
            ncol       <<- ncol(dataset)
            nrow       <<- nrow(dataset)
            row_filter <<- NULL
            names      <<- names(dataset)

            children <<- list()
            for (cname in names(dataset)){
                x = dataset[[cname]]
                u <- length(unique(x))
                N <- length(x)
                if(is.numeric(x)){
                    # determine whether to treat a numeric vector as num or cat
                    is_integer <- all(x %% 1 == 0, na.rm=TRUE)
                    if(is_integer && u <= max_levels && u / N < max_prop){
                        child <- DataCat$new()
                    } else {
                        child <- DataNum$new()
                    }
                } else {
                    x <- as.character(x)
                    # determine whether a character vector is cat, longcat,
                    # cor, or seq
                    if(max(nchar(x), rm.na=TRUE) > max_length){
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
                child$init(value=x, name=cname, key=key, metadata=NULL,
                           max_levels=max_levels, max_length=max_length)
                children[[cname]] <<- child
            }

            # Initialize metadata if none is given
            if(is.null(metadata)){
                metadata <- data.frame(
                    column_names = sapply(children, function(x) x$name),
                    type = sapply(children, function(x) x$type),
                    stringsAsFactors=FALSE
                )
            }
            # initialize each child
            for(child in children){
                if(!child$name %in% metadata[[1]]){
                    metadata[nrow(metadata)+1, 1] <- child$name
                }
                met <- metadata[metadata[[1]] == child$name, ]
                if(nrow(met) > 1) {
                    cat('WARNING: multiple rows describing one column METADATA, choosing first\n')
                }
                child$metadata <- met
            }
        },

        getNameByType = function(type){
            names(getDataByType(type))
        },

        getDataByType = function(type){
            children[unlist(sapply(children, function(x) x$type %in% type))]
        },

        get = function(name){
            if(name %in% names){
                cat(' DF: get() - returning ', name, '\n')
                children[[name]]
            } else {
                cat(' DF: get() - returning Empty()\n')
                Empty()
            }
        },

        setFilter = function(filt){
            cat(' dataClass:setFilter()\n')
            row_filter <<- filt
            if(length(row_filter) == 0 || is.null(row_filter)){
                clearFilter()
            } else {
                for (child in children) { child$filter(row_filter) }
            }
        },

        clearFilter = function(){
            cat(' dataClass::clearFilter()\n')
            row_filter <<- NULL
            for (child in children) { child$refresh() }
        },

        getDF = function(filterRows=FALSE, cols=names){
            if(length(children) == 0) { return(NULL) }
            d <- do.call(cbind.data.frame,
                    append(lapply(children, function(child) child$.value),
                           list(stringsAsFactors = FALSE)))
            d <- d[, names %in% cols]
            if(filterRows && !is.null(row_filter)){
                d <- d[row_filter, ]
            }
            return(d)
        },

        getMetadataDF = function(){
            if(length(children) == 0) { return(NULL) }
            do.call(rbind.data.frame, lapply(children, function(child) child$metadata))
        },
        refresh = function(){
            for (child in children) { child$refresh() }
        }
    )
)

# Fields
# value   - data
# name    - column name
# type    - data type [cat, num, cor, seq, longcat], determines how the data is
#           treated, the user may change it to change the behaior of the data.
# .type   - the permanent type
Data <- setRefClass(
    Class    = 'Data',
    fields   = c('value', '.value', 'name', 'type', '.type', 'key', 'metadata'),
    methods  = list(
        init = function(value=NULL, name='None', key=NULL, metadata=NULL, ...){
            require(plyr)
            require(magrittr)
            name     <<- name
            value    <<- value
            type     <<- '-'
            .value   <<- value
            .type    <<- type
            key      <<- key
            metadata <<- metadata
            type_specific_init(...)
        },
        type_specific_init = function(...){ },
        getData = function(filt=FALSE){
            if(filt){
                value[filt]
            } else {
                value 
            }
        },
        asCat = function(){ as.factor(value)  },
        asNum = function(){ as.numeric(value) },
        setStat = function() {cat('\t - WARNING: using unimplemented setStat\n')},
        filter = function(row_filter){
            #cat(sprintf('\t - filtering %s (%s)\n', name, type))
            value <<- .value[row_filter]
            setStat()
        },
        refresh = function(){
            value <<- .value
            type <<- .type
            setStat()
        }
    )
)

Empty <- function(){
    d <- Data$new()
    d$init()  
    return(d)
}

DataNum <- setRefClass(
    Class    = 'DataNum',
    fields   = c('max', 'min'),
    contains = 'Data',
    methods  = list(
        type_specific_init = function(...){
            type <<- .type <<- 'num'
            setStat()
        },
        setStat = function(){
            if(length(value) > 0){
                max  <<- max(value, na.rm=TRUE) 
                min  <<- min(value, na.rm=TRUE)
            } else {
                max <<- NA
                min <<- NA
            }
        },
        asCat = function(breaks=5, dig.lab=1, ordered_result=TRUE){ 
            if(type == 'num'){
                type  <<- 'cat'
                value <<- cut(value,
                              breaks=breaks,
                              dig.lab=dig.lab,
                              ordered_result=ordered_result)
            }
        }
    )
)

DataCat <- setRefClass(
    Class    = 'DataCat',
    fields   = c('nchar', 'max.length', 'min.length', 'counts', 'max.count', 'min.count'),
    contains = 'Data',
    methods  = list(
        type_specific_init = function(...){
            type  <<- .type <<- 'cat'
            value <<- .value <<- factor(.value)
            setStat()
        },
        setStat = function(){
            nchar      <<- nchar(as.character(value)) 
            if(length(value) > 0){
                max.length <<- max(nchar, na.rm=TRUE)
                min.length <<- min(nchar, na.rm=TRUE)
            } else {
                max.length <<- NA
                min.length <<- NA
            }
            counts     <<- count(as.character(value)) %>% arrange(-freq)
            max.count  <<- counts$freq[1]
            min.count  <<- counts$freq[nrow(counts)]
        },
        asNum = function(){ }
    )
)

DataLongcat <- setRefClass(
    Class = 'DataLongcat',
    contains = 'DataCat',
    fields = c('max_length', 'max_levels'),
    methods = list(
        type_specific_init = function(max_length, max_levels, ...){
            type <<- .type <<- 'longcat'
            value <<- .value <<- as.character(value)
            max_length <<- max_length
            max_levels <<- max_levels
        },
        asCat = function(ml=max_levels){
            if(type == 'longcat'){
                type <<- 'cat'
                if(length(unique(value)) <= ml){
                    value <<- factor(value)
                } else {
                    trunc.names <- head(counts, ml)$x
                    value[! value %in% trunc.names] <<- 'other'
                    value <<- factor(value)
                }
            }
        },
        filter = function(row_filter){
            # cat(sprintf('\t - filtering %s (%s)\n', name, type))
            value <<- .value[row_filter]
            setStat()
            if(type == 'cat'){
                type <<- 'longcat'
                asCat()
            }
        }
    )
)

# =====================================================================
# For each column of sequences, build a data.table the columns:
#   1. key    - the unique key associated with the row
#   2. char   - a letter from the sequence (e.g. {'A', 'C', 'G', 'T'})
#   3. count  - the number of times *char* appears in the sequence *key*
#   4. total  - the total number of *char* in *key*, i.e. sequence length
#   5. prop   - the proportion of *char* in *key*
# Return: A list containing one data.table for each sequence column
# =====================================================================
DataSeq <- setRefClass(
    Class = 'DataSeq',
    fields = c('char.frequency', 'seq.lengths', 'alphabet', 'char.table', '.char.table'),
    contains = 'Data',
    methods = list(
        type_specific_init = function(...){
            type   <<- .type <<- 'seq'
            .value <<- getPretty(w=60)
            value  <<- .value
            parseSeq() # set alphabet, char.table, and char.frequency
            .char.table <<- char.table
            setStat()
        },
        parseSeq = function(){
            require(reshape2)
            require(data.table)
            hasseq   <- !is.na(value)
            carray   <- value[hasseq] %>% toupper %>% strsplit('')
            alphabet <<- unique(unlist(carray))
            counts   <- lapply(carray, table)
            d <- matrix(rep(0, sum(hasseq) * length(alphabet)),
                        ncol=length(alphabet),
                        dim=list(key[hasseq], alphabet))
            for(i in 1:nrow(d)){
                d[i, names(counts[[i]])] <- counts[[i]]
            }
            d <- reshape2::melt(d)
            colnames(d) <- c('key', 'char', 'count')
            d <- data.table(d)
            setkey(d, key)
            d[, total := sum(count), by=key]
            d[, prop  := count / total]
            char.table <<- d
        },
        setStat = function(){
            char.frequency <<- count(char.table$char)
            seq.lengths    <<- unique(char.table[, c('key', 'total'), with=FALSE])
        },
        filter = function(row_filter){
            # NOTE: on filtering do NOT copy sequence info, only the stats
            cat(sprintf('\t - filtering %s (%s)\n', name, type))
            char.table <<- .char.table[row_filter, ]
            setStat()
        },
        refresh = function(){
            char.table <<- .char.table
            type <<- .type
            setStat()
        },
        getPretty = function(w=60){
            stopifnot(is.numeric(w) && w %% 1 == 0)
            gsub('\\s', '', value, perl=TRUE) %>%
                strsplit('') %>%
                lapply(function(s) {
                    sapply(seq(1, by=w, length.out=(length(s) %/% w + 1)),
                           function(i) paste0(s[i:min((i+w), length(s))], collapse=''))
                }) %>%
                lapply(paste, collapse='\n') %>%
                unlist
        },
        asCat = function(){
            warning("Cannot convert 'seq' type to 'cat'")
        },
        asNum = function(){
            warning("Cannot convert 'seq' type to 'num'")
        }
    )
)

DataCor <- setRefClass(
    Class = 'DataCor',
    fields = c('mat', '.mat'),
    contains = 'Data',
    methods = list(
        type_specific_init = function(...){
            type <<- .type <<- 'cor' 
            calculateMatrix()
            .mat <<- mat
        },
        calculateMatrix = function(){
            require(Matrix)
            require(tm)
            corpus <- value                                                                       %>%
                tm::VectorSource()                                                                %>%
                tm::Corpus()                                                                      %>%
                tm::tm_map(tm::content_transformer(function(x) iconv(x, to='ASCII', sub='byte'))) %>%
                tm::tm_map(tm::content_transformer(tolower))                                      %>%
                tm::tm_map(tm::removePunctuation, preserve_intra_word_dashes=TRUE)                %>%
                tm::tm_map(function(x) tm::removeWords(x, stopwords("english")))
            dtm <- tm::DocumentTermMatrix(corpus)
            mat <<- Matrix::sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,
                                         dims=c(dtm$nrow, dtm$ncol), dimnames=dtm$dimnames)
        },
        filter = function(row_filter){
            # NOTE: on filtering do NOT copy text
            cat(sprintf('\t - filtering %s (%s)\n', name, type))
            mat <<- .mat[row_filter, ]
        },
        refresh = function(){
            mat <<- .mat
        },
        asCat = function(){
            warning("Cannot convert 'cor' type to 'cat'")
        },
        asNum = function(){
            warning("Cannot convert 'cor' type to 'num'")
        }
    )
)
