require(data.table)
require(reshape2)

merge.files <- function(data.dir, data.pat){
    cat('entering merge.files\n')
    global.key <- NULL
    global.d   <- NULL
    for (f in list.files(path=data.dir, pattern=data.pat, full.names=TRUE)){
        d <- as.data.table(read.delim(f, quote="", stringsAsFactors=FALSE))
        key = names(d)[1]
        setkeyv(d, key)

        # All input tables must start with the same key
        if(is.null(global.key)){
            global.key <- key
        } else {
            stopifnot(global.key == key)
        }

        if(is.null(global.d)){
            global.d <- d
        } else {
            global.d <- merge(global.d, d, by=key, all=TRUE)
        }
    }
    if(length(global.d) == 0){
        cat('WARNING: no data found')
    }
    return(global.d)
}



process.metadata <- function(columns, metadata){
    cat('entering process.metadata\n')
    if(file.exists(metadata)){
        md <- read.delim(metadata, stringsAsFactors=FALSE)
    } else {
        md <- NULL
    }

    for (cname in columns){
        # if no metadata fiel is found, 
        if(is.null(md)){
            md <- data.frame(column_name = cname, stringsAsFactors=FALSE)
        }

        stopifnot(names(md)[1] == 'column_name') 

        if(! cname %in% md$column_name){
            md[nrow(md) + 1, 1] <- cname
        }
    }

    return(md)
}

# ========================================================================
# Deterine the internal type. This is essential for dispatching to the
# correct plotting functions.
# Datatypes:
#    num     - numeric
#    cat     - categorical
#    longcat - categorical with many elements (e.g. too many for x-axis in boxplot)
#    cor     - textual data
#    seq     - sequence data, e.g. protein or nucleotide sequence
# ========================================================================
determine.type <- function(d, max.levels, max.prop, max.length){
    cat('entering determine.type\n')
    types <- rep(NA, ncol(d))
    names(types) <- names(d)
    for (cname in names(types)){
        x = d[[cname]]
        u <- length(unique(x))
        N <- length(x)

        if(is.numeric(x)){
            # determine whether to treat a numeric vector as num or cat
            if(all(x %% 1 == 0, na.rm=TRUE) && u <= max.levels && u / N < max.prop){
                typ <- 'cat'
            } else {
                typ <- 'num'
            }
        } else if(is.character(x)) {
            # determine whether a character vector is cat, longcat, cor, or seq
            if(max(nchar(x)) > max.length){
                # if there are any spaces, treat the string as textual
                # otherwise, consider it a sequence
                if(any(grepl(' ', x))){
                    typ <- 'cor'
                } else {
                    typ <- 'seq'
                }
            } else {
                # if there are lots of levels, it is a longcat 
                if(u > max.levels){
                    typ <- 'longcat'
                } else {
                    typ <- 'cat'
                }
            }
        }
        types[cname] <- typ
    }
    types['Selection'] <- 'cat'
    return(types)
}



build.corpa <- function(global){
    cat('entering build.corpa\n')
    corpa <- list()
    for(cname in names(global$type[global$type == 'cor'])){
        # only load these if necessary (Matrix, especially, is big)
        require(Matrix)
        require(tm)
        txt <- global$table[[cname]]
        corpus <- tm::Corpus(tm::VectorSource(txt))
        corpus <- tm::tm_map(corpus, tm::content_transformer(function(x) iconv(x, to='ASCII', sub='byte')))
        corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
        corpus <- tm::tm_map(corpus, tm::removePunctuation, preserve_intra_word_dashes=TRUE)
        corpus <- tm::tm_map(corpus, function(x) tm::removeWords(x, stopwords("english")))
        dtm <- tm::DocumentTermMatrix(corpus)
        m <- Matrix::sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, dims=c(dtm$nrow, dtm$ncol), dimnames=dtm$dimnames)
        corpa[[cname]] = m
    }
    return(corpa)
}



build.seqs <- function(global){
    cat('entering build.seqs\n')
    seqs <- list()
    for(cname in names(global$type[global$type == 'seq'])){
        s1     <- global$table[[cname]]
        hasseq <- !is.na(s1)
        s2     <- strsplit(toupper(s1[hasseq]), '')
        let    <- unique(unlist(s2))
        s3     <- lapply(s2, table)
        d <- matrix(rep(0, sum(hasseq) * length(let)),
                    ncol=length(let),
                    dim=list(global$table[[global$key]][hasseq], let))
        for(i in 1:nrow(d)){
            d[i, names(s3[[i]])] <- s3[[i]]
        }

        d <- reshape2::melt(d)
        colnames(d) <- c('model', 'aa', 'count')
        seqs[[cname]] <- d
    }
    return(seqs)
}



# ========================================================================
# Convert columns to the appopriate type
# Specifically:
#  * cat               -> factor
#  * longcat, cor, seq -> character vectors
#  * num               -> numeric vector
# ========================================================================
set.types <- function(d, types){
    cat('entering set.types\n')
    for(cname in names(d)){
        if(types[cname] == 'cat'){
            d[[cname]] <- factor(d[[cname]])
        } else {
            d[[cname]] <- as.character(d[[cname]])
            if(types[cname] == 'num'){
                d[[cname]] <- as.numeric(d[[cname]])
            }
        }
    }
    return(d)
}


# ========================================================================
# Build the main dataset and required metadata
# Imports the following variables from the config file
#  * DATA_PAT
#  * DATA_DIR
#  * METADATA
#  * MAX_PROP
#  * MAX_LEVELS
#  * MAX_LENGTH
# see config for details
# ========================================================================
build.global <- function(){
    source('config')

    # If a data file already exists, load and return it
    if(file.exists(DAT_NAME)) {
        load(DAT_NAME)
        if(exists('global')){
            return(global)
        }
    }

    global <- list(
        table    = NULL,   # a data.frame holding all data
        corpa    = list(), # word usage matrices for building word clouds
        key      = NULL,   # the name of the key column
        metadata = NULL,   # metadata for each columns
        type     = NULL    # the type of data contained in each column
    )
    global$table    <- merge.files(
                            data.pat=DATA_PAT,
                            data.dir=DATA_DIR)
    global$key      <- names(global$table)[1]
    global$metadata <- process.metadata(
                            columns=names(global$table),
                            metadata=METADATA)
    global$type     <- determine.type(
                            global$table,
                            max.prop=MAX_PROP,
                            max.levels=MAX_LEVELS,
                            max.length=MAX_LENGTH)
    global$corpa    <- build.corpa(global)
    global$table    <- set.types(global$table, global$type)
    global$seqs     <- build.seqs(global)
    save(global, file=DAT_NAME)
    return(global)
}
