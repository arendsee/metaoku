require(data.table)
require(reshape2)
require(magrittr)

# ==============================================================================
# If there are multiple datafiles in one data folder, try to merge them by the
# first column. Die on failure.
# ==============================================================================
merge.files <- function(data.dir, data.pat){
    cat('\t  * merge.files\n')
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
        cat('WARNING: no data found\n')
    }
    return(global.d)
}



# ====================================================================================
# Load the METADATA file
#   - This file should contains info on each column in a dataset
#   - If the METADATA file is missing, build it based on column names.
# ====================================================================================
process.metadata <- function(columns, metadata){
    cat('\t  * process.metadata\n')
    if(file.exists(metadata)){
        md <- read.delim(metadata, stringsAsFactors=FALSE)
        stopifnot(names(md)[1] == 'column_name') 
    } else {
        md <- NULL
    }
    for (cname in columns){
        # if no metadata file exists, create one
        if(is.null(md)){
            md <- data.frame(column_name = cname, stringsAsFactors=FALSE)
        }
        if(! cname %in% md$column_name){
            md[nrow(md) + 1, 1] <- cname
        }
    }
    return(md)
}

# ====================================================================================
# Deterine the internal type. This is essential for dispatching to the
# correct plotting functions.
# Datatypes:
#    num     - numeric
#    cat     - categorical
#    longcat - categorical with many elements (e.g. too many for x-axis in boxplot)
#    cor     - textual data
#    seq     - sequence data, e.g. protein or nucleotide sequence
# ==============================================================================
determine.type <- function(d, max.levels, max.prop, max.length){
    cat('\t  * determine.type\n')
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



# ====================================================================================
# Build a word usage matrix for a textual column
# ====================================================================================
build.corpa <- function(global){
    cat('\t  * build.corpa\n')
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



# ====================================================================================
# For each column of sequences, build a data.table with the following columns:
#   1. key    - the unique key associated with the row
#   2. char   - a letter from the sequence (e.g. for DNA {'A', 'C', 'G', 'T'})
#   3. count  - the number of times *char* appears in the sequence *key*
#   4. total  - the total number of *char* in *key*, i.e. the sequence length
#   5. prop   - the proportion of *char* in *key*
# Return: A list containing one data.table for each sequence column
# ====================================================================================
build.seqs <- function(global){
    cat('\t  * build.seqs\n')
    seqs <- list()
    for(cname in names(global$type[global$type == 'seq'])){
        s1     <- global$table[[cname]]
        hasseq <- !is.na(s1)
        s2     <- strsplit(toupper(s1[hasseq]), '')
        let    <- unique(unlist(s2))
        s3     <- lapply(s2, table)
        d <- matrix(rep(0, sum(hasseq) * length(let)),
                    ncol=length(let),
                    dim=list(global$table$KEY[hasseq], let))
        for(i in 1:nrow(d)){
            d[i, names(s3[[i]])] <- s3[[i]]
        }
        d <- reshape2::melt(d)
        colnames(d) <- c('key', 'char', 'count')
        d <- data.table(d)
        setkey(d, key)
        d[, total := sum(count), by=key]
        d[, prop  := count / total]
        seqs[[cname]] <- d
    }
    return(seqs)
}

# ====================================================================================
# Adds line breaks to sequences
# ====================================================================================
pretty.seqs <- function(global){
    cat('\t  * build.pretty.seqs()\n')
    for(cname in names(global$type[global$type == 'seq'])){
        global$table[[cname]] <- gsub('\\s', '', global$table[[cname]], perl=TRUE) %>%
            strsplit('') %>%
            lapply(function(s) {
                sapply(seq(10, by=10, length.out=(length(s) %/% 10)),
                       function(i) paste0(s[i:min((i+10), length(s))], collapse=''))
            }) %>%
            lapply(paste, collapse=' ') %>%
            unlist
    }
    return(global)
}



# ==============================================================================
# Convert columns to the appopriate type
# Specifically:
#  * cat               -> factor
#  * longcat, cor, seq -> character vectors
#  * num               -> numeric vector
# ==============================================================================
set.types <- function(d, types){
    cat('\t  * set.types\n')
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



# ==============================================================================
# Build a dataset and required metadata
# Imports the following variables from the config file
#  * DATA_PAT
#  * DATA_DIR
#  * SAVE_DIR
#  * MAX_PROP
#  * MAX_LEVELS
#  * MAX_LENGTH
# see config for details
# ==============================================================================
build.one.dataset <- function(dataname, config){
    cat('\t - build.one.dataset() ', dataname, '\n')
    data.dir <- file.path(config$data_dir, dataname)
    rdat <- file.path(config$save_dir, paste0(dataname, '.Rdat'))
    # If a data file already exists, return it
    if(file.exists(rdat)) {
        return(rdat)
    }
    global <- list(
        table    = NULL,   # a data.frame holding all data
        corpa    = list(), # word usage matrices for building word clouds
        key      = NULL,   # the name of the key column
        metadata = NULL,   # metadata for each columns
        type     = NULL    # the type of data contained in each column
    )
    global$table <- merge.files(
                         data.pat=config$data_pat,
                         data.dir=data.dir)
    global$table$KEY <- 1:nrow(global$table)
    global$metadata <- process.metadata(
                            columns=names(global$table),
                            metadata=file.path(data.dir, config$metadata))
    global$type     <- determine.type(
                            global$table,
                            max.prop=config$max_prop,
                            max.levels=config$max_levels,
                            max.length=config$max_length)
    global$corpa    <- build.corpa(global)
    global$table    <- set.types(global$table, global$type)
    global$seqs     <- build.seqs(global)
    global          <- pretty.seqs(global)

    if(!dir.exists(config$save_dir)){
        dir.create(config$save_dir)
    }
    save(global, file=rdat)
    return(rdat)
}



# ==============================================================================
# Returns of filenames of saved datasets
# * The names of the datasets are from the names of folders in the data
#   directory
# ==============================================================================
build.all.datasets <- function(config){
    cat('load.R::build.all.datasets\n')
    datadirs <- basename(list.dirs(path=config$data_dir, recursive=FALSE))
    sapply(datadirs, build.one.dataset, config)
}
