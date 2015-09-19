require(data.table)
require(Matrix)
require(tm)

# All datasets are
# 1. TAB-delimited
# 2. Have headers
# 3. All tables must have the same first column (the key column)

# --- Input constants
DATA_DIR = 'data'              # folder holding all data
DATA_PAT = '.tab$'             # regular expression identifying data files
METADATA = 'data/METADATA'     # metadata filename
DAT_NAME = '.global-data.Rdat' # filename for saved R data

# --- Constants for determining whether to consider an integer a factor
#   let x be a vector of integers of total length N
#   let u equal the number of unique values in the vector
#   we consider x to be categorical if the following is true:
#      u < MAX_LEVELS and u/N < MAX_PROP
MAX_LEVELS = 20
MAX_PROP   = 0.1

# --- Constants for classifying character vector as cat, longcat, cor, or seq
# if the longest element is greater than MAX_LENGTH, classify as cor or seq
MAX_LENGTH = 50



merge.files <- function(){
    global.key <- NULL
    global.d <- NULL
    for (f in list.files(path=DATA_DIR, pattern=DATA_PAT, full.names=TRUE)){
        d <- as.data.table(read.delim(f, quote="", stringsAsFactors=FALSE))
        key = colnames(d)[1]
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
    return(global.d)
}



process.metadata <- function(columns){
    if(file.exists(METADATA)){
        md <- read.delim(METADATA, stringsAsFactors=FALSE)
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



determine.type <- function(d){
    # Deterine the internal type. This is essential for dispatching to the
    # correct plotting functions.
    # Datatypes:
    #    num     - numeric
    #    cat     - categorical
    #    longcat - categorical with many elements (e.g. too many for x-axis in boxplot)
    #    cor     - textual data
    #    seq     - sequence data, e.g. protein or nucleotide sequence
    types <- rep(NA, ncol(d))
    names(types) <- names(d)
    for (cname in names(types)){
        x = d[[cname]]
        u <- length(unique(x))
        N <- length(x)

        # if the input isn't numeric or character, something weird is going on
        stopifnot(is.numeric(x) || is.character(x))

        if(is.numeric(x)){
            # determine whether to treat a numeric vector as num or cat
            if(all(x %% 1 == 0, na.rm=TRUE) && u <= MAX_LEVELS && u / N < MAX_PROP){
                typ <- 'cat'
            } else {
                typ <- 'num'
            }
        } else if(is.character(x)) {
            # determine whether a character vector is cat, longcat, cor, or seq
            if(max(nchar(x)) > MAX_LENGTH){
                # if there are any spaces, treat the string as textual
                # otherwise, consider it a sequence
                if(any(grepl(' ', x))){
                    typ <- 'cor'
                } else {
                    typ <- 'seq'
                }
            } else {
                # if there are lots of levels, it is a longcat 
                if(u > MAX_LEVELS){
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
    corpa <- list()
    for(cname in names(global$type[global$type == 'cor'])){
        txt <- global$table[[cname]]
        corpus <- Corpus(VectorSource(txt))
        corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to='ASCII', sub='byte')))
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes=TRUE)
        corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
        dtm <- DocumentTermMatrix(corpus)
        m <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, dims=c(dtm$nrow, dtm$ncol), dimnames=dtm$dimnames)
        corpa[[cname]] = m
    }
    return(corpa)
}



set.types <- function(d, types){
    # cat               -> factor
    # longcat, cor, seq -> character vectors
    # num               -> numeric vector
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



build.global <- function(){
    global <- list(
        table    = NULL,   # a data.frame holding all data
        corpa    = list(), # word usage matrices for building word clouds
        key      = NULL,   # the name of the key column
        metadata = NULL,   # metadata for each columns
        type     = NULL
    )

    global$table    <- merge.files()
    global$metadata <- process.metadata(columns=names(global$table))
    global$type     <- determine.type(global$table)
    global$corpa    <- build.corpa(global)
    global$table    <- set.types(global$table, global$type)

    return(global)
}



if(file.exists(DAT_NAME)) {
    load(DAT_NAME)
} else {
    global <- build.global()
    save(global, file=DAT_NAME)
}
