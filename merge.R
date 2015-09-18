require(data.table)
require(Matrix)
require(tm)

# All datasets are
# 1. TAB-delimited
# 2. Have headers
# 3. All tables must have the same first column (the key column)

rdat.filename = '.global-data.Rdat'

if(! file.exists(rdat.filename)){
    global          <- list()
    global$table    <- NULL
    global$corpa    <- list()
    global$key      <- NULL
    if(file.exists('data/METADATA')){
        global$metadata <- read.delim('data/METADATA', stringsAsFactors=FALSE)
    } else {
        global$metadata <- NULL
    }

    for(f in list.files('data/', '*.tab')){
        d <- as.data.table(read.delim(paste0('data/', f), quote="", stringsAsFactors=FALSE))
        key = colnames(d)[1]
        setkeyv(d, key)

        # All input tables must start with the same key
        if(is.null(global$key)){
            global$key <- key
        } else {
            stopifnot(global$key == key)
        }

        if(is.null(global$table)){
            global$table <- d
        } else {
            global$table <- merge(global$table, d, by=key, all=TRUE)
        }

        d <- as.data.frame(d)

        # build corpus for columns that look like text
        for (cname in colnames(d)){

            if(is.null(global$metadata)){
                global$metadata <- data.frame(column_name = cname, stringsAsFactors=FALSE)
            }
            stopifnot(colnames(global$metadata)[1] == 'column_name') 
            if(! cname %in% global$metadata$column_name){
                global$metadata[nrow(global$metadata) + 1, 1] <- cname
            }

            txt = d[, cname]
            longest.line = max(nchar(txt))
            if(longest.line > 50){
                corpus <- Corpus(VectorSource(txt))
                corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to='ASCII', sub='byte')))
                corpus <- tm_map(corpus, content_transformer(tolower))
                corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes=TRUE)
                corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
                dtm <- DocumentTermMatrix(corpus)
                m <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, dims=c(dtm$nrow, dtm$ncol), dimnames=dtm$dimnames)
                global$corpa[[cname]] = m
            }
        }
    }
    save(global, file=rdat.filename)
}
