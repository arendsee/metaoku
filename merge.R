require(data.table)
require(Matrix)

# All datasets are
# 1. TAB-delimited
# 2. Have headers
# 3. The first column is either 'model' or 'locus'

rdat.filename = 'global-data.Rdat'

if(! file.exists(rdat.filename)){
    global          = list()
    global$datasets = list()
    global$columns  = c()
    global$indx     = c()
    global$files    = c()
    global$loci     = c()
    global$corpa    = list()

    for(f in list.files('data/', '*.tab')){
        d <- as.data.table(read.delim(paste0('data/', f), quote="", stringsAsFactors=FALSE))

        global$columns <- c(global$columns, colnames(d))
        global$indx    <- c(global$indx, colnames(d)[1])
        global$files   <- c(global$files, f)

        # if the first column is 'locus' of 'model', index on this column
        # otherwise skip this dataset
        if('model' %in% colnames(d)) {
            global$models <- unique(c(global$models, as.character(d$model)))
            d$model = as.vector(d$model)
            setkey(d, key='model')
        } else if('locus' %in% colnames(d)) {
            d$locus = as.vector(d$locus)
            setkey(d, key='locus')
        } else {
            warning(paste('table', f, 'skipped'))
            next
        }

        for (cname in colnames(d)){
            txt = as.character(data.frame(d)[, cname])
            longest.line = max(nchar(txt))
            if(longest.line > 50){
                write(paste(cname), 'log', append=TRUE)
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

        global$datasets[[f]] <- d
    }

    global$columns <- unique(global$columns[-which(global$columns %in% c('model', 'locus'))])
    global$loci    <- sub('\\.\\d+', '', global$models)
    global$columns <- sort(global$columns)

    # model.data <- MergeData(d)
    save(global, file=rdat.filename)
}
