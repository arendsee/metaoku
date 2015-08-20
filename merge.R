require(data.table)

# All datasets are
# 1. TAB-delimited
# 2. Have headers
# 3. The first column is either 'model' or 'locus'

datalist <- list()
cols <- c()
indx <- c()
loci <- c()
files <- c()
models <- c()
for(f in list.files('data/', '*.tab')){
    d <- read.delim(paste0('data/', f), quote="")
    datalist[[f]] <- d
    cols <- c(cols, colnames(d))
    indx <- c(indx, colnames(d)[1])
    files <- c(files, f)
    if(colnames(d)[1] == 'loci'){
        loci <- unique(c(loci, d$locus))
    } else {
        models <- unique(c(models, as.character(d$model)))
    }
}
cols <- unique(cols[-which(cols %in% c('model', 'locus'))])
names(indx) <- files

# model.data <- MergeData(d)
save(datalist, cols, file='datalist.Rdat')
