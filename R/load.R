# ==============================================================================
# If there are multiple datafiles in one data folder, try to merge them by the
# first column. Die on failure.
# ==============================================================================
load.data <- function(data.dir, data.pat, skip=NULL){
    require(data.table)
    cat('\t  * merge.files\n')
    global.key <- NULL
    global.d   <- NULL
    for (f in list.files(path=data.dir, pattern=data.pat, full.names=TRUE)){
        if(basename(f) %in% skip) next
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

    df <- load.data(data.pat=config$data_pat, data.dir=data.dir, skip=config$fields)

    mdfile <- file.path(data.dir, config$fields)
    if(file.exists(mdfile)){
        md <- read.delim(mdfile, stringsAsFactors=FALSE)
    } else {
        cat(sprintf('No FIELDS file found for %s at %s\n', dataname, mdfile))
        md <- NULL
    }

    source('R/dataClass.R', local=TRUE)
    dataset <- DataSet$new()
    dataset$build(
        df,
        metadata=md,
        max_levels=config$max_levels,
        max_prop=config$max_prop,
        max_length=config$max_length)

    if(!dir.exists(config$save_dir)){
        dir.create(config$save_dir, recursive=TRUE)
    }
    save(dataset, file=rdat)
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
