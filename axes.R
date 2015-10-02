# =========================================================================
# Construct the axis object
# Fields:
#   1) name - column name (not the axis label)
#   2) values - data
#   3) defined - logical, if FALSE, the plotter will ignore this axis
#   4) type - ['cat', 'num', 'longcat', 'seq', 'cor']
#   5) (if type is 'seq') seq
#   6) (if type is 'cor') mat
# =========================================================================
prepare.axis <- function(aname, global){
    a <- list()
    a$name    <- aname
    a$values  <- global$table[[a$name]]
    a$defined <- length(a$values > 0)
    if(aname %in% names(global$type)){
        a$type <- global$type[aname]
    } else {
        a$type <- '-'
    }
    if(a$type == 'cor'){
        a$mat <- global$corpa[[a$name]]
    }
    if(a$type == 'seq'){
        a$seq <- global$seq[[a$name]]
    }
    return(a)
}



# =========================================================================
# Subset the axis based on the logical selection vector
# =========================================================================
get.column.selection <- function(a, global, selection){
    cat('entering get.column.selection\n')
    if(a$type == 'cor'){
        a$mat <- a$mat[selection, ]
    }
    if(a$type == 'seq'){
        keys  <- global$table[selection]$KEY
        a$seq <- a$seq[keys, allow.cartesian=TRUE]
    }
    a$values <- a$values[selection]
    return(a)
}



# =========================================================================
# If the axis is named 'Selection', replace booleans with string labels
# If nothing is selected, set the axis type to empty ('-')
# =========================================================================
factor.selection <- function(a, global, selection){
    if(a$name == 'Selection'){
        k <- sum(selection)
        if(k > 0 && k < nrow(global$table)){
            a$values <- as.factor(ifelse(selection, 'selected', 'unselected'))
        } else {
            a$type = '-'
        }
    }
    return(a)
}



# =========================================================================
# Determine whether to
#   1) extract the selected rows OR
#   2) group by them
# The latter is done if
#   1) the user has chosed 'Selection' as an axis AND
#   2) at least one column is selected
# =========================================================================
selection.as.factor <- function(axes, selection){
    any('Selection' %in% names(axes)) && sum(selection) > 0
}



# =========================================================================
# Prepare columns of data
# Parameters:
#   axes: list of axis names mapped to column names
#   global: list with the following components
#     * table
#     * type
#     * corpa
#     * seq
#   selection: a logical vector specifying whether a given row is selected
# =========================================================================
dataAxis <- function(axes, ...){
    axes <- lapply(axes, prepare.axis, ...)
    axes <- lapply(axes, factor.selection, ...)
    if(selection.as.factor(axes, ...)){
        axes <- lapply(axes, get.column.selection, ...)
    }
    return(axes)
}
