#' Combine duplicate columns
#' @param x A dataframe
#' @param cols A vector of names of duplicate columns, as strings
#' @param new_name The new name of the combined column, as a string
#' @export
combineDupCols <- function(x, cols, new_name){
    check <- apply(x[, cols], 1, scales::zero_range)
    flag <- FALSE
    if (!all(check)){
        warning("Not all columns match")
        print(paste("n mismatches:", sum(!check)))
        flag <- TRUE
    }
    x[, new_name] <- x[, cols[1]]
    for (i in 2:length(cols)){
        ind <- is.na(x[, new_name])
        x[ind, new_name] <- x[ind, cols[i]]
        }
    if (!flag){
        x <- x[, !names(x) %in% cols]
    }
    return(x)
}
