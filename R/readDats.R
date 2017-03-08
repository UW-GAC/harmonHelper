#' Loads datasets returned by harmonization. Requires .RData files to return dataset named `dat`
#' @param file_list A list of files to be passed to `load()`
#' @export

readDats <- function(file_list){
    dats <- list()
    for (file in file_list){
        load(file_list[file_list == file])
        dat <- dplyr::tbl_df(dat)
        dats[[file]] <- dat
        name <- sub("\\.RData$", "", file)
        name <- gsub("_?output_?", "_", name)
        name <- gsub("/", "_", name)
        name <- gsub("__", "_", name)
        name <- gsub("^_", "", name)
        names(dats)[names(dats) == file] <- name
    }
    names(dats) <- make.names(names(dats), unique = T)
    return(dats)
}
