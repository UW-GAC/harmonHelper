#' Loads datasets returned by harmonization. Requires .RData files to return dataset named `dat`
#' @param file_list A list of files to be passed to `load`
#' @export

readDats <- function(file_list){
  dats <- list()
  for(i in seq_along(file_list)){
    load(file_list[i])
    dat <- tbl_df(dat)
    dats[[i]] <- dat
  }
  return(dats)
}
