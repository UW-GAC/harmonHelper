library(testthat)
library(dplyr)
source("../../R/combineDupCols.R")

test_that("Test combineDupCols()", {
              ref  <- mutate(iris, Sepal.Length2 = Sepal.Length, 
                             Sepal.Length3 = Sepal.Length, 
                             Sepal.Length4 = Sepal.Length)
              ref$Sepal.Length2[1:15] <- NA
              ref$Sepal.Length3[10:20] <- NA
              ref$Sepal.Length4[20:30] <- 1:10
              test_warning <- combineDupCols()
              "Not all columns match"
              expect_equal()
})
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
