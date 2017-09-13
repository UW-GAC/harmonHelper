library(testthat)
library(dplyr)
source("../../R/combineDupCols.R")

test_that("Test combineDupCols()", {
              ref  <- mutate(iris, Sepal.Length2 = Sepal.Length, 
                             Sepal.Length3 = Sepal.Length, 
                             Sepal.Length4 = Sepal.Length)
              ref$Sepal.Length2[1:15] <- NA
              ref$Sepal.Length3[11:20] <- NA
              ref$Sepal.Length4[21:30] <- 1:10
              expect_warning(combineDupCols(ref, 
                                            c("Sepal.Length", "Sepal.Length4"), 
                                            "sl"), 
                             "Not all columns match")

              order_alph <- function(x) x[, order(names(x))]

              ref_2 <- select(ref, -Sepal.Length2) %>% 
                  rename(sl = Sepal.Length) %>% 
                  order_alph()
              test_2 <- combineDupCols(ref, 
                                      c("Sepal.Length2", "Sepal.Length"), 
                                      "sl") %>% 
                    order_alph()
              expect_identical(test_2, ref_2)

              ref_3 <- select(ref, -Sepal.Length2, -Sepal.Length3) %>% 
                  rename(sl = Sepal.Length) %>% 
                  order_alph()
              test_3 <- combineDupCols(ref, 
                                       c("Sepal.Length2", 
                                         "Sepal.Length3", 
                                         "Sepal.Length"), "sl") %>% 
                    order_alph()
              expect_identical(test_3, ref_3)
})
