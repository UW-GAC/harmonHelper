library(testthat)
library(magrittr)
library(XML)
source("../../R/renderOutputNode.R")

test_that("Render output node", {
    test_dir <- "test_dir"
    test_prefix <- "test_prefix"
    
    test_node <- renderOutputNode(test_dir, test_prefix)

    ref_node <- xmlNode("output")
    ref_node %<>% addChildren(kids = list(xmlNode("output_directory",
                                                     value=test_dir)))
    ref_node %<>% addChildren(kids =
                       list(xmlNode("output_prefix", value=test_prefix)))

    expect_identical(test_node, ref_node)
})
