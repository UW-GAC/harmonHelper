library(testthat)
library(magrittr)
library(XML)
source("../../R/renderMetaNode.R")

test_that("Render metadata node", {
    name = "test"
    description = "description"
    data_type = "data_type"
    unit = "unit"
    encoded = c("a" = "z", "b" = "y", "c" = "x")
    qc_doc = "qc_doc"

    expect_error(renderMetaNode(is_demographic = TRUE, is_longitudinal = TRUE),
                 "Cannot be demographic AND longitudinal")

    ref_node <- xmlNode("metadata")
    target_node <- xmlNode("target")

    encoded_values_node <- xmlNode("encoded_values")
    for (v in names(encoded)){
    encoded_values_node %<>% 
        addChildren(kids = list(xmlNode("value", 
                                   value = encoded[v], 
                                   attrs = c("code" = v))))
    }

    target_kids <- list(xmlNode("name", value = name),
                      xmlNode("description", value = description),
                      xmlNode("data_type", value = data_type),
                      xmlNode("unit", value = unit),
                      encoded_values_node)
    target_node %<>% addChildren(kids = target_kids)

    ref_node %<>% addChildren(kids = list(target_node,
                                          xmlNode("qc_document", value = qc_doc),
                                          xmlNode("is_longitudinal"),
                                          xmlNode("has_batch")))
    
    test_node <- renderMetaNode(name, description, data_type, unit, encoded, qc_doc,
                                is_longitudinal = TRUE, has_batch = TRUE)
    expect_identical(ref_node, test_node)

    ref_node <- xmlNode("metadata")
    target_node <- xmlNode("target")
    target_kids <- list(xmlNode("name", value = name),
                      xmlNode("description", value = description),
                      xmlNode("data_type", value = data_type))
    target_kids %<>% c(list(xmlNode("unit", value = unit)))
    target_node %<>% addChildren(kids = target_kids)
    ref_node %<>% addChildren(kids = list(target_node))
    ref_node %<>% addChildren(kids = list(xmlNode("qc_document", value = qc_doc)))
    ref_node %<>%  addChildren(kids = list(xmlNode("is_demographic")))

    test_node <- renderMetaNode(name, description, data_type, unit, qc_doc = qc_doc,
                                is_demographic = TRUE)
    expect_identical(ref_node, test_node)

    ref_node <- xmlNode("metadata")
    target_node <- xmlNode("target")

    encoded_values_node <- xmlNode("encoded_values")
    for (v in names(encoded)){
    encoded_values_node %<>% 
        addChildren(kids = list(xmlNode("value", 
                                   value = encoded[v], 
                                   attrs = c("code" = v))))
    }

    target_kids <- list(xmlNode("name", value = name),
                      xmlNode("description", value = description),
                      xmlNode("data_type", value = data_type),
                      encoded_values_node)
    target_node %<>% addChildren(kids = target_kids)
    ref_node %<>% addChildren(kids = list(target_node))
    ref_node %<>% addChildren(kids = list(xmlNode("qc_document", value = qc_doc)))

    test_node <- renderMetaNode(name, description, data_type, encoded = encoded, qc_doc = qc_doc)
    expect_identical(ref_node, test_node)
})
