library(testthat)
library(magrittr)
library(XML)
source("../../R/renderInputUnitNode.R")

test_that("Render input unit", {
        s1 <- 1
        s <- c(1, 2, 3)
        a1 <- 2
        a <- c(4, 5, 6)
        h1 <- 3
        h <- c(7, 8, 9)
        b1 <- 4
        b <- c(10, 11)
        f <- "path_to_function.R"

        # Test for a single id passed to each argument
        test_node_single <- renderInputUnitNode("test", s1, a1, h1, b1, f)
        ref_node_single <- xmlNode("input_unit", attrs = c(unit_id = "test")) 
            
        stid_node <- lapply(s1, xmlNode, name = "source_trait_id") 
        ref_node_single %<>% addChildren(kids = stid_node)
        atid_node <- lapply(a1, xmlNode, name = "age_trait_id")
        ref_node_single %<>% addChildren(kids = atid_node)
        btid_node <- lapply(b1, xmlNode, name = "batch_trait_id")
        ref_node_single %<>% addChildren(kids = btid_node)
        htsid_node <- lapply(h1, xmlNode, name = "harmonized_trait_set_id")
        ref_node_single %<>% addChildren(kids = htsid_node)
        ref_node_single %<>% addChildren(kids = list(xmlNode("custom_function", value = f)))

        
        expect_equivalent(test_node_single, ref_node_single)

        #Test for a mix if single and multiple ids
        test_node_mixed <- renderInputUnitNode("test", s1, a, h1, b, f)
        ref_node_mixed <- xmlNode("input_unit", attrs = c(unit_id = "test")) 

        ref_node_mixed %<>% addChildren(kids = stid_node)
        atid_node <- lapply(a, xmlNode, name = "age_trait_id")
        ref_node_mixed %<>% addChildren(kids = atid_node)
        btid_node <- lapply(b, xmlNode, name = "batch_trait_id")
        ref_node_mixed %<>% addChildren(kids = btid_node)
        ref_node_mixed %<>% addChildren(kids = htsid_node)
        ref_node_mixed %<>% addChildren(kids = list(xmlNode("custom_function", value = f)))

        expect_equivalent(test_node_mixed, ref_node_mixed)

        # Test for multiple types of each ID
        test_node_multiple <- renderInputUnitNode("test", s, a, h, b, f)
        ref_node_multiple <- xmlNode("input_unit", attrs = c(unit_id = "test")) 

        stid_node <- lapply(s, xmlNode, name = "source_trait_id") 
        ref_node_multiple %<>% addChildren(kids = stid_node)
        atid_node <- lapply(a, xmlNode, name = "age_trait_id")
        ref_node_multiple %<>% addChildren(kids = atid_node)
        btid_node <- lapply(b, xmlNode, name = "batch_trait_id")
        ref_node_multiple %<>% addChildren(kids = btid_node)
        htsid_node <- lapply(h, xmlNode, name = "harmonized_trait_set_id")
        ref_node_multiple %<>% addChildren(kids = htsid_node)
        ref_node_multiple %<>% addChildren(kids = list(xmlNode("custom_function", value = f)))
        
        expect_equivalent(test_node_multiple, ref_node_multiple)

        # Test demographic (i.e. no age trait id)
        test_node_dem <- renderInputUnitNode("test", s, NULL, NULL, NULL, f)
        ref_node_dem <- xmlNode("input_unit", attrs = c(unit_id = "test"))

        stid_node <- lapply(s, xmlNode, name = "source_trait_id") 
        ref_node_dem %<>% addChildren(kids = stid_node)
        ref_node_dem %<>% addChildren(kids = list(xmlNode("custom_function", value = f)))

        expect_equivalent(test_node_dem, ref_node_dem)

})


