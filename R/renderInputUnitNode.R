#' Builds an input unit node for a phenotype harmonization configuration file
#' @param unit_name Name of the harmonization unit
#' @param stids A vector of source_trait_ids
#' @param atids A vector of source_trait_ids of phenotypes used to calculate age 
#' @param htsids A vector of harmonized_trait_set_ids 
#' @param btids A vector of source_trait_ids used for separating subjects/observations into batches
#' @param hf Path to the harmonization function for this unit 
#' @import XML
#' @import magrittr

renderInputUnitNode <- function(unit_name, 
                                stids = NULL, 
                                atids = NULL, 
                                htsids = NULL, 
                                btids = NULL, 
                                hfs){
    if (all(is.null(stids), is.null(atids), is.null(htsids), is.null(btids))){
        stop("Please supply source_trait_ids for at least one argument")
    }

    node <- xmlNode("input_unit", attrs = c(unit_id = unit_name)) %>%
        addChildren(kids = list(xmlNode("custom_function", value = hfs)))

    stid_node <- lapply(stids, xmlNode, name = "source_trait_id")
    node %<>% addChildren(kids = stid_node)
    atid_node <- lapply(atids, xmlNode, name = "age_trait_id")
    node %<>% addChildren(kids = atid_node)
    htsid_node <- lapply(htsids, xmlNode, name = "harmonized_trait_set_id")
    node %<>% addChildren(kids = htsid_node)
    btid_node <- lapply(btids, xmlNode, name = "batch_trait_id")
    node %<>% addChildren(kids = btid_node)
    
    return(node)
}

