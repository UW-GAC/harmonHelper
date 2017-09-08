#' Builds an input unit node for a phenotype harmonization configuration file
#' @param unit_name Name of the harmonization unit
#' @param stids A vector of source_trait_ids
#' @param atids A vector of source_trait_ids of phenotypes used to calculate age 
#' @param htsvids A vector of harmonized_trait_set_version_ids 
#' @param btids A vector of source_trait_ids used for separating subjects/observations into batches
#' @param hf Path to the harmonization function for this unit 
#' @return A child node for the input node in a phenotype harmonization configuration file
#' @import XML
#' @import magrittr
#' @export

renderInputUnitNode <- function(unit_name, 
                                stids = NULL, 
                                atids = NULL, 
                                htsvids = NULL, 
                                btids = NULL, 
                                hfs){
    if (all(is.null(stids), is.null(atids), is.null(htsvids), is.null(btids))){
        stop("Please supply source_trait_ids for at least one argument")
    }

    node <- xmlNode("input_unit", attrs = c(unit_id = unit_name))

    stid_node <- lapply(stids, xmlNode, name = "source_trait_id")
    node %<>% addChildren(kids = stid_node)
    atid_node <- lapply(atids, xmlNode, name = "age_trait_id")
    node %<>% addChildren(kids = atid_node)
    btid_node <- lapply(btids, xmlNode, name = "batch_trait_id")
    node %<>% addChildren(kids = btid_node)
    htsvid_node <- lapply(htsvids, xmlNode, name = "harmonized_trait_set_version_id")
    node %<>% addChildren(kids = htsvid_node)
    node %<>% addChildren(kids = list(xmlNode("custom_function", value = hfs)))
    
    return(node)
}

