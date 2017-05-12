#' Construct the skeleton for the XML configuration file for the harmonization batch. Takes named lists as arguments
#' @param source_trait_ids A named vector or list of database IDs of source traits
#' @param age_trait_ids A named vector or list of database IDs of age traits
#' @param harmonized_trait_set_ids A named vector or list of database IDs of harmonized trait sets
#' @param batch_trait_ids A named vector or list of database IDs of batch traits
#' @param harmon_functions A named vector or list of paths to harmonization functions
#' @param name Name of the harmonized trait
#' @param qc_doc File path to the accompanying QC document
#' @param output_dir Path to output directory
#' @param output_prefix String to be prepended to the output files
#' @param description Description of the harmonized trait
#' @param data_type Data type of the output
#' @param unit Unit of the harmonized trait
#' @param is_demographic Flags output as a demographic trait
#' @param is_longitudinal Flags output as a longitudinal trait and allows multiple observations per topmed_subject_id
#' @import magrittr
#' @import XML
#' @export


configSkeleton <- function(source_trait_ids,
                           age_trait_ids,
                           harmonized_trait_set_ids,
                           batch_trait_ids,
                           harmon_functions,
                           name,
                           qc_doc,
                           output_dir = "",
                           output_prefix = "output",
                           description = "",
                           data_type = "",
                           unit,
                           is_demographic = FALSE,
                           is_longitudinal = FALSE){

    stids <- source_trait_ids
    atids <- age_trait_ids
    htsids <- harmonized_trait_set_ids
    btids <- batch_trait_ids
    hfs <- harmon_functions

    # ERRORS
    
    # This function requires that all of the lists provided as arguments have the
    # same names
    if (list(atids, hfs, htsids, btids) %>% 
            lapply(names) %>% 
            lapply(sort) %>% 
            sapply(equals, sort(names(stids))) %>% 
            all %>% 
            not){
        stop("The names of the arguments do not match")
    }
    
    if (all(is.na(atids)) & !is_demographic) {
        stop("Age trait ids are missing")
    }
    
    if (all(!is.na(atids)) & is_demographic) {
        stop("Demographic datasets cannot take age trait ids")
    }
    
    # Input
    input_node <- xmlNode("input")
    for(hu in names(stids)){
        node <- xmlNode("input_unit", attrs = c(unit_id = hu)) %>%
            addChildren(kids = list(xmlNode("custom_function", value = hfs[[hu]])))

        if (stids[[hu]] %>% is.na %>% all %>% not){
            stid_node <- lapply(stids[[hu]], xmlNode, name = "source_trait_id")
            atid_node <- lapply(atids[[hu]], xmlNode, name = "age_trait_id")
            node %<>% addChildren(kids = c(stid_node, atid_node))
        }

        if (htsids[[hu]] %>% is.na %>% all %>% not){
            htsid_node <- lapply(htsids[[hu]], xmlNode, name = "harmonized_trait_set_id")
            node %<>% addChildren(kids = htsid_node)
        }
        
        if (btids[[hu]] %>% is.na %>% all %>% not){
            btid_node <- lapply(btids[[hu]], xmlNode, name = "batch_trait_id")
            node %<>% addChildren(kids = btid_node)
        }

        input_node %<>% addChildren(kids = list(node))
    }

    # Output

    output_node <- xmlNode("output")
    output_node %<>% addChildren(kids = list(xmlNode("output_directory",
                                                     value=output_dir)))

    output_node %<>% addChildren(kids =
                       list(xmlNode("output_prefix", value=output_prefix)))

    # Metadata

    metadata_node <- xmlNode("metadata")

    target_node <- xmlNode("target")
    target_kids <- list(xmlNode("name", value = name),
                      xmlNode("description", value = description),
                      xmlNode("data_type", value = data_type))
    if (hasArg(unit)){
        target_kids %<>% c(list(xmlNode("unit", value = unit)))
    }


    target_node %<>% addChildren(kids = target_kids)

    metadata_node %<>% addChildren(kids = list(target_node))

    if (hasArg(qc_doc)){
      metadata_node %<>% addChildren(kids = list(xmlNode("qc_document", value = qc_doc)))
    }

    if (is_demographic){
        metadata_node %<>%  addChildren(kids = list(xmlNode("is_demographic")))
    }
    
    if (is_longitudinal){
        metadata_node %<>%  addChildren(kids = list(xmlNode("is_longitudinal")))
    }
    
    if (btids %>% is.na %>% all %>% not){
        metadata_node %<>%  addChildren(kids = list(xmlNode("has_batch")))
    }

    # Config
    xml_config <- xmlNode("config")
    config_kids <- list(input_node,  metadata_node, output_node)
    xml_config %<>% addChildren(kids = config_kids)

    return(xml_config)
}
