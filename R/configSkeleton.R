#' Construct the skeleton for the XML configuration file for the harmonization batch. Takes named lists as arguments
#' @param dbgap_trait_accessions A vector of dbGaP variable accessions, as integers
#' @param file File path and filename for exported XML config file
#' @import magrittr
#' @import XML
#' @export


configSkeleton <- function(source_trait_ids,
                           age_trait_ids,
                           harmon_functions,
                           name,
                           qc_doc,
                           output_dir = "",
                           output_prefix = "output",
                           description = "",
                           data_type = "",
                           unit){

    stids <- source_trait_ids
    atids <- age_trait_ids
    hfs <- harmon_functions

    # Error messages
    if (sum(!(names(stids) %in% names(atids)))){
        stop("The names of source_trait_ids and age_trait_ids don't match")
    } else if (sum(!(names(stids) %in% names(hfs)))){
        stop("The names of source_trait_ids and harmon_functions don't match")
    } else if (!all(sapply(list(stids, atids, hfs), length) == length(stids))){
        stop("source_trait_ids, age_trait_ids and harmon_functions are not all the same length")
    }

    # Input
    input_node <- xmlNode("input")
    for(hu in names(stids)){
        node <- xmlNode("input_unit", attrs = c(unit_id = hu)) %>%
            addChildren(kids = list(xmlNode("custom_function", value = hfs[[hu]])))

        stid_node <- lapply(stids[[hu]], xmlNode, name = "source_trait_id")
        atid_node <- lapply(atids[[hu]], xmlNode, name = "age_trait_id")

        node %<>% addChildren(kids = c(stid_node, atid_node))
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


    # Config
    xml_config <- xmlNode("config")
    config_kids <- list(input_node,  metadata_node, output_node)
    xml_config %<>% addChildren(kids = config_kids)

    return(xml_config)
}