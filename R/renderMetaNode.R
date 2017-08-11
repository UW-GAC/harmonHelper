#' Creates the metadata node for a phenotype harmonization file
#' @param name Name of the harmonized trait
#' @param qc_doc File path to the accompanying QC document
#' @param description Description of the harmonized trait
#' @param data_type Data type of the output
#' @param unit Unit of the harmonized trait
#' @param is_demographic Flags output as a demographic trait
#' @param is_longitudinal Flags output as a longitudinal trait and allows multiple observations per topmed_subject_id
#' @param encoded A named vector of encoded values
#' @return The metadata node for a phenotype harmonization configuration file
#' @import magrittr
#' @import XML
#' @export

renderMetaNode <- function(name, description, data_type, unit, encoded, qc_doc, 
                           is_demographic = FALSE, is_longitudinal = FALSE, 
                           has_batch = FALSE){
    if (is_demographic & is_longitudinal) {
        stop("Cannot be demographic AND longitudinal")
    }

    metadata_node <- xmlNode("metadata")

    target_node <- xmlNode("target")
    target_kids <- list(xmlNode("name", value = name),
                      xmlNode("description", value = description),
                      xmlNode("data_type", value = data_type))
    if (hasArg(unit)){
        target_kids %<>% c(list(xmlNode("unit", value = unit)))
    }
    
    if (hasArg(encoded)){
        for (v in names(encoded)){
            target_kids %<>% c(list(xmlNode("value", value = encoded[v], attrs = c("code" = v))))
        }
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
    
    if (has_batch){
        metadata_node %<>%  addChildren(kids = list(xmlNode("has_batch")))
    }

    return(metadata_node)    
}
