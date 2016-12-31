#' Get encoded values by trait ID
#' @param db A database connection
#' @param dbgap_trait_accessions A vector of dbGaP variable accessions, as integers
#' @param file File path and filename for exported XML config file
#' @export
configSkeleton <- function(db, dbgap_trait_accessions, file = "RENAME_ME_config.xml"){
  trait_ids <- dbGetTraitInfoByAccession(db, dbgap_trait_accessions) %>%
    select(source_trait_id, study_name) %>%
    plyr::dlply("study_name") %>%
    lapply(function(x) x[,1])

  # Input

  input_node <- xmlNode("input")
  for(study in names(trait_ids)){
    node <- xmlNode("input_unit", attrs=c(unit_id=study)) %>%
      addChildren(kids = list(xmlNode("custom_function", value="")))

    kid_nodes <- lapply(trait_ids[[study]], function(x) {
      xmlNode("source_trait_id", value=x)
    }
    )
    node %<>% addChildren(kids = kid_nodes)
    input_node %<>% addChildren(kids = list(node))
  }

  # Output

  output_node <- xmlNode("output")
  output_node <- addChildren(output_node, kids = list(
    xmlNode("output_directory", value="")
  ))

  output_node <- addChildren(output_node, kids = list(
    xmlNode("output_prefix", value="output")
  ))

  # Metadata

  metadata_node <- xmlNode("metadata")

  target_node <- xmlNode("target")
  target_node <- addChildren(target_node, kids = list(
    xmlNode("name", value = ""),
    xmlNode("description", value = ""),
    xmlNode("data_type", value = ""),
    xmlNode("unit", value = "")
  ))

  metadata_node <- addChildren(metadata_node, kids = list(target_node))

  #Config
  xml_config <- xmlNode("config")
  xml_config <- addChildren(xml_config, kids = list(
    input_node,
    metadata_node,
    output_node
  ))

  saveXML(xml_config, file = file)
}
