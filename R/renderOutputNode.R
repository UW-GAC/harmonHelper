#' Create the output node for a phenotype harmonization configuration file
#' @param output_dir Output directory
#' @param output_prefix Prefix for output files
#' @return Output node for a phenotype harmonization configuration file
#' @import XML
#' @import magrittr
#' @export

renderOutputNode <- function(output_dir, output_prefix){
    output_node <- xmlNode("output")
    output_node %<>% addChildren(kids = list(xmlNode("output_directory",
                                                     value=output_dir)))

    output_node %<>% addChildren(kids =
                       list(xmlNode("output_prefix", value=output_prefix)))
}
