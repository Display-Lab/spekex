#' @title Read Spek
#' @description Read json-ld spek from disk.
#' @param spek_path String path to file on disk or NULL
#' @return List representation of the spek from disk.
#' @note this function will be extracted to spektools package
#'
#' @importFrom jsonlite fromJSON
#' @importFrom jsonld jsonld_expand
#' @importFrom readr read_file
#' @importFrom rlang warn
#' @export
read_spek <- function(spek_path = NULL){
  if(is.null(spek_path)){
    rlang::abort(SE$NO_SPEK_MSG)
  } else {
    spek_str <- readr::read_file(spek_path)
  }

  expanded <- jsonld::jsonld_expand(spek_str )
  converted <- jsonlite::fromJSON(expanded, simplifyDataFrame = F)
  fill_measure_and_comparator_ids(converted[[1]])
}


fill_measure_and_comparator_ids <- function(spek){
  measures <- measures_from_spek(spek)
  # fill ids of measures
  filled_measures <- fill_decendent_ids(measures, SE$MEASURE_BNODE_PREFIX)
  # fill ids of comparators of each measure
  # spek[[SE$ABOUT_MEASURE_IRI]] <- lapply(filled_measures, fill_comparator_ids)
  spek[[SE$ABOUT_MEASURE_IRI]] <- fill_decendent_ids(measures, SE$MEASURE_BNODE_PREFIX)
  spek
}

fill_comparator_ids <- function(measure){
  #comparators <- comparators_of_measure(measure)
  #measure[[SE$WITH_COMPARATOR_IRI]] <- fill_ids(comparators, "_:c")
  fill_all_ids(measure, "_:c", 1000)
}

fill_ids <- function(nodes, prefix, numbering){
  # Start numbering at 1000.
  ids <- paste(prefix, 1000:(length(nodes)+999), sep="")
  mapply(FUN=add_id_to_node, ids, nodes, USE.NAMES = F, SIMPLIFY=F)
}

add_id_to_node <- function(id, node){
  if(is.null(node[['@id']])){
    node[['@id']] <- id
  }
  node
}

# Recursively add blank node ids for those missing @ids but with @type
# Pass initial number to begin with as numbering.
fill_decendent_ids <- function(node, prefix=SE$DEFAULT_BNODE_PREFIX, numbering=1000){
  # Setup env to serve as pass by reference integer
  if(is.numeric(numbering)){
    n <- numbering
    numbering <- new.env(parent=emptyenv())
    numbering$n <- n
  }
  # Walk all nodes of spek.
  # If node has a @type attribute and no @id, add id
  if(is.list(node) && length(node) > 0){
    if(!is.null(node[['@type']]) && is.null(node[['@id']]) ){
      node[['@id']] <- paste0(prefix, numbering$n)
      numbering$n <- numbering$n + 1
    }
    node <- lapply(X=node, FUN=fill_decendent_ids, prefix, numbering)
  }
  node
}
