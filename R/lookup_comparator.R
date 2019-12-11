#' @title Lookup Comparator
#' @return list representation of comparator node
#' @description Lookup the measure from the @id value ("_:m01") or identifier value ("BP001".)
#' @return list representation of measure or empty list
#' @importFrom rlang warn is_empty
#' @export
lookup_comparator <- function(identifier, spek){
  comparator_i <- lookup_comparator_by_id(identifier, spek)
  if(length(comparator_i) > 0){
    comparator_i
  }else{
    rlang::warn(paste(identifier, SE$COMPARATOR_NOT_IN_SPEK))
    list()
  }
}

lookup_comparator_by_id <- function(id, spek){
  measures <- measures_from_spek(spek)
  comparators <- unlist(lapply(measures, comparators_of_measure), recursive = F)
  ids <- sapply(comparators, id_of_comparator)
  pos <- which(ids == id)
  # handle no comparator found
  if(length(pos) < 1){
    list()
  }else{
    comparators[[pos]]
  }
}
