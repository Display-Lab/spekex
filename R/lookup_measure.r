#' @title Lookup Measure
#' @return measure id string
#' @description Lookup the measure from the @id value ("_:m01") or identifier value ("BP001".)
#' @return list representation of measure or empty list
#' @importFrom rlang warn is_empty
#' @export
lookup_measure <- function(identifier, spek){
  measures <- measures_from_spek(spek)
  measure_v <- measure_by_val(identifier, measures)
  measure_i <- measure_by_id(identifier, measures)
  if(length(measure_i) > 0){
    measure_i
  }else if(length(measure_v) > 0){
    measure_v
  }else{
    rlang::warn(paste(identifier, SE$MEASURE_NOT_IN_SPEK))
    list()
  }
}

#' @describeIn lookup_measure Convenience method to get @id from identifier value
#' @export
lookup_measure_id_by_value <- function(ident_value, spek){
  measure <- lookup_measure(ident_value, spek)
  id_of_measure(measure)
}

#' @describeIn lookup_measure find measure by identifier value
measure_by_val <- function(identifier, measures){
  ident_vals <- sapply(measures, FUN=identifier_of_measure)
  pos <- which(ident_vals == identifier)
  if(rlang::is_empty(pos)){
    list()
  }else{
    measures[[pos]]
  }
}

#' @describeIn lookup_measure find measure by rdf @id
measure_by_id <- function(identifier, measures){
  id_vals <- sapply(measures, id_of_measure)
  pos <- which(id_vals == identifier)
  if(rlang::is_empty(pos)){
    list()
  }else{
    measures[[pos]]
  }
}
