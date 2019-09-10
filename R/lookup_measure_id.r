#' @title Lookup Measure ID by identifier
#' @return measure id string
#' @description Lookup the measure id from the identifier value.
#'   Identifier values show up as values in the measurement column of data.
#' @importFrom rlang abort is_empty
#' @export
lookup_measure_id <- function(identifier, spek){
  measures <- measures_from_spek(spek)
  idents <- sapply(measures, FUN=identifier_of_measure)
  pos <- which(idents == identifier)
  if(rlang::is_empty(pos)){
    rlang::abort(paste(identifier, SE$MEASURE_NOT_IN_SPEK))
  }else{
    id_of_measure(measures[[pos]])
  }
}
