#' @title Lookup Measure ID by identifier
#' @return measure id string
#' @description Lookup the measure id from the identifier value.
#'   Identifier values show up as values in the measurement column of data.
#' @export
lookup_measure_id <- function(identifier, spek){
  measures <- measures_from_spek(spek)
  idents <- sapply(measures, FUN=identifier_of_measure)
  pos <- which(idents == identifier)
  id <- id_of_measure(measures[[pos]])
}
