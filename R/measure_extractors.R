#' @title Get Measures from Spek
#' @return list of measures
#' @describeIn measure_extractors Retrieve list of measures from the spek
#' @note this function will be extracted to spektools package
#' @export
measures_from_spek <- function(spek){ spek[[SE$ABOUT_MEASURE_IRI]] }

#' @title Title of Measure
#' @return list of measures
#' @describeIn measure_extractors Retrieve title of a measure
#' @note this function will be extracted to spektools package
#' @export
title_of_measure <- function(measure){ measure[[SE$DC_TITLE_IRI]][[1]][['@value']] }

#' @title Identifier of Measure
#' @describeIn measure_extractors Retrieve id of a measure
#' @export
identifier_of_measure <- function(measure){ measure[[SE$SCHEMA_IDENTIFIER_IRI]][[1]][['@value']] }

#' @title ID of Measure
#' @describeIn measure_extractors Retrieve id of a measure
#' @export
id_of_measure <- function(measure){ measure[['@id']] }
