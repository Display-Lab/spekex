#' @title Get Measures from Spek
#' @description Retrieve list of measures from the spek
#' @return list of measures
#' @describeIn measure_extractors
#' @note this function will be extracted to spektools package
#' @export
measures_from_spek <- function(spek){ spek[[SE$ABOUT_MEASURE_IRI]] }

#' @title Title of Measure
#' @description Retrieve list of measures from the spek
#' @return list of measures
#' @describeIn measure_extractors
#' @note this function will be extracted to spektools package
#' @export
title_of_measure <- function(measure){ measure[[SE$DC_TITLE_IRI]][[1]][['@value']] }

#' @title Identifier of Measure
#' @export
identifier_of_measure <- function(measure){ measure[[SE$SCHEMA_IDENTIFIER_IRI]][[1]][['@value']] }

#' @title Comparator of Measure
#' @export
comparator_of_measure <- function(measure){ measure[[SE$WITH_COMPARATOR_IRI]][[1]] }

#' @title Name of Comparator
#' @export
name_of_comparator <- function(comparator){ comparator[[SE$SCHEMA_NAME_IRI]][[1]][['@value']] }

#' @title Title of Comparator
#' @export
title_of_comparator <- function(comparator){ comparator[[SE$DC_TITLE_IRI]][[1]][['@value']] }

#' @title Comparison Value of Comparator
#' @export
comparison_value_of_comparator <- function(comparator){
  comparator[[SE$COMPARISON_VALUE]][[1]][['@value']]
}
