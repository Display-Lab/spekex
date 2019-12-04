#' @title Comparator of Measures
#' @note this function will be extracted to spektools package
#' @return list of comparators
#' @export
comparators_of_measure <- function(measure){ measure[[SE$WITH_COMPARATOR_IRI]] }

#' @describeIn comparators_of_measure Retrieve name of a comparator
#' @export
name_of_comparator <- function(comparator){ comparator[[SE$SCHEMA_NAME_IRI]][[1]][['@value']] }

#' @describeIn comparators_of_measure Retrieve title of a comparator
#' @export
title_of_comparator <- function(comparator){ comparator[[SE$DC_TITLE_IRI]][[1]][['@value']] }

#' @describeIn comparators_of_measure Id of a comparator
#' @export
id_of_comparator <- function(comparator){ comparator[['@id']] }

#' @describeIn comparators_of_measure Retrieve comparison value of a comparator
#' @export
comparison_value_of_comparator <- function(comparator){
  comparator[[SE$COMPARISON_VALUE]][[1]][['@value']]
}

#' @describeIn comparators_of_measure Retrieve type of a comparator
#' @export
type_of_comparator <- function(comparator){
  comparator[['@type']]
}

#' @describeIn comparators_of_measure Comparator Types of Measure
#' @export
comparator_types_of_measure <- function(measure){
  comparators <- comparators_of_measure((measure))
  lapply(comparators, type_of_comparator)
}
