#' @title Get ID Columns from Spek
#' @description Examines column use attribute for "identifier" in column spec.
#' @return character vector of identifier columns
#' @describeIn col_extractors
#' @note this function will be extracted to spektools package
#' @export
get_id_col_from_spek <- function(spek){ get_colnames_by_use(spek, "identifier") }

#' @title Get Numerator Column Name
#' @description Get name of column that has "numerator" column use in the column spec.
#' @return name of single numerator column
#' @describeIn col_extractors
#' @note this function will be extracted to spektools package.
#' Does not handle multi-numerator data.
#' @importFrom dplyr first
#' @export
get_numerator_colname <- function(spek){
  dplyr::first(get_colnames_by_use(spek, "numerator"), default=c(NA))
}

#' @title Get Denominator Column Name
#' @description Get name of column that has "denominator" column use in the column spec.
#' @return name of single denominator column or NA when no denominator is found
#' @describeIn col_extractors
#' @note this function will be extracted to spektools package
#' Does not handle multi-denominator data.
#' @importFrom dplyr first
#' @export
get_denominator_colname <- function(spek){
  dplyr::first(get_colnames_by_use(spek, "denominator"), default=c(NA))
}

#' @title Get Value or Numerator columns
#' @description Get names of columsn that have "value" or "numerator" column use
#' in the column spec.
#' @return character vector of names of value or numerator columns
#' @describeIn col_extractors
#' @note this function will be extracted to spektools package
#' @export
get_value_or_numerator_col_from_spek <- function(spek) {
  column_list <- get_column_list(spek)
  column_names <- sapply(column_list, FUN=get_name_of_column)
  column_uses <- sapply(column_list, FUN=get_use_of_column)

  return(column_names[which(column_uses == "value" | column_uses == "numerator")])
}

#' @title Get Column List
#' @description Extract column list from spek
#' @return list of columns from table schema of spek
#' @describeIn col_extractors
#' @note this function will be extracted to spektools package
#' @export
get_column_list <- function(spek) {
  spek[[SE$INPUT_TABLE_IRI]][[1]][[SE$TABLE_SCHEMA_IRI]][[1]][[SE$COLUMNS_IRI]]
}

#' @title Get Name of Column
#' @description Extract column name from a single column specification
#' @return character vector of the column name
#' @describeIn col_extractors
#' @note this function will be extracted to spektools package
#' @export
get_name_of_column <- function(column_specification) {
  column_specification[[SE$COLUMN_NAME_IRI]][[1]][['@value']]
}

#' @title Get Use of Column
#' @description Extract column use from a single column specification
#' @param column list representation of a single column from input table of spek
#' @return character vector of the column use
#' @describeIn col_extractors
#' @note this function will be extracted to spektools package
#' @export
get_use_of_column <- function(column) {
  column[[SE$COLUMN_USE_IRI]][[1]][['@value']]
}

#' @title Get Single Column By Use
#' @description Extract column names from spek
#' @param spek list representation of spek
#' @param use string name of column use
#' @return character vector of the column names
#' @describeIn col_extractors
#' @note this function will be extracted to spektools package
get_colnames_by_use <- function(spek, use){
  column_list <- get_column_list(spek)
  column_names <- sapply(column_list, FUN=get_name_of_column)
  column_uses <- sapply(column_list, FUN=get_use_of_column)

  return(column_names[which(column_uses == use)])
}
