#' @title Get ID Columns from Spek
#' @description Examines column use attribute for "identifier" in column spec.
#' @return character vector of identifier columns
#' @describeIn spek_extractors
#' @note this function will be extracted to spektools package
#' @export
get_id_col_from_spek <- function(spek) {
  column_list <- get_column_list(spek)
  column_names <- sapply(column_list, FUN=get_name_of_column)
  column_uses <- sapply(column_list, FUN=get_use_of_column)

  return(column_names[which(column_uses == "identifier")])
}

#' @title Get Value or Numerator columns
#' @description Get names of columsn that have "value" or "numerator" column use
#' in the column spec.
#' @return character vector of names of value or numerator columns
#' @describeIn spek_extractors
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
#' @describeIn spek_extractors
#' @note this function will be extracted to spektools package
#' @export
get_column_list <- function(spek) {
  spek[[SE$INPUT_TABLE_IRI]][[1]][[SE$TABLE_SCHEMA_IRI]][[1]][[SE$COLUMNS_IRI]]
}

#' @title Get Name of Column
#' @description Extract column name from a single column specification
#' @return character vector of the column name
#' @describeIn spek_extractors
#' @note this function will be extracted to spektools package
#' @export
get_name_of_column <- function(column_specification) {
  column_specification[['http://www.w3.org/ns/csvw#name']][[1]][['@value']]
}

#' @title Get Use of Column
#' @description Extract column use from a single column specification
#' @return character vector of the column use
#' @describeIn spek_extractors
#' @note this function will be extracted to spektools package
#' @export
get_use_of_column <- function(column_specification) {
  column_specification[['http://example.com/slowmo#ColumnUse']][[1]][['@value']]
}
