#' @title Get ID Columns from Spek
#' @return character vector of identifier columns
#' @describeIn col_extractors Examines column use attribute for "identifier" in column spec.
#' @note this function will be extracted to spektools package
#' @export
get_id_col_from_spek <- function(spek){ get_colnames_by_use(spek, "identifier") }

#' @title Get Numerator Column Name
#' @return name of single numerator column
#' @describeIn col_extractors Get name of column that has "numerator" column use in the column spec.
#' @note this function will be extracted to spektools package.
#' Does not handle multi-numerator data.
#' @importFrom dplyr first
#' @export
get_numerator_colname <- function(spek){
  dplyr::first(get_colnames_by_use(spek, "numerator"), default=c(NA))
}

#' @title Get Denominator Column Name
#' @return name of single denominator column or NA when no denominator is found
#' @describeIn col_extractors Get name of column that has "denominator" column use in the column spec.
#' @note this function will be extracted to spektools package
#' Does not handle multi-denominator data.
#' @importFrom dplyr first
#' @export
get_denominator_colname <- function(spek){
  dplyr::first(get_colnames_by_use(spek, "denominator"), default=c(NA))
}

#' @title Get Measure Column Name
#' @describeIn col_extractors Get name of column that has "measure" column use in the column spec.
#' @return name of measure column or NA when no denominator is found
#' @note this function will be extracted to spektools package
#' @importFrom dplyr first
#' @export
get_measure_colname <- function(spek){
  dplyr::first(get_colnames_by_use(spek, "measure"), default=c(NA))
}

#' @title Get Value or Numerator columns
#' in the column spec.
#' @return character vector of names of value or numerator columns
#' @describeIn col_extractors Get names of columsn that have "value" or "numerator" column use
#' @note this function will be extracted to spektools package
#' @export
get_value_or_numerator_col_from_spek <- function(spek) {
  column_list <- get_column_list(spek)
  column_names <- sapply(column_list, FUN=get_name_of_column)
  column_uses <- sapply(column_list, FUN=get_use_of_column)

  return(column_names[which(column_uses == "value" | column_uses == "numerator")])
}

#' @title Get Column List
#' @return list of columns from table schema of spek
#' @describeIn col_extractors Extract column list from spek
#' @note this function will be extracted to spektools package
#' @export
get_column_list <- function(spek) {
  spek[[SE$INPUT_TABLE_IRI]][[1]][[SE$TABLE_SCHEMA_IRI]][[1]][[SE$COLUMNS_IRI]]
}

#' @title Get Name of Column
#' @return character vector of the column name
#' @describeIn col_extractors Extract column name from a single column specification
#' @note this function will be extracted to spektools package
#' @export
get_name_of_column <- function(column_specification) {
  column_specification[[SE$COLUMN_NAME_IRI]][[1]][['@value']]
}

#' @describeIn col_extractors Get data type of column
get_type_of_column <- function(column_specification) {
  column_specification[[SE$COLUMN_DTYPE_IRI]][[1]][['@value']]
}

#' @title Get Use of Column
#' @param column list representation of a single column from input table of spek
#' @return character vector of the column use
#' @describeIn col_extractors Extract column use from a single column specification
#' @note this function will be extracted to spektools package
#' @export
get_use_of_column <- function(column) {
  column[[SE$COLUMN_USE_IRI]][[1]][['@value']]
}

#' @title Get Single Column By Use
#' @param spek list representation of spek
#' @param use string name of column use
#' @return character vector of the column names
#' @describeIn col_extractors Extract column names from spek
#' @note this function will be extracted to spektools package
get_colnames_by_use <- function(spek, use){
  column_list <- get_column_list(spek)
  column_names <- sapply(column_list, FUN=get_name_of_column)
  column_uses <- sapply(column_list, FUN=get_use_of_column)

  return(column_names[which(column_uses == use)])
}
