#' @title Columns for reader colspec
#'
#' @export
cols_for_readr <- function(spek){
  columns <- get_column_list(spek)
  col_names <- sapply(columns, get_name_of_column)
  col_types <- sapply(columns, get_type_of_column)
  col_list <- lapply(col_types, csvw_to_readr_types)

  names(col_list) <- col_names
  return(col_list)
}

#' @describeIn cols_for_readr Lookup function for csvw types to readr type specification.
#' @importFrom readr col_integer col_character col_date
csvw_to_readr_types <- function(csvw_type){
  switch(csvw_type,
    "string"  = readr::col_character(),
    "date"    = readr::col_date(),
    "integer" = readr::col_integer(),
    readr::col_character()
  )
}

