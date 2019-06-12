#' @title Get Spek Path
#' @description Path to a data suite csv in package.
#' @export
get_data_path <- function(suite_name){ internal_suite_path(suite_name, "data") }

#' @title Get Data Path
#' @description Path to a data suite csv in package.
#' @export
get_spek_path <- function(suite_name){ internal_suite_path(suite_name, "spek") }

#' @title Get Annotations Path
#' @description Path to a data suite csv in package.
#' @export
get_annotations_path <- function(suite_name){ internal_suite_path(suite_name, "anno") }


#' @title Internal suite Path
#' @description Get the path to the spek,data, or annotation for a suite.
internal_suite_path <- function(suite_name, kindof){
  if(!(suite_name %in% SE$SUITE_NAMES)){ stop(SE$UNRECOGNIZED_NAME) }

  file_suffix <-switch(kindof,
                       "data"="-data.csv",
                       "spek"="-spek.json",
                       "anno"="-annotations.r")

  file_name <- paste0(suite_name,file_suffix)
  system.file("extdata", file_name, package="spekex", mustWork = TRUE)
}
