#' @title Get Spek Path
#' @describeIn suite_paths Path to a data suite csv in package.
#' @param suite_name String name of suite of examples to access.  Use `list_suites()` for available suites.
#' @export
get_data_path <- function(suite_name){ internal_suite_path(suite_name, "data") }

#' @title Get Spek Path
#' @describeIn suite_paths Path to a data suite csv in package.
#' @param suite_name String name of suite of examples to access.  Use `list_suites()` for available suites.
#' @param stage String name of stage of pipeline.  'initial' for handwritten spek, 'candidates' for processed spek with candidates.
#' @export
get_spek_path <- function(suite_name, stage="initial"){ internal_suite_path(suite_name, "spek", stage) }

#' @title Get Annotations Path
#' @describeIn suite_paths Path to a data suite csv in package.
#' @export
get_annotations_path <- function(suite_name){ internal_suite_path(suite_name, "anno") }


#' @title Internal suite Path
#' @description Get the path to the spek,data, or annotation for a suite.
#' @noRd
#' @importFrom rlang abort
internal_suite_path <- function(suite_name, kindof, stage="initial"){
  if(!(suite_name %in% SE$SUITE_NAMES)){ rlang::abort(SE$UNRECOGNIZED_NAME) }

  file_suffix <-switch(kindof,
                       "data"="-data.csv",
                       "spek"="-spek.json",
                       "anno"="-annotations.r")
  file_thorax <-switch(stage,
                       "initial"="",
                       "candidates"="-candidates")

  file_name <- paste0(suite_name,file_thorax,file_suffix)
  system.file("extdata", file_name, package="spekex", mustWork = TRUE)
}
