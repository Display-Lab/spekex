#' @title Generate Spek
#' @description Generate R representation of spek.
#' @param suite_name String name of an example suite.  See `list_suites()` for list of suitenames.
#' @param stage String name of stage of pipeline to emit. Valid values are: "initial", "candidates"
#' @return List representation of the spek from disk.
#' @export
generate_spek <- function(suitename, stage="initial"){
  read_spek(get_spek_path(suitename, stage))
}
