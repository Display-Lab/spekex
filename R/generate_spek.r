#' @title Generate Spek
#' @description Generate R representation of spek.
#' @param suite_name String name of an example suite.  See `list_suites()` for list of suitenames.
#' @return List representation of the spek from disk.
#' @export
generate_spek <- function(suitename){
  read_spek(get_spek_path(suitename))
}
