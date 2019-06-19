#' @title Read Spek
#' @description Read json-ld spek from disk.
#' @param spek_path String path to file on disk or NULL
#' @return List representation of the spek from disk.
#' @note this function will be extracted to spektools package
#'
#' @importFrom jsonlite fromJSON
#' @importFrom jsonld jsonld_expand
#' @importFrom readr read_file
#' @importFrom rlang warn
#' @export
read_spek <- function(spek_path = NULL){
  if(is.null(spek_path)){
    rlang::abort(SE$NO_SPEK_MSG)
  } else {
    spek_str <- readr::read_file(spek_path)
  }

  expanded <- jsonld::jsonld_expand(spek_str )
  converted <- jsonlite::fromJSON(expanded, simplifyDataFrame = F)
  return(converted[[1]])
}

