#' @title Write Spek
#' @description Output spek to file or stdout
#' @param spek The list representation of the spek to be written
#' @param outpath Path to file. Defaults to "" for stdout.
#' @importFrom jsonlite toJSON
#' @export
write_spek <- function(spek, outpath=""){
  if(!identical(outpath, "")){
    out <- file(outfile, "wt")
    if(!isOpen(outfile, "w")){
      rlang::abort("Output file is not writeble.")
    }
  }else{
    out <- outpath
  }
   # convert to json
  spek_json <- jsonlite::toJSON(spek, auto_unbox = T)

  # Write content using cat to ensure sink() isn't circumvented
  cat(spek_json, out)
}
