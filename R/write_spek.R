#' @title Write Spek
#' @description Output spek to file or stdout
#' @param spek The list representation of the spek to be written
#' @param outpath Path to file. Defaults to "" for stdout.
#' @note this function will be extracted to spektools package
#' @export
write_spek <- function(spek, outpath=""){
  if(identical(outpath, "")){
    write_spek_stdout(spek)
  }
  else{
    write_spek_file(spek, outpath)
  }
}

#' @describeIn write_spek write spek to stdout
#' @importFrom jsonlite toJSON
write_spek_stdout <- function(spek){
  spek_json <- jsonlite::toJSON(spek, auto_unbox = T)
  cat(spek_json, "")
}

#' @describeIn write_spek write spek to given path
#' @importFrom jsonlite toJSON
#' @importFrom rlang abort
write_spek_file <- function(spek, outpath){
  out <- file(outpath, "wt")
  if(!isOpen(out, "w")){
    rlang::abort("Output file is not writeble.")
  }
  spek_json <- jsonlite::toJSON(spek, auto_unbox = T)
  cat(spek_json, file=out)
  close(out)
}
