#' Guess the data filename given the sss filename.
#' 
#' @param x filename
#' @keywords internal
#' @noRd
guess_asc_filename <- function(x){
  ext <- get_sss_format(x)
  fn <- file.path(
    dirname(x),
    paste0(gsub("\\..*$", "", basename(x)), ext)
  )
  
  fe <- file.exists(fn)
  if (any(fe)) {
    fn[fe]
  } else {
    stop("Unable to find guessed data file at ", fn, call. = FALSE)
  }
}


# Guess if format is csv or asc
get_sss_format <- function(x){
  y <- xml_child(readSSSmetadata(x), "survey/record")
  z <- xml_attr(y, "format")
  if(!is.na(z) && z == "csv") ".csv" else c(".asc", ".dat")
}
