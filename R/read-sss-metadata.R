


#' Reads a triple-s XML (sss) metadata file, as specified by the triple-s XML standard.  
#'
#' This function reads a .sss XML metadata file.  The .sss standard defines a standard survey structure
#'
#' @param x Name of .sss file containing the survey metadata
#' @param SSSfilename No longer used. Use `x` instead.
#' @export 
#' @seealso [parseSSSmetadata()], [read.sss()], [readSSSdata()]
#' @keywords read
#' @importFrom xml2 read_xml
#' @example inst/examples/example-read-sss-metadata.R
readSSSmetadata <- function(x, SSSfilename){
  if(!missing(SSSfilename)) {
    warning("The argument SSSfilename has been deprecated. Use `x` instead.")
    if(missing(x)) x <- SSSfilename
  }
  assert_that(is.character(x))
  assert_that(file.exists(x))
  read_xml(x)
}


is.xml_document <- function(x)inherits(x, "xml_document")

#' Parses a triple-s XML (sss) metadata file, as specified by the triple-s XML standard.  
#'
#' This function reads and parses a .sss XML metadata file as well as its associated .asc data file. The .sss standard defines a standard survey structure
#' 
#' @param x An XML document - as returned by [XML::xml()], or [readSSSmetadata()]
#' @param XMLdoc No longer used. Use `x` instead.
#' @keywords parse
#' @export 
#' @importFrom xml2 xml_attrs xml_children xml_child xml_attr xml_length xml_contents
#' @seealso readSSSmetadata, read.sss, readSSSdata
parseSSSmetadata <- function(x, XMLdoc){
  if(!missing(XMLdoc)) {
    warning("The argument XMLdox has been deprecated. Use `x` instead")
    if(missing(x)) x <- XMLdoc
  }
  assert_that(is.xml_document(x))
  r <- xml_child(x, "survey/record")
  ra <- xml_attrs(r)
  format <- if("format" %in% names(ra)) ra[["format"]] else "fixed"
  skip   <- if("skip"   %in% names(ra)) ra[["skip"]] else 0
  variables <- fastdf(
    do.call(rbind, lapply(xml_children(r), getSSSrecord)) 
  )
  variables$positionFinish <- as.numeric(variables$positionFinish)
  variables$positionStart <- as.numeric(variables$positionStart)
  
  codes <- fastdf(do.call(rbind, lapply(xml_children(r), getSSScodes)))
  list(variables = variables, 
       codes = codes, 
       format = format, 
       skip = skip
  )
}


#' Reads a triple-s XML (asc) data file, as specified by the triple-s XML standard.
#'
#' This function reads an `.asc`` data file.
#'
#' @param x Name of .asc file containing the survey metadata
#' @param ascFilename No longer used. Use `x` instead.
#' @export 
#' @seealso [read.sss()], [readSSSmetadata()]
#' @keywords parse
#' @example inst/examples/example-read-sss-metadata.R
readSSSdata <- function(x, ascFilename){
  if(!missing(ascFilename)) {
    warning("The argument ascFilename has been deprecated. Use `x` instead")
    if(missing(x)) x <- ascFilename
  }
  assert_that(is.character(x))
  assert_that(file.exists(x))
  suppressWarnings(scan(x, sep="\n", what="character"))
}
