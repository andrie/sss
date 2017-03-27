


#' Reads a triple-s XML (sss) metadata file, as specified by the triple-s XML standard.  
#'
#' This function reads a .sss XML metadata file.  The .sss standard defines a standard survey structure
#'
#' @param SSSfilename Name of .sss file containing the survey metadata
#' @export 
#' @seealso [parseSSSmetadata()], [read.sss()], [readSSSdata()]
#' @keywords read
#' @importFrom xml2 read_xml
#' @example inst/examples/example-read-sss-metadata.R
readSSSmetadata <- function(SSSfilename){
  read_xml(SSSfilename)
}


#' Parses a triple-s XML (sss) metadata file, as specified by the triple-s XML standard.  
#'
#' This function reads and parses a .sss XML metadata file as well as its associated .asc data file. The .sss standard defines a standard survey structure
#' 
#' @param XMLdoc An XML document - as returned by [XML::xml()], or [readSSSmetadata()]
#' @keywords parse
#' @export 
#' @importFrom xml2 xml_attrs xml_children xml_child xml_attr xml_length xml_contents
#' @seealso readSSSmetadata, read.sss, readSSSdata
parseSSSmetadata <- function(XMLdoc){
  r <- xml_child(XMLdoc, "survey/record")
  format <- if("format" %in% names(xml_attrs(r))) xml_attrs(r)[["format"]] else "fixed"
  skip   <- if("skip"   %in% names(xml_attrs(r))) xml_attrs(r)[["skip"]] else 0
  variables <- fastdf(
    do.call(rbind, lapply(xml_children(r), getSSSrecord)) 
  )
  variables$positionFinish <- as.numeric(variables$positionFinish)
  variables$positionStart <- as.numeric(variables$positionStart)
  
  codes <- fastdf(do.call(rbind, lapply(xml_children(r), getSSScodes)))
  list(variables=variables, codes=codes, format = format, skip = skip)
}


#' Reads a triple-s XML (asc) data file, as specified by the triple-s XML standard.
#'
#' This function reads and parses a .sss XML metadata file as well as its associated .asc data file. The .sss standard defines a standard survey structure
#'
#' @param ascFilename Name of .asc file containing the survey metadata
#' @export 
#' @seealso [read.sss()], [readSSSmetadata()]
#' @keywords parse
#' @example inst/examples/example-read-sss-metadata.R
readSSSdata <- function(ascFilename){
  suppressWarnings(scan(ascFilename, sep="\n", what="character"))
}




