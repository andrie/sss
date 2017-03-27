


#' Reads a triple-s XML (sss) metadata file, as specified by the triple-s XML standard.  
#'
#' This function reads a .sss XML metadata file.  The .sss standard defines a standard survey structure
#'
#' @param SSSfilename Name of .sss file containing the survey metadata
#' @export 
#' @seealso [parseSSSmetadata()], [read.sss()], [readSSSdata()]
#' @keywords read
#' @examples
#' # Not executed
#' # readSSSmetadata("sample.sss")
readSSSmetadata <- function(SSSfilename){
  xmlTreeParse(SSSfilename, getDTD = F)
}


#' Parses a triple-s XML (sss) metadata file, as specified by the triple-s XML standard.  
#'
#' This function reads and parses a .sss XML metadata file as well as its associated .asc data file. The .sss standard defines a standard survey structure
#' 
#' @param XMLdoc An XML document - as returned by [XML::xml()], or [readSSSmetadata()]
#' @keywords parse
#' @export 
#' @seealso readSSSmetadata, read.sss, readSSSdata
parseSSSmetadata <- function(XMLdoc){
  r <- xmlRoot(XMLdoc)[["survey"]][["record"]]
  format <- if("format" %in% names(xmlAttrs(r))) xmlAttrs(r)[["format"]] else "fixed"
  skip   <- if("skip"   %in% names(xmlAttrs(r))) xmlAttrs(r)[["skip"]] else 0
  variables <- fastdf(
    do.call(rbind, lapply(xmlChildren(r), getSSSrecord)) 
  )
  variables$positionFinish <- as.numeric(variables$positionFinish)
  variables$positionStart <- as.numeric(variables$positionStart)
  
  codes <- fastdf(do.call(rbind, lapply(xmlChildren(r), getSSScodes)))
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
#' @examples
#' # Not executed
#' # readSSSdata("sample.asc")
readSSSdata <- function(ascFilename){
  suppressWarnings(scan(ascFilename, sep="\n", what="character"))
}




