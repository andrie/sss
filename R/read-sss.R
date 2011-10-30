


#' Reads a triple-s XML (sss) metadata file, as specified by the triple-s XML standard.  
#'
#' This function reads a .sss XML metadata file.  The .sss standard defines a standard survey structure
#'
#' @param SSSfilename Name of .sss file containing the survey metadata
#' @export 
#' @seealso \code{\link{parseSSSmetadata}}, \code{\link{read.sss}}, \code{\link{readSSSdata}}
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
#' @param XMLdoc An XML document - as returned by \code{\link[XML]{xml}}, or \code{\link{readSSSmetadata}}
#' @keywords parse
#' @export 
#' @seealso readSSSmetadata, read.sss, readSSSdata
parseSSSmetadata <- function(XMLdoc){
	r <- xmlRoot(XMLdoc)[["survey"]][["record"]]
  variables <- quickdf(
      do.call(rbind, lapply(xmlChildren(r), getSSSrecord)) 
      #stringsAsFactors=FALSE)
  )
  variables$positionFinish <- as.numeric(variables$positionFinish)
  variables$positionStart <- as.numeric(variables$positionStart)
  
  codes <- quickdf(do.call(rbind, lapply(xmlChildren(r), getSSScodes)))#, stringsAsFactors=FALSE)
  list(variables=variables, codes=codes)
}


#' Reads a triple-s XML (asc) data file, as specified by the triple-s XML standard.
#'
#' This function reads and parses a .sss XML metadata file as well as its associated .asc data file. The .sss standard defines a standard survey structure
#'
#' @param ascFilename Name of .asc file containing the survey metadata
#' @export 
#' @seealso \code{\link{read.sss}}, \code{\link{readSSSmetadata}}
#' @keywords parse
#' @examples
#' # Not executed
#' # readSSSdata("sample.asc")
readSSSdata <- function(ascFilename){
	suppressWarnings(scan(ascFilename, sep="\n", what="character"))
}


#' Reads a triple-s XML (asc) data file, as specified by the triple-s XML standard.
#'
#' This function reads and parses a .sss XML metadata file as well as its associated .asc data file. The .sss standard defines a standard survey structure
#'
#' @param sssFilename Character string: name of .sss file containing the survey metadata
#' @param ascFilename Character string: name of .asc file containing survey data
#' @param sep Character vector defining the string that separates question and subquestion labels, e.g. \code{c("Q_1", "Q_2")}
#' @return
#' A data frame with one element (column) for each variable in the data set.
#' The data.frame contains several attributes:
#' 
#' \describe{
#' \item{variable.labels}{a named list of value labels with one element per variable, either NULL or a names character vector}
#' }
#' @keywords read
#' @references http://www.triple-s.org/
#' @export 
#' @examples
#' # Not executed
#' # read.sss("sample.sss, sample.asc")
read.sss <- function(sssFilename, ascFilename, sep="_"){
  message("Reading SSS metadata")
  if (class(sssFilename)=="character"){
    doc <- readSSSmetadata(sssFilename)
    sss <- parseSSSmetadata(doc)
  } else if (class(sssFilename=="XMLDocumentContent")){
    sss <- parseSSSmetadata(sssFilename)
  } else {
    stop("SSSfilename not recognised as either a file or an XML object")
  } 
  
  sss$variables <- splitSSS(sss$variable, sep)
  
  message("Reading SSS data")
  #asc <- readSSSdata(ascFilename)
  
  ascWidth <- sss$variables$colWidth
  
  types <- c(rep("character", 3), "logical", rep("numeric", 2))
  names(types) <- c("single", "multiple", "character", "logical", "numeric", "quantity")
  ascType <- types[sss$variables$type]
  ascType[sss$variables$type=="multiple"] <- "numeric"
  ascType[sss$variables$type=="multiple" & sss$variables$subfields>0] <- "character"
  
  ascNames <- sss$variables$name
  
  asc <- fast.read.fwf(
      file=ascFilename, 
      widths=ascWidth, 
      colClasses=ascType, 
      col.names=ascNames)
  #asc
  asc <- changeValues(sss, asc)
  asc <- addQtext(sss, asc)
  asc
}

	

