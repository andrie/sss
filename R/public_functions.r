


#' Reads a triple-s XML (sss) metadata file, as specified by the triple-s XML standard.  
#'
#' This function reads a .sss XML metadata file.  The .sss standard defines a standard survey structure
#'
#' @param sss_filename Name of .sss file containing the survey metadata
#' @export 
#' @seealso \code{\link{parse_sss_metadata}}, \code{\link{read_sss}}, \code{\link{read_sss_data}}
#' @keywords read
#' @examples
#' # Not executed
#' # read_sss_metadata("sample.sss")
read_sss_metadata <- function(sss_filename){
	xmlTreeParse(sss_filename, getDTD = F)
}

###############################################################################

#' Parses a triple-s XML (sss) metadata file, as specified by the triple-s XML standard.  
#'
#' This function reads and parses a .sss XML metadata file as well as its associated .asc data file. The .sss standard defines a standard survey structure
#' #'
#' @param XMLdoc An XML document - as returned by XML()
#' @keywords parse
#' @export 
#' @seealso read_sss_metadata, read_sss, read_sss_data
parse_sss_metadata <- function(XMLdoc){
	r <- xmlRoot(XMLdoc)[["survey"]][["record"]]
	variables <- ldply(xmlChildren(r), get_sss_record)
	codes     <- ldply(xmlChildren(r), get_sss_codes)
	list(variables=variables, codes=codes)
}


###############################################################################

#' Reads a triple-s XML (asc) data file, as specified by the triple-s XML standard.
#'
#' This function reads and parses a .sss XML metadata file as well as its associated .asc data file. The .sss standard defines a standard survey structure
#'
#' @param data_filename Name of .asc file containing the survey metadata
#' @export 
#' @seealso \code{\link{read_sss}}, \code{\link{read_sss_metadata}}
#' @keywords parse
#' @examples
#' # Not executed
#' # read_sss_data("sample.asc")
read_sss_data <- function(data_filename){
	suppressWarnings(scan(data_filename, sep="\n", what="character"))
}


###############################################################################

#' Reads and processes a triple-s files (in sss and asc formats).  
#'
#' This function reads and parses a .sss XML metadata file as well as its associated .asc data file. The .sss standard defines a standard survey structure
#'
#' @param sss_filename Name of .sss file containing the survey metadata
#' @param data_filename Name of .asc file containing survey data
#' @export 
#' @keywords read
#' @examples
#' # Not executed
#' # read_sss("sample.sss, sample.asc")
read_sss <- function(sss_filename, data_filename){
	if (class(sss_filename)=="character"){
		doc <- read_sss_metadata(sss_filename)
		sss <- parse_sss_metadata(doc)
	} else if (class(sss_filename=="XMLDocumentContent")){
		sss <- parse_sss_metadata(sss_filename)
	} else {
		stop("sss_filename not recognised as either a file or an XML object")
	}	
	
	asc <- read_sss_data(data_filename)
	parse_sss(sss, asc)
}


###############################################################################

#' A wrapper around read_sss.  
#'
#' This function reads and parses a .sss XML metadata file as well as its associated .asc data file. The .sss standard defines a standard survey structure
#'
#' @param sss_filename Name of .sss file containing the survey metadata
#' @param data_filename Name of .asc file containing survey data
#' @keywords read
#' @seealso \code{\link{read_sss}}
#' @export 
#' @examples
#' # Not executed
#' # read.sss("sample.sss, sample.asc")
read.sss <- function(sss_filename, data_filename){
	read_sss(sss_filename, data_filename)
}

	

