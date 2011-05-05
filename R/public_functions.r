


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
#' 
#' @param XMLdoc An XML document - as returned by XML()
#' @keywords parse
#' @export 
#' @seealso read_sss_metadata, read_sss, read_sss_data
parse_sss_metadata <- function(XMLdoc){
	r <- xmlRoot(XMLdoc)[["survey"]][["record"]]
	#variables <- ldply(xmlChildren(r), get_sss_record)
  #print("### parse_sss_metadata ###")
  #print(variables)
  #	codes     <- ldply(xmlChildren(r), get_sss_codes)
  variables <- as.data.frame(do.call(rbind, lapply(xmlChildren(r), get_sss_record)), stringsAsFactors=FALSE)
  variables$position_finish <- as.numeric(variables$position_finish)
  variables$position_start <- as.numeric(variables$position_start)
  
  #print("### parse_sss_metadata ###")
  #print(variables)
  codes     <- as.data.frame(do.call(rbind, lapply(xmlChildren(r), get_sss_codes)), stringsAsFactors=FALSE)
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
#' @param sep character vector defining the string that separates field and subfield names
#' @export 
#' @keywords read
#' @examples
#' # Not executed
#' # read_sss("sample.sss, sample.asc")
read_sss <- function(sss_filename, data_filename, sep="_"){
  message("Reading SSS metadata")
	if (class(sss_filename)=="character"){
		doc <- read_sss_metadata(sss_filename)
		sss <- parse_sss_metadata(doc)
	} else if (class(sss_filename=="XMLDocumentContent")){
		sss <- parse_sss_metadata(sss_filename)
	} else {
		stop("sss_filename not recognised as either a file or an XML object")
	}	
  
  sss$variables <- split_sss(sss$variable, sep)
  
  message("Reading SSS data")
	#asc <- read_sss_data(data_filename)
  
  asc_width <- sss$variables$col_width
  
  types <- c(rep("character", 4), rep("numeric", 2))
  names(types) <- c("single", "multiple", "character", "logical", "numeric", "quantity")
  asc_type <- types[sss$variables$type]
  asc_type[sss$variables$type=="multiple"] <- "numeric"
  asc_type[sss$variables$type=="multiple" & sss$variables$subfields>0] <- "character"
  
  asc_names <- sss$variables$name
  
  asc <- read.fwf(file=data_filename, widths=asc_width, colClasses=asc_type, col.names=asc_names)
  
  #change_values(sss, asc)
  asc
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

	

