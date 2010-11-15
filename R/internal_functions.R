# sss internal functions
# 
# Author: Andrie
###############################################################################


#' Reads all "variables" inside the triple-s "record" 
#'
#' This function parses the record node, extracts all variables
#' and creates a data frame with information about size, position, type, etc.
#'
#' @param x XML node
#' @keywords XML
#' @examples
#' get_sss_record(xmlRoot(doc)[["survey"]][["record"]])
get_sss_record <- function(xmlNode){
	p <- as.character(xmlAttrs (xmlNode[["position"]]))
	if (is.null(xmlNode[["spread"]])){
		subfields <- 0
		width <- 0
	} else {
		subfields <- xmlAttrs(xmlNode[["spread"]])[["subfields"]]
		if ("width" %in% xmlAttrs(xmlNode[["spread"]])){
			width   <- xmlAttrs(xmlNode[["spread"]])[["width"]]
		} else {
			width <- 1
		}
	}
	pfrom <- p[[1]]
	if (length(p) == 2){
		pto <- p[[2]]
	} else {
		pto <- p[[1]]
	}
	data.frame(
			ident      = as.character(xmlAttrs (xmlNode)["ident"]),
			type       = as.character(xmlAttrs (xmlNode)["type"]),
			name       = as.character(xmlValue (xmlNode[["name"]])[1]),
			label      = as.character(xmlValue (xmlNode[["label"]])[1]),
			position_start  = as.character(pfrom),
			position_finish = as.character(pto),
			subfields = subfields,
			width     = width,
			has_values = !is.null(xmlNode[["values"]]),
			stringsAsFactors = FALSE
	)
}

#is.character0 <- function(x){
#	identical(x, character(0))
#}


#' Reads all "codes" inside the triple-s "record" 
#'
#' This function parses the record node and extracts all "codes" nodes into a data.frame
#'
#' @param x XML node
#' @keywords XML
#' @examples
#' get_sss_record(xmlRoot(doc)[["survey"]][["record"]])
get_sss_codes <- function(x){
	size <- xmlSize(x[["values"]])
	if (is.null(x[["values"]])){
		df <- data.frame(
				ident      = as.character(xmlAttrs (x)["ident"]),
				code       = NA,
				codevalues = NA,
				stringsAsFactors = FALSE
		)
	} else {
		df <- data.frame(
				ident      = rep(as.character(xmlAttrs (x)["ident"]), size),
				code       = as.character(xmlSApply(x[["values"]], xmlAttrs)),
				codevalues = as.character(xmlSApply(x[["values"]], xmlValue)),
				stringsAsFactors = FALSE
		)
		df <- subset(df, df$codevalues!="character(0)")
	}
	
	df
}


get_variable_position <- function(sss, asc, variable_row){
	position <- c(
			sss$variables[variable_row,]$position_start,
			sss$variables[variable_row,]$position_finish
	)
	ldply(asc, function(x)substring(x, position[1], position[2]))	
}

## trim trailing white space
#' Trims leading and trailing white space from a string  
#'
#' @param x A string
#' @examples
#' str_clean("  string with white space   ")
str_clean <- function(x){
	x <- sub('[[:space:]]+$', '', x) ## white space, POSIX-style
	x <- sub('^[[:space:]]+', '', x) ## white space, POSIX-style
	x
}

list_to_df <- function(list, names){
	df <- as.data.frame(list, stringsAsFactors=FALSE)
	names(df) <- names
	df
}


#' Sets the data type of a column to correspond to parsed sss metadata  
#'
#' @param sss Parsed .sss metadata information
#' @param df Parsed .asc data
#' @param i The column number of sss that should be processed 
#' @keywords triple-s
#' @seealso llply_colwise
#' @examples
#' # None
change_type <- function (sss, df, i){
	type <- sss$variables$type[i]
	x <- df[, i]
	
	if (type=="quantity"){
		xnew <- as.numeric(x)
		xnew[is.na(xnew)] <- x[is.na(xnew)]
		x <- xnew
	}
	
	if (type=="character"){
		x <- factor(str_clean(x))
	}
	x
}

#' Applies coded values to asc data, as described in sss metadata  
#'
#' @param sss Parsed .sss metadata information
#' @param df Parsed .asc data
#' @param i The column number of sss that should be processed 
#' @keywords triple-s
#' @seealso llply_colwise
#' @examples
#' # None
change_values <- function (sss, df, i){
#	ident <- sss$variables$ident[i]
	has_values <- sss$variables$has_values[i]
	x <- df[, i]
	if (has_values){
		code_frame <- subset(
										sss$codes,
										ident==sss$variables$ident[i], 
										select=c(code, codevalues)
								)
								
		row.names(code_frame) <- code_frame$code
		xnew <- code_frame[x, "codevalues"]
		xnew[is.na(xnew)] <- x[is.na(xnew)]
		x <- xnew
	}
	x
}

parse_multiple <- function (sss, df, i){
	# This needs to process fields with type "multiple" in two ways:
	# 1. If subfields is set to n, where n>0, it means that there are n columns
	#    Each of these columns can take on any of the values in $codes
	#    Width defines the number of characters to read in the .asc file
	# 2. If width is not set, it means each variable was coded as boolean
	#    Thus there will be a column for each value in $codes, and each column 
	#    will be boolean
	n <- nrow(sss$variables)
	dfnew <- df
	for (i in 1:n){
		x <- df[, i]
		if (sss$variables$type[i] == "multiple"){
			x <- data.frame(
					x1=x,
					x2=x,
					stringsAsFactors=FALSE
			)
			dfnew <- cbind(dfnew, x)
		}		
	}
	dfnew
}

	

#' Applies a custom function to each column of a data.frame  
#'
#' @param sss Parsed .sss metadata information
#' @param df Parsed .asc data
#' @param custom_function A function that operates on a single column 
#' @keywords triple-s
#' @examples
#' # None
llply_colwise <- function(sss, df, custom_function){
	n <- nrow(sss$variables)
	df <- as.data.frame(
			llply(1:n, function(i)custom_function(sss, df, i)),
			stringsAsFactors=FALSE
	)
	names(df) <- sss$variables$name
	df
}

