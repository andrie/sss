# sss internal functions
# 
# Author: Andrie
###############################################################################

#' The top level parse function 
#'
#' @param sss Parsed sss metadata file
#' @param asc Parsed sss data file
#' @keywords internal
parse_sss <- function(sss, asc){
	n <- nrow(sss$variables)
	
	df <- list_to_df(
			llply(
					1:n, 
					function(x)get_variable_position(sss, asc, x)), 
			sss$variables$name)
	df <- llply_colwise(sss, df, change_values)
	df <- llply_colwise(sss, df, change_type)
	df <- parse_multiple(sss, df)
	df
	
}



#' Reads all "variables" inside the triple-s "record" 
#'
#' This function parses the record node, extracts all variables
#' and creates a data frame with information about size, position, type, etc.
#'
#' @param xmlNode XML node
#' @keywords internal
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
#' @keywords internal
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


#' Reads metadata to find position of variables 
#'
#' @param sss Parsed .sss metadata information
#' @param asc Parsed .asc data
#' @param variable_row Variable row
#' @keywords internal
get_variable_position <- function(sss, asc, variable_row){
	position <- c(
			sss$variables[variable_row,]$position_start,
			sss$variables[variable_row,]$position_finish
	)
	ldply(asc, function(x)substring(x, position[1], position[2]))	
}

#' Converts list to data frame  
#'
#' @param list A list
#' @param	names Character: a list of names
#' @keywords internal
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
#' @seealso llply_colwise
#' @keywords internal
change_type <- function (sss, df, i){
	type <- sss$variables$type[i]
	x <- df[, i]
	
	if (type=="quantity"){
		xnew <- as.numeric(x)
		xnew[is.na(xnew)] <- x[is.na(xnew)]
		x <- xnew
	}
	
	if (type=="character"){
		x <- factor(str_trim(x))
	}
	x
}

#' Applies coded values to asc data, as described in sss metadata  
#'
#' @param sss Parsed .sss metadata information
#' @param df Parsed .asc data
#' @param i The column number of sss that should be processed 
#' @keywords internal
#' @seealso llply_colwise
change_values <- function (sss, df, i){
	has_values <- sss$variables$has_values[i]
	x <- df[, i]
	if (has_values){
		code_frame <- subset(
										sss$codes,
										sss$codes$ident==sss$variables$ident[i], 
										select=c("code", "codevalues")
								)
								
		row.names(code_frame) <- code_frame$code
		xnew <- code_frame[x, "codevalues"]
		xnew[is.na(xnew)] <- x[is.na(xnew)]
		x <- xnew
	}
	x
}

#' Splits a data frame column into multiple columns  
#' 
#' @param df_column A fixed-width column in a data frame
#' @param start Numeric: start position
#' @param end Numeric: end position
#' @seealso llply_colwise
#' @keywords internal
split_multiple <- function(df_column, start, end){
	ldply(df_column, function(x)substring(x, start, end))
}

#' Parse field of type "multiple"   
#' 
#' @param sss Parsed .sss metadata information
#' @param df Parsed .asc data
#' @seealso llply_colwise
#' @keywords internal
parse_multiple <- function (sss, df){
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
			subfields <- sss$variables$subfields[i]
			if (subfields > 0){
				### Process multiple with subfields
				width <- sss$variables$width[i]
				for (j in 1:subfields){
					newname <- as.character(paste(names(df)[i], j, sep="_"))
					dfnew <- cbind(dfnew, split_multiple(df[, i], j, j+width-1))
					names(dfnew)[ncol(dfnew)] <- newname
				}
			} else {
				### Process multiple without subfields
				subfields <- as.numeric(sss$variables$position_finish[i]) - 
						as.numeric(sss$variables$position_start[i]) + 1 
				for (j in 1:subfields){
					newname <- as.character(paste(names(df)[i], j, sep="_"))
					dfnew <- cbind(dfnew, split_multiple(df[, i], j, j))
					names(dfnew)[ncol(dfnew)] <- newname
				}
			}	
		} # end if multiple		
	} # end for
	dfnew
}

	

#' Applies a custom function to each column of a data.frame  
#'
#' @param sss Parsed .sss metadata information
#' @param df Parsed .asc data
#' @param custom_function A function that operates on a single column 
#' @keywords internal
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

