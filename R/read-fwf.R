# 
# Author: Andrie
#----------------------------------------------------------------------------------



#' Read in fixed-width files quickly.
#' 
#' Experimental replacement for read.fwf that runs much faster.  However, it is much less flexible than read.fwf.
#' 
#' @param file Character vector: name of file 
#' @param widths Numeric vector: column widths. Negative numbers mean "skip this many columns". Use an NA as the final element if there are likely to be extra characters at the end of each row after the last one that you're interested in. 
#' @param col.names names for the columns that are NOT skipped 
#' @param colClasses can be used to control type conversion; see \code{\link{read.table}}. It is an optional vector whose names must be part of col.names. There is one extension of the \code{\link{read.table}} rules:a colClasses string starting \code{\link{POSIXct}}. will trigger automatic conversion to POSIXct, using the rest of the string as the format specifier. 
#' @param tz used in auto-conversion to \code{\link{POSIXct}} when \code{colClasses} is set 
#' @param dec the character to be assumed for decimal points. Passed to \code{\link[utils]{type.convert}}
#' @param ... ignored 
fast.read.fwf <- function (
    file, widths, col.names = NULL, colClasses = NA,  
    tz = "", dec=".", ...){
  
  if(is.null(col.names)) col.names <- paste("V", 1:length(widths), sep="")
  
  fs <- file.info(file)$size
  acw <- abs(c(widths, 1, 1)) # 1 for \r and 1 for \n
  
  nl <- fs%/%sum(acw)
  #if (fs%%sum(acw) != 0) stop("Line length mismatch")
  fields <- readChar(file, rep(acw, nl))
  
  # Remove \r from fields
  rmv <- grepl("[\r|\n]", fields)
  fields <- fields[!rmv]
  
  cols <- length(widths)
  fields <- matrix(fields, nrow=nl, ncol=cols, byrow=TRUE)
  #colnames(fields) <- col.names
  
  #df <- data.frame(fields, stringsAsFactors=FALSE)

  #Original code
#  for (i in (1L:cols)){
#    df[, i] <- if (is.na(colClasses[i])) 
#          type.convert(fields[, i], as.is = TRUE, dec = dec, na.strings = character(0L))
#        else if (colClasses[i] == "factor") 
#          as.factor(fields[, i])
#        else if (colClasses[i] == "Date") 
#          as.Date(fields[, i])
#        else if (colClasses[i]=="POSIXct") 
#          as.POSIXct(fields[, i])
#        else if(colClasses[i]=="logical")
#          as.logical(as.numeric(fields[, i]))
#        else methods::as(fields[, i], colClasses[i])
#  }
  
  modColClass <- function(i){
    if (is.na(colClasses[i])) 
          type.convert(fields[, i], as.is = TRUE, dec = dec, na.strings = character(0L))
        else if (colClasses[i] == "factor") 
          as.factor(fields[, i])
        else if (colClasses[i] == "Date") 
          as.Date(fields[, i])
        else if (colClasses[i]=="POSIXct") 
          as.POSIXct(fields[, i])
        else if(colClasses[i]=="logical")
          as.logical(as.numeric(fields[, i]))
        else methods::as(fields[, i], colClasses[i])
  }
  
  df2 <- quickdf(lapply(seq_len(ncol(fields)), modColClass))
  names(df2) <- col.names
  df2
}
