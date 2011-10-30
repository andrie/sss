# sss internal functions - manipulate xml metadata
# 
# Author: Andrie
#----------------------------------------------------------------------------------


#' Reads all "variables" inside the triple-s "record". 
#'
#' This function parses the record node, extracts all variables
#' and creates a data frame with information about size, position, type, etc.
#'
#' @param xmlNode XML node
#' @keywords internal
getSSSrecord <- function(xmlNode){
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
  quickdf(list(
      ident      = as.character(xmlAttrs (xmlNode)["ident"]),
      type       = as.character(xmlAttrs (xmlNode)["type"]),
      name       = as.character(xmlValue (xmlNode[["name"]])[1]),
      label      = as.character(xmlValue (xmlNode[["label"]])[1]),
      positionStart  = as.character(pfrom),
      positionFinish = as.character(pto),
      subfields = subfields,
      width     = width,
      hasValues = !is.null(xmlNode[["values"]])
      #stringsAsFactors = FALSE
  ))
}

#is.character0 <- function(x){
# identical(x, character(0))
#}


#' Reads all "codes" inside the triple-s "record". 
#'
#' This function parses the record node and extracts all "codes" nodes into a data.frame
#'
#' @param x XML node
#' @keywords internal
getSSScodes <- function(x){
  size <- xmlSize(x[["values"]])
  if (is.null(x[["values"]])){
    df <- data.frame(
        ident      = as.character(xmlAttrs(x)["ident"]),
        code       = NA,
        codevalues = NA,
        stringsAsFactors = FALSE
    )
  } else {
    #browser()
    df <- data.frame(
        ident      = rep(unname(xmlAttrs (x)["ident"]), size),
        code       = as.character(xmlSApply(x[["values"]], xmlAttrs)),
        codevalues = as.character(xmlSApply(x[["values"]], xmlValue)),
        stringsAsFactors = FALSE
    )
    df <- df[df$codevalues!="character(0)", ]
  }
  
  df
}


#' Splits sss metadata into multiple lines 
#'
#' This is necessary when the variable type is "multiple"
#'
#' @param sss data.frame containing .sss metadata
#' @param sep character vector defining the string that separates field and subfield names
#' @keywords internal
splitSSS <- function(sss, sep="_"){
#  sss$length <- 1
#  SSSmult <- with(sss, sss[type=="multiple", ])
#  sss$length[sss$type=="multiple"] <- with(SSSmult, subfields)
  sss$colWidth <- with(sss, 
      as.numeric(sapply(seq_len(nrow(sss)), function(i){
                ifelse(type[i]!="multiple", 1,
                    ifelse(subfields[i]>0, 
                        subfields[i], 
                        positionFinish[i] - positionStart[i] +1))  
              }
          ))
  )
  ret <- splitMultiple(sss, sss$colWidth, sep)
  ret$name <- namesMultiple(sss$name, sss$colWidth, sep)
  ret$colWidth <- with(ret, (positionFinish - positionStart + 1)/ colWidth)
  ret
}

#' Splits a data.frame into multiple lines 
#'
#' When length is greater than 1, duplicates that row
#'
#' @param df data.frame with .sss metadata
#' @param n Numeric vector that indicates the number of times each row must be repeated
#' @param sep character vector defining the string that separates field and subfield names
#' @keywords internal
splitMultiple <- function(df, n, sep="_"){
  ret <- df[repN(n), ]
  row.names(ret) <- namesMultiple(row.names(df), n, sep)
  ret
}

#' Repeats each element n times 
#'
#' Each element is repeated n times.  This is used to construct a vector of the new length after accounting for fields of type multiple 
#'
#' @param n Numeric vector that indicates the number of times each row must be repeated
#' @keywords internal
repN <- function(n){
  rep(seq_along(n), times=pmax(1, n))
}

#' Reads all "codes" inside the triple-s "record" 
#'
#' This function parses the record node and extracts all "codes" nodes into a data.frame
#'
#' @param x XML node
#' @keywords internal
namesMultiple <- function(names, length, sep="_"){
  xl <- rep(names, times=pmax(1, length))
  sm <- rep(length<=1, times=pmax(1, length))
  xr <- paste(sep, sequence(pmax(1, length)), sep="")
  xr[sm] <- ""
  paste(xl, xr, sep="")
}




#colsplit_fixed <- function(df, n, width=1){
##  cat("### colsplit_fixed ### n = ", n, "\n")
##  cat("### colsplit_fixed ### ", df)
#  s <- unlist(lapply(df[, n], function(x)substr_fixed(x, width)))
#  ret <- as.data.frame(matrix(s, nrow=nrow(df), byrow=TRUE))
#  names(ret) <- paste(names(df)[n], seq_len(ncol(ret)), sep="_")
#  ret
#}
#
#substr_fixed <- function(x, width){
#  n <- ceiling(nchar(x)/width)
#  if(length(width)==1){
#    width <- seq(from=0, to=nchar(x), by=width)
#  } else {
#    width <- c(0, cumsum(width))
#  }
#  sapply(seq_len(length(width)-1), function(i)substr(x, width[i]+1, width[i+1]))
#}
#
#replace_df_col <- function(x, new, n){
#  col_order <- c(seq_len(n-1), seq(ncol(x)+1, ncol(x)+ncol(new)), seq(n+1, ncol(x)))
#  x <- cbind(x, new)
#  x[, col_order]
#}



