# sss internal functions - manipulate xml metadata
# 
# Author: Andrie
#----------------------------------------------------------------------------------

xml_child_attrs <- function(node, name){
  xml_attrs(xml_child(node, name))
}

xml_child_attr <- function(node, name, attr){
  xml_attr(xml_child(node, name), attr)
}


# Reads all "variables" inside the triple-s "record". 
#
# This function parses the record node, extracts all variables
# and creates a data frame with information about size, position, type, etc.
#
# @param xmlNode XML node
# @keywords internal
getSSSrecord <- function(node){
  p <- as.character(xml_child_attrs(node, "position"))
  if (inherits(xml_child(node, "spread"), "xml_missing")){
    subfields <- 0
    width <- 0
  } else {
    subfields <- xml_child_attr(node, "spread", "subfields")
    if ("width" %in% xml_attrs(node, "spread")){
      width   <- xml_child_attr(node, "spread", "width")
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
  fastdf(list(
      ident      = as.character(xml_attr(node, "ident")),
      type       = as.character(xml_attr(node, "type")),
      name       = as.character(xml_contents(xml_child(node, "name")))[1],
      label      = as.character(xml_contents(xml_child(node, "label")))[1],
      positionStart  = as.character(pfrom),
      positionFinish = as.character(pto),
      subfields = subfields,
      width     = width,
      hasValues = inherits(xml_child(node, "values"), "xml_node")
  ))
}


# Reads all "codes" inside the triple-s "record". 
#
# This function parses the record node and extracts all "codes" nodes into a data.frame
#
# @param x XML node
# @keywords internal
getSSScodes <- function(x){
  if (inherits(xml_child(x, "values"), "xml_missing")){
    df <- data.frame(
        ident      = as.character(xml_attrs(x)["ident"]),
        code       = NA,
        codevalues = NA,
        stringsAsFactors = FALSE
    )
  } else {
    xx <- xml2::xml_find_all(x, "values/value")
    size <- length(xx)
    df <- data.frame(
        ident      = rep(unname(xml_attr(x, "ident")), size),
        code       = as.character(xml_attrs(xx)),
        codevalues = as.character(xml_contents(xx)),
        stringsAsFactors = FALSE
    )
    df <- df[df$codevalues != "character(0)", ]
  }
  
  df
}


# Splits sss metadata into multiple lines 
#
# This is necessary when the variable type is "multiple"
#
# @param sss data.frame containing .sss metadata
# @param sep character vector defining the string that separates field and subfield names
# @keywords internal
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

# Splits a data.frame into multiple lines 
#
# When length is greater than 1, duplicates that row
#
# @param df data.frame with .sss metadata
# @param n Numeric vector that indicates the number of times each row must be repeated
# @param sep character vector defining the string that separates field and subfield names
# @keywords internal
splitMultiple <- function(df, n, sep="_"){
  ret <- df[repN(n), ]
  row.names(ret) <- namesMultiple(row.names(df), n, sep)
  ret
}

# Repeats each element n times 
#
# Each element is repeated n times.  This is used to construct a vector of the new length after accounting for fields of type multiple 
#
# @param n Numeric vector that indicates the number of times each row must be repeated
# @keywords internal
repN <- function(n){
  rep(seq_along(n), times=pmax(1, n))
}

# Reads all "codes" inside the triple-s "record" 
#
# This function parses the record node and extracts all "codes" nodes into a data.frame
#
# @param x XML node
# @keywords internal
namesMultiple <- function(names, length, sep="_"){
  xl <- rep(names, times=pmax(1, length))
  sm <- rep(length<=1, times=pmax(1, length))
  xr <- paste(sep, sequence(pmax(1, length)), sep="")
  xr[sm] <- ""
  paste(xl, xr, sep="")
}






