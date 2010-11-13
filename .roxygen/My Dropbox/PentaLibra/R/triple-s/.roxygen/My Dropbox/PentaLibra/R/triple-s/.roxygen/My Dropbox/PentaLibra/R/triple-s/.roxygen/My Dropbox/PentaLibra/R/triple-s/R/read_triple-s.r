library(XML)
library(plyr)

path <- "F:\\My Dropbox\\PentaLibra\\R\\triple-s\\"
filename_xml <- "sample.sss"

doc <- xmlTreeParse(paste(path, filename_xml, sep=""), getDTD = F)
r <- xmlRoot(doc)[["survey"]][["record"]]

# What I want: A list with an entry for each record
# - ident
# - type (Single, multiple, character, quantity, logical)
# - name
# - label
# - position start
# - position finish
# - list of value codes
# - list of value descriptions

#' Reads all "variables" inside the triple-s "record" 
#'
#' This function parses the record node, extracts all variables
#' and creates a data frame with information about size, position, type, etc.
#'
#' @param x XML node
#' @keywords 
#' @export
#' @examples
#' get_sss_record(xmlRoot(doc)[["survey"]][["record"]])

get_sss_record <- function(x){
  p <- as.character(xmlAttrs (x[["position"]]))
  if (is.null(x[["spread"]])){
    subfields <- 0
    width <- 0
  } else {
    subfields <- xmlAttrs(x[["spread"]])[["subfields"]]
    if ("width" %in% xmlAttrs(x[["spread"]])){
      width   <- xmlAttrs(x[["spread"]])[["width"]]
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
    ident      = as.character(xmlAttrs (x)["ident"]),
    type       = as.character(xmlAttrs (x)["type"]),
    name       = as.character(xmlValue (x[["name"]])[1]),
    label      = as.character(xmlValue (x[["label"]])[1]),
    position_start  = as.character(pfrom),
    position_finish = as.character(pto),
    subfields = subfields,
    width     = width,
    stringsAsFactors = FALSE
  )
}

#' Reads all "variables" inside the triple-s "record" 
#'
#' This function parses the record node, extracts all variables
#' and creates a data frame with information about size, position, type, etc.
#'
#' @param x XML node
#' @keywords 
#' @export
#' @examples
#' get_sss_record(xmlRoot(doc)[["survey"]][["record"]])
get_sss_codes <- function(x){
  size <- xmlSize(x[["values"]])
  if (is.null(x[["values"]])){
    data.frame(
      ident      = as.character(xmlAttrs (x)["ident"]),
      code       = NA,
      codevalues = NA
    )
  } else {
    data.frame(
      ident      = rep(as.character(xmlAttrs (x)["ident"]), size),
      code       = as.character(xmlSApply(x[["values"]], xmlAttrs)),
      codevalues = as.character(xmlSApply(x[["values"]], xmlValue))
    )
  }
}


get_sss_record(r[[2]])
get_sss_codes(r[[2]])

dfr <- ldply(xmlChildren(r), get_sss_record)
dfr

dfc <- ldply(xmlChildren(r), get_sss_codes)
dfc



