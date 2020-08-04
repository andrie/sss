# sss internal - manipulate asc data
# 
# Author: Andrie
#------------------------------------------------------------------------------


#' Applies coded values to asc data, as described in sss metadata.  
#'
#' @param sss Parsed .sss metadata information
#' @param df Parsed .asc data
#' @keywords internal
changeValues <- function (sss, df){
  col.names <- names(df)
  whichHasValues <- which(with(sss$variables, 
          (hasValues & !type %in% c("multiple", "quantity")) |
          (hasValues & type == "multiple" & subfields != "0")
  ))
          
  changeSingleValue <- function(i){
    if(i %in% whichHasValues){
      codeFrame <- sss$codes[sss$codes$ident == sss$variables$ident[i], 
                             c("code", "codevalues")]
      codeFrame <- rbind(codeFrame, c(0, NA), c(" ", NA))
      codeFrame$codevalues[match(as.character(df[, i]), 
                                 as.character(codeFrame$code))]
    } else {
      df[, i]
    }
  }
  
  df <- fastdf(lapply(seq_along(df), changeSingleValue))
  names(df) <- col.names  
  df
}

#' Assigns question text to variable.labels attribute.
#' 
#' @inheritParams changeValues
#' @keywords internal
addQtext <- function(sss, df){
  attr(df, "variable.labels") <- sss$variables$label
  df
}


