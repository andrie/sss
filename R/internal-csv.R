# sss internal - manipulate asc data
# 
# Author: Andrie
#------------------------------------------------------------------------------


# Applies coded values to asc data, as described in sss metadata.  
#
# @param sss Parsed .sss metadata information
# @param df Parsed .asc data
# @keywords internal
changeValues <- function (sss, df){
  col.names <- names(df)
  whichHasValues <- which(with(sss$variables, 
          (hasValues & !type %in% c("multiple", "quantity")) |
          (hasValues & type=="multiple" & subfields!="0")
  ))
          
  changeSingleValue <- function(i){
    if(i %in% whichHasValues){
      code_frame <- sss$codes[sss$codes$ident==sss$variables$ident[i], c("code", "codevalues")]
      code_frame <- rbind(code_frame, c(0, NA), c(" ", NA))
      code_frame$codevalues[match(as.character(df[, i]), as.character(code_frame$code))]
    } else {
      df[, i]
    }
  }
  
  df <- fastdf(lapply(seq_along(df), changeSingleValue))
  names(df) <- col.names  
  df
}

# Assigns question text to variable.labels attribute.
# 
# @inheritParams changeValues
# @keywords internal
addQtext <- function(sss, df){
  attr(df, "variable.labels") <- sss$variables$label
  df
}


# Take input sss filename and replace the .sss with .asc
guessAsc <- function(x, ext = ".asc"){
  file.path(
    dirname(x),
    paste0(gsub("\\..*$", "", basename(x)), ext)
  )
}


# Guess if format is csv or asc
getSSSformat <- function(x){
  y <- xml_child(readSSSmetadata(x), "survey/record")
  z <- xml_attr(y, "format")
  if(!is.na(z) && z == "csv") ".csv" else ".asc"
}


