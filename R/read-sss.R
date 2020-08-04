#' Reads a triple-s XML (asc) data file, as specified by the triple-s XML
#' standard.
#'
#' This function reads and parses a `.sss` XML metadata file as well as its
#' associated `.asc` data file. The sss standard defines a standard survey
#' structure.
#'
#' @param sssFilename Character string: name of .sss file containing the survey
#'   metadata
#'   
#' @param ascFilename Character string: name of .asc (or .csv) file containing survey data.  If this is not provided, guesses the filename using 
#' 
#' @param sep Character vector defining the string that separates question and
#'   subquestion labels, e.g. `c("Q_1", "Q_2")`
#'   
#' @return A data frame with one element (column) for each variable in the data
#' set.
#'
#' The resulting  data.frame contains several attributes:
#'
#' * `variable.labels`: a named list of value labels with one element per
#' variable, either NULL or a named character vector
#'
#' * `label.table`: a named list with one element per question. Every element is
#' a named character string that contains the label codes for that question.
#'
#' @family read functions
#' @importFrom stats setNames
#' @references http://www.triple-s.org/
#' @export
#' @example inst/examples/example-read-sss.R
read.sss <- function(sssFilename, ascFilename = guess_asc_filename(sssFilename), sep = "_"){
  assert_that(is.character(sssFilename))
  assert_that(file.exists(sssFilename))
  
  assert_that(is.character(ascFilename))
  assert_that(file.exists(ascFilename))
  
  message("Reading SSS metadata")
  switch(
    class(sssFilename),
    "character" = {
      doc <- readSSSmetadata(sssFilename)
      sss <- parseSSSmetadata(doc)
    }, 
    "XMLDocumentContent" = {
      sss <- parseSSSmetadata(sssFilename)
    }, 
    stop("SSSfilename not recognised as either a file or an XML object")
  )
  
  sss$variables <- splitSSS(sss$variable, sep)
  
  message("Reading SSS data")

  ascWidth <- sss$variables$colWidth
  
  types <- c(single = "character",
              multiple = "character",
              character = "character", 
              logical = "logical",
              numeric = "numeric", 
              quantity = "numeric",
              date = "Date"
              )
  ascType <- types[sss$variables$type]
  idx <- sss$variables$type == "multiple"
  ascType[idx] <- "numeric"
  idx <- sss$variables$type == "multiple" & sss$variables$subfields > 0
  ascType[idx] <- "character"
  
  ascNames <- sss$variables$name
  
  dat <- switch(
    sss$format, 
    csv = 
      read.csv(
        file = ascFilename,
        skip = sss$skip,
        header = FALSE,
        col.names = ascNames,
        colClasses = "character",
        stringsAsFactors = FALSE
      ),
    fixed = 
      fast.read.fwf(
        file = ascFilename, 
        widths = ascWidth, 
        colClasses = ascType, 
        col.names = ascNames
      )
  )
  dat <- changeValues(sss, dat)
  dat <- addQtext(sss, dat)
  labelTabelData <- split(sss$codes, f = sss$codes$ident)
  labelTable <- lapply(labelTabelData, 
                       function(x)setNames(x[["code"]], x[["codevalues"]])
                       )
  attr(dat, "label.table") <- labelTable
  dat
}
