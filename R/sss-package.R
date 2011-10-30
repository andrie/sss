#' Tools for importing files in the triple-s .(Standard Survey Structure) format.
#'
#' sss is a set of tools to import survey files in the .sss (triple-s) format.  triple-s is a standard to transfer survey data between applications.
#' @references http://www.triple-s.org/
#'
#' The most important exported function is:
#' \code{\link{read.sss}}
#'
#' @docType package
#' @name sss-package
#' @import plyr
#' @import XML
#' @aliases sss sss-package
#' @keywords package
NULL

.onLoad <- function(libname, pkgname){
  message("The sss package is in early stages of development and still considered experimental.")
}  

