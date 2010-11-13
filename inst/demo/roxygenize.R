# 
# Author: Andrie
###############################################################################

library(roxygen)
package.dir <- "F:\\My Dropbox\\eclipse projects\\sss\\"
roxygen.dir <- "F:\\My Dropbox\\eclipse projects\\sss\\man\\"

roxygenize(package.dir, roxygen.dir, copy.package=FALSE)

rm(package.dir, roxygen.dir)


