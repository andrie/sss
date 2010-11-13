# This file contains test scripts, using the testthat package, developed by 
# Hadley Wickham:
# https://github.com/hadley/devtools/wiki/Testing
# 
# Author: Andrie
###############################################################################

library(testthat)

test_that("parsing of .sss and .asc works", {
	path <- "F:\\My Dropbox\\Eclipse projects\\sss\\"
	filename_sss <- "data\\sample.sss"
	filename_asc <- "data\\sample.asc"
	d <- read_sss(paste(path, filename_sss, sep=""), paste(path, filename_asc, sep=""))
			
	expect_that(d, is_a("data.frame"))	
})


#test_dir("inst/tests/", "minimal")
