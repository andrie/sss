# This file contains test scripts, using the testthat package, developed by 
# Hadley Wickham:
# https://github.com/hadley/devtools/wiki/Testing
# 
# Author: Andrie
###############################################################################



test_that("parsing of .sss and .asc works", {
	path <- "F:\\git\\sss\\data\\"
	filename_sss <- paste(path, "sample.sss", sep="")
	filename_asc <- paste(path, "sample.asc", sep="")
	d <- read_sss(filename_sss, filename_asc)
			
	expect_that(d, is_a("data.frame"))
	expect_that(nrow(d), is_equal(3))
	expect_that(ncol(d), is_equal(8))
})


