# This file contains test scripts, using the testthat package, developed by 
# Hadley Wickham:
# https://github.com/hadley/devtools/wiki/Testing
# 
# Author: Andrie
###############################################################################



test_that("parsing of .sss and .asc works", {
	path <- file.path("F:", "git", "sss", "sss", "data")
	filename_sss <- file.path(path, "sample.sss")
	filename_asc <- file.path(path, "sample.asc")
	d <- read_sss(filename_sss, filename_asc)
			
	expect_that(d, is_a("data.frame"))
	expect_that(nrow(d), equals(3))
	expect_that(ncol(d), equals(12))
})


