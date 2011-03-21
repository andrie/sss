# This file contains test scripts, using the testthat package, developed by 
# Hadley Wickham:
# https://github.com/hadley/devtools/wiki/Testing
# 
# Author: Andrie
###############################################################################

# See documentation of read.fwf for tips on how to create a temporary file
#ff <- tempfile()
#cat(file=ff, "123456", "987654", sep="\n")
#read.fwf(ff, widths=c(1,2,3))    #> 1 23 456 \ 9 87 654
#read.fwf(ff, widths=c(1,-2,3))   #> 1 456 \ 9 654
#unlink(ff)
#cat(file=ff, "123", "987654", sep="\n")
#read.fwf(ff, widths=c(1,0, 2,3))    #> 1 NA 23 NA \ 9 NA 87 654
#unlink(ff)
#cat(file=ff, "123456", "987654", sep="\n")
#read.fwf(ff, widths=list(c(1,0, 2,3), c(2,2,2))) #> 1 NA 23 456 98 76 54
#unlink(ff)




test_that("parsing of .sss and .asc works", {
	path <- file.path("F:", "git", "sss", "sss", "data")
	filename_sss <- file.path(path, "sample.sss")
	filename_asc <- file.path(path, "sample.asc")
	d <- read_sss(filename_sss, filename_asc)
			
	expect_that(d, is_a("data.frame"))
	expect_that(nrow(d), equals(3))
	expect_that(ncol(d), equals(19))
})


