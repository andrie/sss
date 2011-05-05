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

expected_names <- c("Q1", "Q2_1", "Q2_2", "Q2_3", "Q2_4", "Q2_5", "Q2_6", "Q2_7", 
    "Q2_8", "Q2_9", "Q3", "Q4_1", "Q4_2", "Q5", "Q6", "Q7", "Q99")
expected_names2 <- c("Q1", "Q2.1", "Q2.2", "Q2.3", "Q2.4", "Q2.5", "Q2.6", "Q2.7", 
    "Q2.8", "Q2.9", "Q3", "Q4.1", "Q4.2", "Q5", "Q6", "Q7", "Q99")

path <- file.path("F:", "git", "sss", "sss", "_dev")
filename_sss <- file.path(path, "sample.sss")
filename_asc <- file.path(path, "sample.asc")


test_that("parsing of .sss and .asc works", {
	d <- read_sss(filename_sss, filename_asc)
			
	expect_that(d, is_a("data.frame"))
	expect_that(nrow(d), equals(3))
	expect_that(ncol(d), equals(17))
  expect_that(names(d), equals(expected_names))
})

test_that("separator parameter works", {
      d <- read_sss(filename_sss, filename_asc, sep=".")
      
      expect_that(names(d), equals(expected_names2))
    })


