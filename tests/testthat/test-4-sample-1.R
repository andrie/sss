# Test sss integration
#
# Author: Andrie
#------------------------------------------------------------------------------


if(interactive()) library(testthat)
sampleRoot <- system.file("data/sample-2", package = "sss")
filenameSSS <- file.path(sampleRoot, "recruitment_test.sss")
filenameASC <- file.path(sampleRoot, "recruitment_test.csv")
file.exists(filenameSSS)

#------------------------------------------------------------------------------

context("sample 2")

test_that("parsing of .sss and .asc works", {
	test <- read.sss(filenameSSS, filenameASC)
	expect_is(test, "data.frame")
})

