# Test sss integration
#
# Author: Andrie
#------------------------------------------------------------------------------


if(interactive()) library(testthat)
sampleRoot <- system.file("sampledata", package = "sss")
filenameSSS <- file.path(sampleRoot, "sample-2.sss")
filenameASC <- file.path(sampleRoot, "sample-2.csv")

readSSSmetadata(filenameSSS)
file.exists(filenameSSS)

#------------------------------------------------------------------------------

context("sample 2")

test_that("parsing of .sss and .asc works", {
	test <- read.sss(filenameSSS, filenameASC)
	expect_is(test, "data.frame")
})


test_that("parsing works when you only provide the .sss", {
  test <- read.sss(filenameSSS)
  expect_is(test, "data.frame")
})
