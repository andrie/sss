# Test sss integration
#
# Author: Andrie
#------------------------------------------------------------------------------


if(interactive()) library(testthat)
sampleRoot <- system.file("sample_data/sample-2", package = "sss")
filenameSSS <- file.path(sampleRoot, "survey_567311_20160213-200520_triples.sss")
filenameASC <- file.path(sampleRoot, "survey_567311_20160213-200520_triples.dat")
file.exists(filenameSSS)
file.exists(filenameASC)


#------------------------------------------------------------------------------

context("sample 2")

test_that("parsing of .sss and .asc works", {
  test <- read.sss(filenameSSS, filenameASC)
  expect_is(test, "data.frame")
  expect_equal(nrow(test), 694)
  expect_equal(ncol(test), 200)
})

