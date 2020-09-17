# Test sss integration
#
# Author: Andrie
#------------------------------------------------------------------------------


sampleRoot <- system.file("sampledata", package = "sss")
filenameSSS <- file.path(sampleRoot, "sample-3.sss")
filenameASC <- file.path(sampleRoot, "sample-3.dat")
file.exists(filenameSSS)
file.exists(filenameASC)


#------------------------------------------------------------------------------



test_that("parsing of .sss and .asc works", {
  test <- read.sss(filenameSSS, filenameASC)
  expect_s3_class(test,  "data.frame")
  expect_equal(nrow(test), 98)
  expect_equal(ncol(test), 200)
})
