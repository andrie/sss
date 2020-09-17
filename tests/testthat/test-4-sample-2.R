
if(interactive()) library(testthat)
sampleRoot <- system.file("sampledata", package = "sss")
filenameSSS <- file.path(sampleRoot, "sample-2.sss")
filenameASC <- file.path(sampleRoot, "sample-2.csv")

#------------------------------------------------------------------------------



test_that("parsing of .sss and .asc works", {
	test <- read.sss(filenameSSS, filenameASC)
	expect_s3_class(test,  "data.frame")
})

test_that("parsing works when you only provide the .sss", {
  expect_equal(get_sss_format(filenameSSS), ".csv")
  test <- read.sss(filenameSSS)
  expect_s3_class(test,  "data.frame")
  
  test_2 <- read.sss(filenameSSS, filenameASC)
  expect_equal(test, test_2)
  
})
