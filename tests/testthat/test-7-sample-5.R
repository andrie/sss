# Test sss integration
#
# Author: Andrie
#------------------------------------------------------------------------------


sampleRoot <- system.file("sampledata", package = "sss")
filenameSSS <- file.path(sampleRoot, "sample-5.sss")
filenameASC <- file.path(sampleRoot, "sample-5.asc")
file.exists(filenameSSS)
file.exists(filenameASC)



#------------------------------------------------------------------------------



test_that("parsing of .sss and .asc works", {
  test <- 
    read.sss(filenameSSS, filenameASC)
  
  expect_s3_class(test,  "data.frame")
  expect_equal(nrow(test), 10)
  expect_equal(ncol(test), 3)
  expect_equal(
    test$weekday, 
    paste0(
      c("Mon", "Tues", "Wednes", "Thurs", "Satur", "Fri", "Thurs", 
        "Wednes", "Thurs", "Sun"), 
      "day"
    )
  )
  expect_equal(test$foyer, 1:10)
  expect_equal(test$typtel, c("Fixed", "Mobile")[c(1,1,2,2,1,2,1,3,3,3)])
})
