sampleRoot <- system.file("data/sample-1", package = "sss")
filenameSSS <- file.path(sampleRoot, "sample.sss")
filenameASC <- file.path(sampleRoot, "sample.asc")

readSSSdata(filenameSSS)
readSSSmetadata(filenameSSS)
