sampleRoot <- system.file("sampledata", package = "sss")
filenameSSS <- file.path(sampleRoot, "sample-1.sss")
filenameASC <- file.path(sampleRoot, "sample-1.asc")

readSSSdata(filenameSSS)
readSSSmetadata(filenameSSS)
