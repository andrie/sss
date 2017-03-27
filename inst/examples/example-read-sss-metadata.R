sampleRoot <- system.file("sample_data/sample-0", package = "sss")
filenameSSS <- file.path(sampleRoot, "sample.sss")
filenameASC <- file.path(sampleRoot, "sample.asc")

readSSSdata(filenameSSS)
readSSSmetadata(filenameSSS)
