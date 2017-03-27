sampleRoot <- system.file("sample_data/sample-0", package = "sss")
filenameSSS <- file.path(sampleRoot, "sample.sss")
filenameASC <- file.path(sampleRoot, "sample.asc")

read.sss(filenameSSS, filenameASC)

readSSSdata(filenameSSS)
readSSSmetadata(filenameSSS)
