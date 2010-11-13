# TODO: Add comment
# 
# Author: Andrie
###############################################################################


path <- "F:\\My Dropbox\\PentaLibra\\R\\triple-s\\"
filename_xml <- "sample.sss"

doc <- xmlTreeParse(paste(path, filename_xml, sep=""), getDTD = F)
r <- xmlRoot(doc)[["survey"]][["record"]]


get_sss_record(r[[2]])
get_sss_codes(r[[2]])

dfr <- ldply(xmlChildren(r), get_sss_record)
dfr

dfc <- ldply(xmlChildren(r), get_sss_codes)
dfc


