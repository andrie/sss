# 
# Author: Andrie
###############################################################################

library(XML)
library(plyr)






###############################################################################

path <- "F:\\My Dropbox\\eclipse projects\\sss\\"

source(paste(path, "R\\public_functions.r", sep=""))
source(paste(path, "R\\internal_functions.r", sep=""))

filename_sss <- "data\\sample.sss"
filename_asc <- "data\\sample.asc"

# Following two lines for debugging only
sss <- read_sss_metadata(paste(path, filename_sss, sep=""))
asc <- read_sss_data(paste(path, filename_asc, sep=""))


df <- read_sss(paste(path, filename_sss, sep=""), paste(path, filename_asc, sep=""))
print(df)
print(str(df))

#change_values(sss, df, 1)
#print(sss$codes)
#llply_colwise(sss, df, change_values)







