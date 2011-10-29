\name{parseSSSmetadata}
\alias{parseSSSmetadata}
\title{Parses a triple-s XML (sss) metadata file, as specified by the triple-s XML standard.}
\usage{
  parseSSSmetadata(XMLdoc)
}
\arguments{
  \item{XMLdoc}{An XML document - as returned by
  \code{\link[XML]{xml}}, or \code{\link{readSSSmetadata}}}
}
\description{
  This function reads and parses a .sss XML metadata file
  as well as its associated .asc data file. The .sss
  standard defines a standard survey structure
}
\seealso{
  readSSSmetadata, read.sss, readSSSdata
}
\keyword{parse}

