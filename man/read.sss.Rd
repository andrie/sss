\name{read.sss}
\alias{read.sss}
\title{Reads a triple-s XML (asc) data file, as specified by the triple-s XML standard.}
\usage{
  read.sss(sssFilename, ascFilename, sep = "_")
}
\arguments{
  \item{sssFilename}{Name of .sss file containing the
  survey metadata}

  \item{ascFilename}{Name of .asc file containing survey
  data}

  \item{sep}{character vector defining the string that
  separates field and subfield names}
}
\description{
  This function reads and parses a .sss XML metadata file
  as well as its associated .asc data file. The .sss
  standard defines a standard survey structure
}
\examples{
# Not executed
# read.sss("sample.sss, sample.asc")
}
\keyword{read}

