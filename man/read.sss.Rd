\name{read.sss}
\alias{read.sss}
\title{Reads a triple-s XML (asc) data file, as specified by the triple-s XML standard.}
\usage{
  read.sss(sssFilename, ascFilename, sep = "_")
}
\arguments{
  \item{sssFilename}{Character string: name of .sss file
  containing the survey metadata}

  \item{ascFilename}{Character string: name of .asc file
  containing survey data}

  \item{sep}{Character vector defining the string that
  separates question and subquestion labels, e.g.
  \code{c("Q_1", "Q_2")}}
}
\value{
  A data frame with one element (column) for each variable
  in the data set. The data.frame contains several
  attributes:

  \describe{ \item{variable.labels}{a named list of value
  labels with one element per variable, either NULL or a
  names character vector} }
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
\references{
  http://www.triple-s.org/
}
\keyword{read}

