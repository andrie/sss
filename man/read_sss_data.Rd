\name{read_sss_data}
\alias{read_sss_data}
\title{Reads a triple-s XML (asc) data file, as specified by the triple-s XML standard.}
\usage{read_sss_data(data_filename)}
\description{Reads a triple-s XML (asc) data file, as specified by the triple-s XML standard.}
\details{This function reads and parses a .sss XML metadata file as well as its associated .asc data file. The .sss standard defines a standard survey structure}
\seealso{\code{\link{read_sss}}, \code{\link{read_sss_metadata}}}
\keyword{parse}
\arguments{\item{data_filename}{Name of .asc file containing the survey metadata}}
\examples{# Not executed
# read_sss_data("sample.asc")}
