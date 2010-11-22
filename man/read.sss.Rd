\name{read.sss}
\alias{read.sss}
\title{A wrapper around read_sss...}
\usage{read.sss(sss_filename, data_filename)}
\description{A wrapper around read_sss}
\details{This function reads and parses a .sss XML metadata file as well as its
associated .asc data file. 
The .sss standard defines a standard survey structure}
\keyword{triple-s}
\seealso{read_sss}
\arguments{\item{sss_filename}{Name of .sss file containing the survey metadata}
\item{data_filename}{Name of .asc file containing survey data}}
\examples{# Not executed
# read.sss("sample.sss, sample.asc")}
