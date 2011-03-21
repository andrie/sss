\name{read_sss}
\alias{read_sss}
\title{Reads and processes a triple-s files (in sss and asc formats).}
\usage{read_sss(sss_filename, data_filename)}
\description{Reads and processes a triple-s files (in sss and asc formats).}
\details{This function reads and parses a .sss XML metadata file as well as its associated .asc data file. The .sss standard defines a standard survey structure}
\keyword{read}
\arguments{\item{sss_filename}{Name of .sss file containing the survey metadata}
\item{data_filename}{Name of .asc file containing survey data}}
\examples{# Not executed
# read_sss("sample.sss, sample.asc")}
