\name{read_sss_metadata}
\alias{read_sss_metadata}
\title{Reads a triple-s XML (sss) metadata file, as specified by the triple-s XML standard.}
\usage{read_sss_metadata(sss_filename)}
\description{Reads a triple-s XML (sss) metadata file, as specified by the triple-s XML standard.}
\details{This function reads a .sss XML metadata file.  The .sss standard defines a standard survey structure}
\seealso{\code{\link{parse_sss_metadata}}, \code{\link{read_sss}}, \code{\link{read_sss_data}}}
\keyword{read}
\arguments{\item{sss_filename}{Name of .sss file containing the survey metadata}}
\examples{# Not executed
# read_sss_metadata("sample.sss")}
