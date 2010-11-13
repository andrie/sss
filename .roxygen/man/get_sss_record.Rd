\name{get_sss_record}
\alias{get_sss_record}
\title{Reads all "variables" inside the triple-s "record"...}
\usage{get_sss_record(x)}
\description{Reads all "variables" inside the triple-s "record"}
\details{This function parses the record node, extracts all variables
and creates a data frame with information about size, position, type, etc.}
\keyword{XML}
\arguments{\item{x}{XML node}}
\examples{get_sss_record(xmlRoot(doc)[["survey"]][["record"]])}
