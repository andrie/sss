\name{fast.read.fwf}
\alias{fast.read.fwf}
\title{Read in fixed-width files quickly.}
\usage{
  fast.read.fwf(file, widths, col.names = NULL, colClasses
  = NA, tz = "", dec = ".", ...)
}
\arguments{
  \item{file}{character}

  \item{widths}{vector of column widths. Negative numbers
  mean "skip this many columns". Use an NA as the final
  element if there are likely to be extra characters at the
  end of each row after the last one that you're interested
  in.}

  \item{col.names}{names for the columns that are NOT
  skipped}

  \item{colClasses}{can be used to control type conversion;
  see read.table. It is an optional vector whose names must
  be part of col.names. There is one extension of the
  read.table rules:a colClass string starting POSIXct. will
  trigger automatic conversion to POSIXct, using the rest
  of the string as the format specifier. See also tz.}

  \item{tz}{used in auto-conversion to POSIXct when
  colClass is set}

  \item{...}{ignored; it's here so that this function can
  be called just like read.fwf}

  \item{dec}{Decimal period}
}
\description{
  Experimental replacement for read.fwf that runs much
  faster.
}
\note{
  Original code from package mvbutils
}

