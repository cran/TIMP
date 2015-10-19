\name{readData}
\alias{readData}
\title{This function reads in data the ivo file format}
\description{Data in the formats described at 
\url{http://glotaran.org/wiki/doku.php?id=file_formats}
% \url{http://www.nat.vu.nl/~kate/TIM/tim/node74.html} and 
% \url{http://www.nat.vu.nl/~kate/FLIM_format}
may be read from file into an R object for analysis.  
}
\usage{
readData(filenm, typ="", sep = "")
}
\arguments{
  \item{filenm}{ This is the path to the file to read in, as a quoted
string. }
 \item{typ}{if \code{typ="plain"} the the file being read in stores 
data as a plain matrix, with \code{x} values as the first element of 
each row except the first and \code{x2} values as the first row. }
  \item{sep}{This is an optional argument describing how the data is 
delimited; defaults to \code{""}}
}
\value{
an object of class \code{dat}
}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{ \code{\link{preProcess}} }
\keyword{file}

