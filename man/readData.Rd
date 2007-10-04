\name{readData}
\alias{readData}
\title{This function reads in data the ivo file format}
\description{Data in the formats described at 
\url{http://www.nat.vu.nl/~kate/TIM/tim/node74.html} and 
\url{http://www.nat.vu.nl/~kate/FLIM_format}
may be read from file into an R object for analysis.  
}
\usage{
readData(filenm, sep = "")
}
\arguments{
  \item{filenm}{ This is the path to the file to read in, as a quoted
string. }
  \item{sep}{This is an optional argument describing how the data is 
delimited; defaults to \code{""}}
}
\value{
an object of class \code{dat}
}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{ \code{\link{preProcess}} }
\keyword{file}

