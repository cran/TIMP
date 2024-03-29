\name{efit2file}
\alias{efit2file}
\title{Convert 'tim' FORTRAN efit files to plain matrices in ASCII files}
\description{'tim' efit files sometimes represent spectra associated
  with multiple datasets; for each matrix of spectra stored in such a
  file, this function writes a plain text file.
}
\usage{
efit2file(filename, skip = 2, numcol, nrows=vector())
}
\arguments{
\item{filename}{This is the path to the file to read in, as a quoted string. }
\item{skip}{number of lines at the top of the file before the data begins}
\item{numcol}{number of columns the data}
\item{nrows}{a vector saying how many rows are in each of the matrices
  of spectra in the file; for instance \code{nrows = c(256,256,256)}
  would indicate that the file stores spectra associated with 3
  datasets, each of which contains 256 wavelengths.  If \code{nrows} is
  not given, then a single file containing all data is written. }
}
\value{No return value, called for side effects}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{ \code{\link{readData}} }
\keyword{file}

