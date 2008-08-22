\name{writeAverage}
\alias{writeAverage}
\title{Writes the average of scans stored in a file to a new file in the
'ivo' format}
\description{Some measurement set-ups dump a set of matrices stacked on
  top of each other to a file; each matrix represents a scan.  This
  function writes the average of the scans to a file in the '.ivo'
  format.
}
\usage{
writeAverage(filename, ntimes, nwave, scans,
             fileout = paste(filename, "Average.ivo", sep=""),
             calibration = 1:nwave, wexplicit=FALSE)
}
\arguments{
\item{filename}{This is the path to the file to read in, as a quoted string. }
\item{ntimes}{number of times in each scan}
\item{nwave}{number of wavelengths in each scan}
\item{scans}{number of full scans to read}
\item{fileout}{a character vector specifying the filename to write
             the averaged data to; the default is to write a file
             named "filenameAverage.ivo"}
\item{calibration}{a numeric vector representing the wavelength labels; by
                   default the labels "1, 2, ..., nwave" are used}
 \item{wexplicit}{logical whether the file is written in the
   'wavelength explicit' format, with each column of the
   matrix written representing a wavelength, as opposed to the
   'time explicit' format, where each column represents a timepoint. 
 }
}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{ \code{\link{readData}} }
\keyword{file}

