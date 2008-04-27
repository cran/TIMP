\name{preProcess}
\alias{preProcess}
\title{Performs preprocessing on data stored as an objects of class dat.}
\description{Performs data sampling, selection, baseline correction,  
scaling, and data correction on an object of class \code{dat}. 
}
\usage{
preProcess(data, sample = 1, sample_time = 1, sample_lambda = 1, 
    sel_time = vector(), sel_lambda = vector(), baselinetime = vector(), 
    baselinelambda = vector(), scalx = NULL, scalx2 = NULL, 
    sel_lambda_ab = vector(), sel_time_ab = vector(), rm_x2=vector(), 
    rm_x = vector(), svdResid = list(), numV = 0, sel_special = list())
}
\arguments{
  \item{data}{Object of class \code{dat}}
  \item{sample}{integer describing sampling interval to take in both time and 
  \code{x2}; e.g., \code{sample=2} 
  will sample every 2nd time and every 2nd point in 
   \code{x2}.}
  \item{sample_time}{integer describing sampling interval in time; e.g.,
  \code{sample_time=2} will sample every 2nd element of the time vector. }
  \item{sample_lambda}{ integer describing sampling interval in \code{x2}; 
  e.g., \code{sample_lambda=2} will sample every 2nd element in the 
  \code{x2} vector. }
  \item{sel_time}{vector of length 2 describing the first and last time 
  index of data to select; e.g., \code{sel_time=c(5,120)} will select 
  data at times indexed 5-120. }
  \item{sel_lambda}{vector of length 2 describing the first and last \code{x2}  
  index of data to select; e.g., \code{sel_lambda=c(5,120)} 
  will select data at \code{x2}
  indexed 5-120. }
  \item{baselinetime}{a vector of form \code{c(timeIndexmin, timeIndexmax, 
  lambdaIndexmin, lambdaIndexmax)}.  The average of data between 
   \code{x2} indexes  
   \code{lambdaIndexmin} and \code{lambdaIndexmax} 
   is subtracted from data with 
   time index between \code{timeIndexmin} and \code{timeIndexmax}.  }
  \item{baselinelambda}{  a vector of form \code{c(timeIndexmin, timeIndexmax, 
  lambdaIndexmin, lambdaIndexmax)}.  The average of data between time indexes  
  \code{timeIndexmin} and \code{timeIndexmax} 
  is subtracted from data with \code{x2} index 
   between \code{lambdaIndexmin} and \code{lambdaIndexmax}.}
  \item{scalx}{numeric by which to linearly scale the \code{x} axis
  (which often represents time), so that newx = oldx * scalx }
  \item{scalx2}{ vector of length 2 by which to linearly scale the 
  \code{x2} axis, so that newx2 = oldx2 * scalx2[1] + scalx2[2]}
  \item{sel_lambda_ab}{vector of length 2 describing the absolute values
  (e.g., wavelengths, wavenumbers, etc.) between which data should be
  selected.   e.g., \code{sel_lambda_ab = c(400, 600)} will select data  
  associated with \code{x2} values between 400 and 600. } 
  \item{sel_time_ab}{ vector of length 2 describing the absolute times 
  between which data should be
  selected.   e.g., \code{sel_time_ab = c(50, 5000)} will select data  
  associated with time values between 50 and 5000 picoseconds.} 
  \item{rm_x2}{vector of \code{x2} indices to remove from the data} 
 \item{rm_x}{vector of \code{x} indices to remove from the data}
  \item{svdResid}{list returned from the \code{getResid} function, containing
  residuals to be used in data correction. } 
 \item{numV}{numeric specifying how many singular vectors to use in data
   correction.  Maximum is five. }
 \item{sel_special}{list of lists specifying \code{x} indices to remove
   from individual wavelength ranges, e.g.,
   \code{sel_special = list(c(400,600,10,12),c(600,800,11,13))}
 indicates that between wavelength 400 and 600, time indices between 10
 and 12 should be removed from the data, and between wavelengths 600 and
800, time indices between 11 and 13 should be removed from the data.
Note that the number of time indices to remove should be the same in
each wavelength interval specified.  Also note that the time vector
associated with the data after the first set of indices is removed will
be associated with the resulting dataset. }
}  
\value{
object of class \code{dat}. 
}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{ \code{\link{readData}}, \code{\link{getResid} }}
\keyword{file} 
