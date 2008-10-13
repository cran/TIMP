\name{getResid}
\alias{getResid}
\title{For data correction, fits a model (but ignores 
plotting commands) in order to obtain the SVD of the residuals, which 
then can be used in data-correction. }
\description{For data correction, fits a model exactly as does 
\code{fitModel} (but ignores 
plotting commands) in order to obtain the SVD of the residuals.  These 
residuals can then be subtracted away from the original data to some
extent with the \code{preProcess} function. }
\usage{
getResid(data, modspec=list(), datasetind = vector(),
     modeldiffs = list(), opt = opt() )
}
\arguments{
 \item{data}{As in the \code{fitModel} function }
 \item{modspec}{ As in the \code{fitModel} function}
\item{datasetind}{ As in the \code{fitModel} function}
\item{modeldiffs}{ As in the \code{fitModel} function}
\item{opt}{ As in the \code{fitModel} function}
 }

\value{list containing the first five left and right singular vectors of the 
residuals, as well as the first five singular values.  A weight matrix 
(if used) is also included in this list. 
}
\seealso{ \code{\link{fitModel}}, \code{\link{preProcess}}}
\keyword{file}
