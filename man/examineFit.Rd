\name{examineFit}
\alias{examineFit}
\title{Examines the results of a call to  fitModel}
\description{Examine the results of a call to  \code{\link{fitModel}} 
by a call to plotting functions; call this function with argument 
an object returned from \code{fitModel}.  Possibly also supply a new 
specification of plots to be generated. }
\usage{
examineFit(resultfitModel, opt=vector())
}
\arguments{
  \item{resultfitModel}{list returned by a call to  \code{\link{fitModel}}}
  \item{opt}{possibly an object of class \code{opt} giving options for 
  plotting; if \code{opt} has length zero (the default) then the plotting
  options given in the \code{opt} list of \code{resultFitModel} are 
  applied}
}
\details{
The  \code{\link{fitModel}} function returns a list of results, and initiates plotting
functions.  Given the \code{resultfitModel} list  \code{\link{fitModel}} returns, 
\code{examineFit} initiates the plotting functions, and thus may be 
used to examine results.  

}
\seealso{ \code{\link{fitModel}}, \code{\link{opt} }}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\keyword{file }

