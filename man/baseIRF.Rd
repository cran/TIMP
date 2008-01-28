\name{baseIRF}
\alias{baseIRF}
\title{Baseline subtraction from a vector, usually representing an IRF.}
\description{Baseline subtraction from a vector, usually representing an IRF.
}
\usage{
baseIRF(irfvec, indexlow, indexhigh, removeNeg = FALSE) 
}
\arguments{
  \item{irfvec}{Vector to subtract a baseline from}
  \item{indexlow}{Lowest index to base the baseline estimation on}
   \item{indexhigh}{Highest index to base the baseline estimation on}
 \item{removeNeg}{Whether negative values should be replaced with 0.}
}
\details{
	Currently estimates the baseline as the mean of data between 
	indexlow and indexhigh, and subtracts the result from the 
	entire vector.
}
\value{vector}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\examples{ 
irfvec <- rnorm(128, mean=1) 
plot(irfvec,type="l") 
irfvec_corrected <- baseIRF(irfvec, 1, 10)
lines(irfvec_corrected, col=2)
}
 
\keyword{file}

