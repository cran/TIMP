\name{denS4}
\alias{denS4}
\alias{denS5}
\docType{data}
\title{Time-resolved absorption data}
\description{Time-resolved absorption data measured at two different
  laser intensities}
\usage{
data("denS4")
data("denS5")
}
\format{
  \code{denS4} is an object of class \code{dat} representing absorption
  data.
  \code{denS5} is an object of class \code{dat} representing absorption
  data meausured at half the laser intensity as compared to the
  intensity used to measure \code{denS4}.
}
\references{This data was described in
Mullen KM, van Stokkum IHM (2007).  
TIMP: An R Package for Modeling Multi-way Spectroscopic
Measurements. \emph{Journal of Statistical Software}, \bold{18}(3), 
\url{http://www.jstatsoft.org/v18/i03/}.
}
\examples{
data("denS4")
image.plot(denS4@x, denS4@x2, denS4@psi.df)
}

\keyword{datasets}
