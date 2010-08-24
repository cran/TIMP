\name{readclp0}
\alias{readclp0}
\title{This function reads in a specification of constraints to 
zero on the clp. }
\description{This function is useful for the case that there are 
many constraints to zero in the model, as is the case for some mass
spectrometry models. 
}
\usage{
readclp0(filenm)
}
\arguments{
  \item{filenm}{Object of class \code{"character"} that gives 
is the path to the file to read in. } 
}
\value{
The constraints to zero in the format documented in the help
file for the \code{"dat"} class.  Therefore a call to 
\code{"readclp0"} may be used inside a call to \code{"initModel"}, 
as in \code{clp0 = readclp0("filename")}.
}
\details{
The file to be read in should have the following format: 
1st line is not read.  Lines thereafter are 
the space-delimited index of the component to constrain, the 
lower bound of the constraint, and the upper bound of the 
constraint, e.g., \code{1 218.800000000000011 220.099999999999994}.
}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{ \code{\link{initModel}} }
\keyword{file}

