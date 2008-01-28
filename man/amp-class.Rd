\name{amp-class}
\docType{class}
\alias{amp-class}
\alias{amp}
\title{Class "amp" for diagonal matrix model specification.}
\description{ 
\code{amp} is the class for diagonal matrix model specification;
such models are internally initialized when a tri-linear-type 
model is fit to the data via passing the argument \code{opt}
to \code{fitModel} as an object of class \code{opt} in which
the slot \code{trilinear} has the value \code{TRUE}.
 All objects of class \code{amp} are sub-classes of 
class \code{dat}; see documentation for \code{dat} 
for a description of 
these slots.  }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("amp", ...)} or 
 \code{amp(...)}.
}
\section{Slots}{
	 \describe{
	\item{amp}{ list of vectors of starting values for the parameters
	of the amplitudes for each dataset; one vector of values is
	used to 
	parameterize 
	the values corresponding to each dataset. 
    }
   }
}

\section{Extends}{
Class \code{\link{kin-class}}, directly.
}
\details{See \code{\link{kin-class}} for an 
example of the initialization of a 
\code{kin} object via the \code{initModel} function. } 
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{
\code{\link{kin-class}}, \code{\link{spec-class}}, \code{\link{opt-class}}
}
\keyword{classes}
