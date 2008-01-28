\name{mass-class}
\docType{class}
\alias{mass-class}
\alias{mass}
\title{Class "mass" for mass spectrometry model storage.}
\description{ 
\code{mass} is the class for mass spectrometry models; an object 
of class "mass" is initialized if 
\code{mod_type = "mass"} is an 
argument of \code{initModel}.
All objects of class \code{mass} are sub-classes of 
class \code{kin}; see documentation for \code{kin} 
for a description of 
these slots.  }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("mass", ...)} or 
 \code{kin(...)}.
}
\section{Slots}{
	 \describe{
	\item{peakpar}{ list of vectors of starting values for the parameters
	of components; one vector of values is used to parameterize 
	each component. 
    }
    \item{\code{peakfunct}:}{ Object of class \code{"character"}  
that specifies the function by which components are parameterized in time; 
this is by default "expmodgaus" for the exponentially modified Gaussian 
function.}
    \item{\code{lzerofile}:}{ Object of class \code{"character"}  that 
specifies the filename of the lzero specification to read in from file.  This
file has the format: 1st line not read; lines thereafter 
are the space-delimited index of the component to constrain, the 
lower bound of the constraint, and the upper bound of the 
constraint, e.g., \code{1 218.800000000000011 220.099999999999994}
}
 \item{\code{extracomp}:}{ Object of class \code{"logical"}  that
defaults to \code{TRUE} and determines whether a component with 
constant concentration in time is added to the model to represent 
a baseline.}  
 \item{\code{shift}:}{ Object of class \code{"vector"} that represents
a shift of the location of each elution profile peak; this can be
specified per-component, in which case \code{length(shift)} is the 
number of components (not including a baseline component) or for
all components, in which case \code{length(shift == 1)}. 
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
\code{\link{kin-class}}, \code{\link{spec-class}}
}
\keyword{classes}
