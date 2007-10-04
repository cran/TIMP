\name{res-class}
\docType{class}
\alias{res-class}
\alias{res}
\title{Class "res" to store the results of model fitting associated with 
a single dataset. }
\description{	 Class to store results of model fitting associated with 
a single dataset.  A list containing objects of class \code{res} is 
a slot in class \code{fit}.  An object of class \code{fit} is stored in 
the slot \code{fit} of objects of class \code{multimodel}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("res", ...)}.
A \code{res} object is created after model fitting
via the residual function \code{residPart}.  
}
\section{Slots}{
	 \describe{
    \item{\code{cp}:}{Object of class \code{"list"} that contains the
    estimates for conditionally linear parameters. }
    \item{\code{resid}:}{Object of class \code{"list"} of residuals, with one
    element for each dataset modeled. }
    \item{\code{fitted}:}{Object of class \code{"list"} of fits, with one
    element for each dataset modeled. }
    \item{\code{irfvec}:}{Object of class \code{"list"} with a vector of
    elements for each element of the clp \code{x2}}
  }
}

\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{
\code{\link{fit-class}}, \code{\link{multimodel-class}}
}
\keyword{classes}
