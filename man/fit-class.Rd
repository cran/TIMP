\name{fit-class}
\docType{class}
\alias{fit-class}
\alias{fit}
\title{Class "fit" to store the results of model fitting associated with 
all datasets analyzed. }
\description{Class to store results of model fitting associated with 
all datasets in a single call to the \code{fitModel} function.  
An object of class \code{fit} is stored in 
the slot \code{fit} of objects of class \code{multimodel}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("fit", ...)}.
}
\section{Slots}{
	 \describe{
	   \item{rss}{}
	   \item{\code{resultlist}:}{ Object of class \code{"list"} that contains 
	     an object of class \code{res} for each dataset modeled, in the order 
	     that they were specified. }
	   \item{\code{nlsres}:}{Object of class \code{"list"} containing 
	     named elements 
	     \itemize{ 
	       \item{onls}{output of the call to \code{nls} used in model optimization.} 
	       \item{sumonls}{result of call \code{summary(onls)}  }
	       
	     }
	   }
	 }
       }
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{
\code{\link{res-class}}, \code{\link{multimodel-class}}
}
\keyword{classes}
