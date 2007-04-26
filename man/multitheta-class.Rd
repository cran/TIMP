\name{multitheta-class}
\docType{class}
\alias{multitheta-class}
\alias{multitheta}
\title{Class "multitheta" that stores a list with one element of class 
"theta" for each dataset modeled. }
\description{	 
Class \code{multitheta} stores a list with one element of class 
\code{theta} for each dataset modeled, corresponding to the parameter 
estimates associated with that dataset. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("multitheta", ...)} or
\code{multitheta(...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{th}:}{Object of class \code{"list"}  with element i
    corresponding to the \code{theta} object for the ith dataset modeled. 
  }
} }

\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{
\code{\link{theta-class}}, \code{\link{dat-class}} 
}
\keyword{classes}
