\name{theta-class}
\docType{class}
\alias{theta-class}
\alias{theta}
\title{Class "theta" for storage of nonlinear parameter estimates}
\description{
\code{theta} is the class to store parameter estimates associated with possibly 
many datasets; after a call to fitModel a list containing n theta objects 
for each of the n datasets analyzed in the call to fitModel is created
as the invisible object \code{.currTheta}.  To see the parameter estimates 
associated with the ith dataset, examine \code{.currTheta[[i]]} }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("theta", ...)} or 
\code{theta(...)}. 
}
\section{Slots}{
	 \describe{
    \item{\code{kinpar}:}{Object of class \code{"vector"} of rate constant estimates }
    \item{\code{specpar}:}{Object of class \code{"list"}   of spectral shape parameter estimates}
    \item{\code{irfpar}:}{Object of class \code{"vector"}  of IRF parameter estimates}
    \item{\code{parmu}:}{Object of class \code{"list"}   of parameter estimates describing dispersion of the location
of other parameters (in time, temp., etc.)}
    \item{\code{partau}:}{Object of class \code{"vector"}  of parameter estimates describing dispersion of the 
width of other parameters (in time)}
    \item{\code{clpequ}:}{Object of class \code{"vector"}  of parameter estimates describing conditionally linear
parameters (spectra, in a kinetic model) relations}
    \item{\code{specdisppar}:}{Object of class \code{"list"}  of parameter estimates describing dispersion of
spectra}
    \item{\code{kinscal}:}{Object of class \code{"vector"}  of parameters describing kinetic relations in
the context of a compartmental scheme}
    \item{\code{prel}:}{Object of class \code{"vector"}  of parameters describing relations between
parameters (which may be linear, exponential, etc.)}
    \item{\code{coh}:}{Object of class \code{"vector"}  of parameters
    describing a coherent artifact or pulse follower. }
    \item{\code{drel}:}{Object of class \code{"vector"}  of parameters describing relations between
datasets (linear, and possibly per-wavelength or, in general, per-clp)}
  }
}

\details{after a call to \code{\link{fitModel}}, an object of class 
\code{theta} exists in the global environment as the variable 
\code{.currTheta}  }  
\seealso{ \code{\link{fitModel} }, \code{\link{multitheta-class}}}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\keyword{classes}
