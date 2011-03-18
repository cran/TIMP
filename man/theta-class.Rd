\name{theta-class}
\docType{class}
\alias{theta-class}
\alias{theta}
\title{Class "theta" for storage of nonlinear parameter estimates}
\description{
  \code{theta} is the class to store parameter estimates associated with
  possibly 
  many datasets; after a call to \code{fitModel}
  a list containing \code{theta} objects 
  for each of the n datasets analyzed in the call to \code{fitModel} is
  created.   To see the parameter estimates 
  associated with the datasets, examine the object \code{currTheta}
  in the list returned by \code{fitModel}}
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
    \item{\code{eigenvaluesK}:}{Object of class \code{"vector"}  containing the eigenvalues of the kinetic transfer matrix K}
    \item{\code{coh}:}{Object of class \code{"vector"}  of parameters
    describing a coherent artifact or pulse follower. }
    \item{\code{drel}:}{Object of class \code{"vector"}  of parameters describing relations between
datasets (linear, and possibly per-wavelength or, in general, per-clp)}
    \item{amplitudes}{}
    \item{amps}{}
    \item{anipar}{}
    \item{cohirf}{}
    \item{jvec}{}
    \item{kin2scal}{}
    \item{kinpar2}{}
    \item{kinscalspecial}{}
    \item{peakpar}{}
    \item{shift}{}
  }
}
\details{after a call to \code{\link{fitModel}}, an object of class 
\code{theta} exists in the global environment as the variable 
\code{currTheta}  }  
\seealso{ \code{\link{fitModel} }, \code{\link{multitheta-class}}}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\keyword{classes}
