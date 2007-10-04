\name{specopt-class}
\docType{class}
\alias{specopt-class}
\alias{specopt}
\title{Class "specopt" stores options for fitting and plotting spectral models}
\description{Class "specopt" stores options for fitting and plotting spectral
models in particular; this is a subclass of class \code{opt}.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("specopt", ...)}.
or \code{specopt(...)}} 
\section{Slots}{
	 \describe{
    \item{\code{nospectra}:}{Object of class \code{"logical"} 
    that defaults
    to \code{FALSE}; if \code{TRUE}, do not plot time-resolved spectra}
    \item{\code{selectedspectra}:}{Object of class \code{"vector"} 
    containing
     \code{x} indices for which plots of 
     time-resolved spectra are desired under a spectral model}
  }
}

\details{See \code{\link{opt-class}} for 
the specification of fitting/plotting options that are not specific to the 
class type.}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{
\code{\link{opt-class}}, \code{\link{kinopt-class}}
}
\keyword{classes}
