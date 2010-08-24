\name{massopt-class}
\docType{class}
\alias{massopt-class}
\alias{massopt} 
\title{Class "massopt" stores options for fitting and plotting models
for mass spectrometry data}
\description{Class "massopt" stores options for fitting and plotting models
models for mass spectrometry data
in particular; this is a subclass of class \code{opt} that contains 
options applicable to all model types }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("massopt", ...)} or
\code{massopt(...)}
}
\section{Slots}{
	 \describe{
	   \item{\code{axis.by}:}{Object of class \code{"numeric"} that
	     allows labels on the bars representing the mass spectra to
	     to skipped, e.g., \code{axis.by=2} will add a label to
	     every second bar}
	   \item{\code{scale.concen}:}{Object of class \code{"logical"} that
	     scales the concentration matrix using the algorithm found
	     in the function \code{scaleConList}.
	   }
	   \item{\code{nummaxtraces}:}{Object of class
	     \code{"nummaxtraces"} that defaults to zero; if greater
	     than zero then this number of the traces with the maximum
	     amplitude are plotted}
       }}
       \details{See \code{\link{opt-class}} and
	 for 
the specification of fitting/plotting options that are not specific to the 
\code{mass} class type.}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{
\code{\link{examineFit}}, \code{\link{fitModel}}, \code{\link{opt-class}}, \code{\link{specopt-class}}
}
\keyword{classes}
