\name{spec-class}
\docType{class}
\alias{spec-class}
\alias{spec}
\title{Class "spec" for the storage of spectral models. } 
\description{
  \code{spec} is the class for spectral models; 
	an object 
of class "mass" is initialized if 
\code{mod_type = "spec"} is an 
argument of \code{initModel}.
All objects of class 
\code{spec} are
also of class \code{dat}; see documentation for \code{dat} 
for a description of 
these slots.  Note that here \code{x2} 
will refer to the independent variable in
which traces are resolved, e.g., wavelength or wavenumber.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("spec", ...)} or 
\code{spec(...)}. 
}
\section{Slots}{
	 \describe{
    \item{\code{clpequ}:}{ Object of class \code{"vector"}  of starting values for linear relationships between 
clp}
    \item{\code{specpar}:}{ Object of class \code{"list"}  of vectors of 
starting values for spectral parameters; the number of vectors gives the 
number of components in the resulting spectral model;  
each vector contains the parameters 
associated with a component.  e.g., 
\code{specpar = list(c(20000, 3000, .3, 21000, 2000, .4), c(18000, 1000, .2))};
the parameters in each vector are grouped 
\code{c(location_spectra, width_spectra, skew_spectra)}.  
the location and width parameters are given in wavenumbers.  
}
    \item{\code{specfun}:}{ Object of class \code{"character"},  \code{"gaus"} for a spectral model of a
superposition of skewed Gaussians; \code{"bspline"} for a 
bspline-based model.  }
    \item{\code{specref}:}{ Object of class \code{"numeric"}  index defining the center value of the 
\code{x2} variable.}
    \item{\code{specCon}:}{ Object of class \code{"list"}  used internally to 
store constraints. }
    \item{\code{specdisp}:}{Object of class \code{"logical"}  \code{TRUE} if time-dependence of the spectral parameters
is to be taken into account and \code{FALSE} otherwise}
    \item{\code{specdisppar}:}{ Object of class \code{"list"}  }
    \item{\code{specdispindex}:}{ Object of class \code{"list"}  of vectors defining those indexes of specpar whose
time-dependence is to be modeled.  e.g., \code{specdispindex = list(c(1,1), 
c(1,2), c(1,3))} says that parameters 1-3 of spectra 1 are to be modeled as 
time-dependent.}
    \item{\code{nupow}:}{ Object of class \code{"numeric"}   describing the power to which wavenumbers are raised in 
the model equation; see Equation 30 of the paper in the references section 
for a complete description}
    \item{\code{timedep}:}{Object of class \code{"logical"} describing whether the model for spectra E is dependent
on x-index (i.e., whether it is clp-dependent). }
    \item{\code{parmufunc}:}{ Object of class \code{"character"}  describing the function form of the 
time-dependence of spectral parameters; options are \code{"exp"} 
for exponential time 
dependence, \code{"multiexp"} for multiexponential time dependence, and 
\code{"poly"} for 
polynomial time dependence. defaults to polynomial time dependence. } 
\item{ ncole }{vector describing the number of columns of the E matrix for 
each value in the \code{x} vector}
   }
}
\section{Extends}{
Class \code{\link{dat-class}}, directly.
}

\author{Katharine M. Mullen, Ivo H. M. van Stokkum} 
\references{
Ivo H. M. van Stokkum, 
"Global and target analysis of time-resolved spectra, Lecture notes
for the Troisieme Cycle de la Physique en Suisse Romande", 
Department of Physics and Astronomy, Faculty of Sciences, Vrije Universiteit,
Amsterdam, The Netherlands, 2005, 
\url{http://www.nat.vu.nl/~ivo/lecturenotes.pdf}
}
\seealso{ \code{\link{kin-class}},
\code{\link{dat-class}} }

\keyword{classes}
