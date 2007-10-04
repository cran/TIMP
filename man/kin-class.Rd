\name{kin-class}
\docType{class}
\alias{kin-class}
\alias{kin}
\title{Class "kin" for kinetic model storage.}
\description{ 
\code{kin} is the class for kinetic models; 
an object 
of class "kin" is initialized if 
\code{mod_type = "kin"} is an 
argument of \code{initModel}.
All objects of class \code{kin} are sub-classes of 
class \code{dat}; see documentation for \code{dat} 
for a description of 
these slots.  }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("kin", ...)} or 
 \code{kin(...)}.
}
\section{Slots}{
	 \describe{
	\item{kinpar}{ vector of rate constants to be used as starting 
	values for the exponential decay of components; the length of this
	vector determines the number of components of the kinetic model.}			 
    }
    \item{\code{specpar}:}{ Object of class \code{"list"}  parameters for
    spectral constraints}
    \item{\code{seqmod}:}{ Object of class \code{"logical"}  that is \code{TRUE} if a sequential model is to be applied
and \code{FALSE} otherwise}
    \item{\code{irf}:}{Object of class \code{"logical"}   that is \code{TRUE} is an IRF is modeled and \code{FALSE} otherwise}
    \item{\code{mirf}:}{Object of class \code{"logical"}  that is \code{TRUE} if a measured IRF is modeled and \code{FALSE}
otherwise}
    \item{\code{measured_irf}:}{ Object of class \code{"vector"}  containing a measured IRF}
    \item{\code{convalg}:}{ Object of class \code{"numeric"}  1-3 determining the numerical convolution algorithm 
used in the case of modeling a measured IRF; if \code{3} then supply a
reference lifetime in the slot \code{reftau}.}
 \item{\code{reftau}:}{ Object of class \code{"numeric"}  containing a
 reference lifetime to be used when \code{convalg=3}}
    \item{\code{irffun}:}{ Object of class \code{"character"}  describing the 
function to use to describe the IRF, by default "gaus"}
    \item{\code{irfpar}:}{ Object of class \code{"vector"}  of IRF parameters; for the common Gaussian IRF this 
vector is ordered \code{c(location, width)}}
    \item{\code{dispmu}:}{Object of class \code{"logical"}   that is \code{TRUE} if dispersion of the parameter for IRF 
location is to be modeled and \code{FALSE} otherwise}
    \item{\code{dispmufun}:}{Object of class \code{"character"}  describing the functional form of the 
dispersion of the IRF location parameter; if equal to "discrete" then the 
IRF location is shifted per element of \code{x2} and \code{parmu} should have the same 
length as \code{x2}.  defaults to a polynomial description}
    \item{\code{parmu}:}{ Object of class \code{"list"}   of starting values for the dispersion model for the 
IRF location}
    \item{\code{disptau}:}{Object of class \code{"logical"}  that is \code{TRUE} if dispersion of the parameter for 
IRF width is to be modeled and \code{FALSE} otherwise}
    \item{\code{disptaufun}:}{ Object of class \code{"character"}  describing the functional form of the 
dispersion of the IRF width parameter; if equal to \code{"discrete"} then the 
IRF width is parameterized per element of \code{x2} and \code{partau} should have the same 
length as \code{x2}.  defaults to a polynomial description}
    \item{\code{partau}:}{ Object of class \code{"vector"}  of starting values for the dispersion model for the 
IRF FWHM }
    \item{\code{fullk}:}{Object of class \code{"logical"}  that is \code{TRUE} if the data are to be modeled using a 
compartmental model defined in a K matrix and \code{FALSE} otherwise}
    \item{\code{kmat}:}{ Object of class \code{"array"}  containing the K matrix descriptive of  a compartmental 
model}
    \item{\code{jvec}:}{ Object of class \code{"vector"}  containing the J vector descriptive of the inputs to a 
compartmental model}
    \item{\code{ncolc}:}{Object of class \code{"vector"}  describing the number of columns of the C matrix for 
each clp in \code{x2}}
    \item{\code{kinscal}:}{ Object of class \code{"vector"}   of starting values for branching parameters in a 
compartmental model}
    \item{\code{kmatfit}:}{Object of class \code{"array"}  of fitted values for a compartmental model}
    \item{\code{cohspec}:}{ Object of class \code{"list"}   describing the model for 
coherent artifact/scatter component(s) containing the element  \code{type}
and optionally the element \code{numdatasets}
if \code{type} is \code{"irf"}, the coherent artifact/scatter has the 
time profile of 
the IRF.  if \code{type} is \code{"freeirfdisp"} the  coherent 
artifact/scatter has  a
Gaussian time profile whose location and width are parameterized in the 
vector \code{coh}.  if \code{type} is \code{"irfmulti"} the time profile of 
the IRF is used for
the coherent artifact/scatter model, but the IRF parameters are taken per 
dataset (for the multidataset case), and the integer argument 
\code{numdatasets} must be equal to the 
number of datasets modeled.  if \code{type} is \code{"seq"} 
a sequential exponential decay 
model is applied, whose parameters are contained in \code{coh}.
if \code{type} is \code{"mix"} a sequential exponential decay 
model is applied along with a model that follows the time profile of the IRF;
the coherent artifact/scatter is then a linear superposition of these two 
models.  }
    \item{\code{coh}:}{ Object of class \code{"vector"}  of starting values for the parameterization of a  
coherent artifact}
    \item{\code{wavedep}:}{Object of class \code{"logical"}  describing whether the kinetic model is dependent on
\code{x2} index (i.e., whether there is clp-dependence)}
    \item{\code{lambdac}:}{ Object of class \code{"numeric"}  for the center wavelength to be used in a polynomial 
description of \code{x2}-dependence }
}
\section{Extends}{
Class \code{\link{dat-class}}, directly.
}

\details{See \code{\link{dat-class}} for an 
example of the initialization of a 
\code{kin} object via the \code{initModel} function. } 
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{
\code{\link{dat-class}}, \code{\link{spec-class}}
}
\keyword{classes}
