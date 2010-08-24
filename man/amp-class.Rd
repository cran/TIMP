\name{amp-class}
\docType{class}
\alias{amp-class}
\alias{amp}
\title{Class "amp" for diagonal matrix model specification.}
\description{ 
\code{amp} is the class for diagonal matrix model specification;
such models are internally initialized when a tri-linear-type 
model is fit to the data via passing the argument \code{opt}
to \code{fitModel} as an object of class \code{opt} in which
the slot \code{trilinear} has the value \code{TRUE}.
 All objects of class \code{amp} are sub-classes of 
class \code{dat}; see documentation for \code{dat} 
for a description of 
these slots.  }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("amp", ...)} or 
 \code{amp(...)}.
}
\section{Slots}{
	 \describe{
	\item{amps}{ list of vectors of starting values for the parameters
	  of the amplitudes for each dataset; one vector of values is
	  used to parameterize the values corresponding to each dataset. 
	}
	\item{autoclp0}{}
	\item{C2}{}
	\item{chinde}{}
	\item{clinde}{}
	\item{clp0}{}
	\item{clpCon}{}
	\item{clpdep}{}
	\item{clpequ}{}
	\item{clpequspec}{}
	\item{clpequspecBD}{}
	\item{clpType}{}
	\item{cohcol}{}
	\item{compnames}{}
	\item{constrained}{}
        \item{datafile}{}
	\item{datCall}{}
	\item{drel}{}
	\item{dscalspec}{}
	\item{E2}{}
	\item{fixed}{}
	\item{free}{}
	\item{fvecind}{}
	\item{getX}{}
	\item{getXsuper}{}
	\item{highcon}{}
	\item{inten}{}
	\item{iter}{}
	\item{lclp0}{}
	\item{lclpequ}{}
	\item{lowcon}{}
	\item{makeps}{}
	\item{mhist}{}
        \item{mod_type}{}
	\item{mvecind}{}
	\item{ncomp}{}
	\item{nl}{}
	\item{nt}{}
	\item{nvecind}{}
	\item{outMat}{}
	\item{parnames}{}
        \item{positivepar}{}
	\item{prel}{}
	\item{prelspec}{}
	\item{psi.df}{}
	\item{psi.weight}{}
	\item{pvecind}{}
	\item{satMat}{}
        \item{scalx}{}
	\item{sigma}{}
	\item{simdata}{}
	\item{title}{}
	\item{usecompnames0}{}
	\item{usecompnamesequ}{}
	\item{weight}{}
        \item{weightList}{}
	\item{weightM}{}
	\item{weightpar}{}
	\item{weightsmooth}{}
	\item{x}{}
	\item{x2}{}
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
    \code{\link{kin-class}}, \code{\link{spec-class}}, \code{\link{opt-class}}
  }
  \keyword{classes}
  