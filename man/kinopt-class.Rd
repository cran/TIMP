\name{kinopt-class}
\docType{class}
\alias{kinopt-class}
\alias{kinopt}
\title{Class "kinopt" stores options for fitting and plotting kinetic models}
\description{Class "kinopt" stores options for fitting and plotting kinetic
  models in particular; this is a subclass of class \code{opt}}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("kinopt", ...)} or
\code{kinopt(...)}
}
\section{Slots}{
	 \describe{
    \item{\code{notraces}:}{Object of class \code{"logical"} that defaults
    to \code{FALSE}; if \code{TRUE}, do not plot traces}
    \item{\code{selectedtraces}:}{Object of class \code{"vector"} containing
     \code{x} indices for which plots of
     traces are desired under a kinetic model }
     \item{\code{breakdown}:}{Object of class \code{"list"} with the
following elements:
  \itemize{
  \item{plot}{vector of \code{x2} values to plot the breakdown for.
  These values be specified in
  a fuzzy way:  an \code{x2} value within \code{abs(x2[1] - x2[2])/100}
  a value given in \code{plot} means that a plot for that \code{x2} value
  will be generated, where
  the reference \code{x2[1]} and \code{x2[2]} are from the first dataset
  modeled.
    }
   \item{tol}{numeric giving a tolerance by which
   the values in \code{plot} are compared to \code{x2} values
   for near-equality. The default is defined as
   \code{abs(x2[1] - x2[2])/100}. }
      \item{superimpose}{vector of dataset indices for which results should
    be superimposed if the dataset has an \code{x2} value at a value in
    \code{plot}.}
    }
    }
    \item{FLIM}{Object of class \code{"logical"} that defaults to
    \code{FALSE}; if \code{TRUE}, the data represent a FLIM experiment and
    special plots are generated. }
\item{FLIMresidimag}{Object of class \code{"logical"} that defaults to
    \code{TRUE}; if \code{FALSE} and a FLIM image is analyzed, the residuals
are not plotted as an image.}
\item{noFLIMsummary}{Object of class \code{"logical"} that defaults to
    \code{FALSE}; if \code{TRUE} and a FLIM image is analyzed, only other
plots requested by the user (such as traces or residuals) are generated, and
no summary plot in made.}
     \item{kinspecest}{Object of class \code{"logical"} that defaults to
    \code{FALSE}; if \code{TRUE}, make a plot of the spectra associated with
    the kinetic components as well as the lifetime estimates. }

 \item{writeplaincon}{Object of class \code{"list"}; if length is greater
than 0, then the concentration model will be evaluated at the vector of
\code{x} values supplied as the element \code{"x"} of \code{writeplaincon}
and the result will be written to file for each dataset.
}
 \item{writerawcon}{Object of class \code{"logical"}
 that defaults to \code{FALSE}; if \code{TRUE}, then the representation of
the concentration profiles before the application of constraints (to
account for the equality of spectra, etc.) is written to file for each
dataset.
  }
 \item{plotcohcolspec}{Object of class \code{"logical"}
that defaults to \code{TRUE}; if \code{FALSE} then the spectra associated
with the coherent artifact (pulse-follower) are not included in the summary
plots
  }
 \item{\code{plotpulsefol}:}{Object of class \code{"logical"} defaults to
    \code{FALSE}; if \code{TRUE} adding imageplots of pulsefolower amplitudes in summary plot (only with FLIM plots).}
\item{ylimcomp}{Object of class \code{"vector"} that defaults to
\code{vector()}; Works In the case of plotting the results of FLIM image
analysis, \code{ylimspec} can be used to determine the range used in the
image plot of normalized amplitudes.}
  \item{addfilename}{}
  \item{addest }{}
  \item{adddataimage}{}
  \item{algorithm}{}
   \item{coldata}{}
  \item{colfit}{}
  \item{divdrel}{}
  \item{getStartTri}{}
  \item{imagepal}{}
  \item{iter}{}
  \item{kinspecerr}{}
  \item{linrange}{}
  \item{ltydata}{}
  \item{ltyfit}{}
  \item{makeps}{}
  \item{maxfev}{}
  \item{minFactor}{}
  \item{nlsalgorithm}{}
  \item{nnls}{}
  \item{nnlscrit}{}
  \item{noplotest}{}
  \item{normspec}{}
  \item{optimmethod}{}
  \item{output}{}
  \item{paropt}{}
  \item{parscale}{}
  \item{plot}{}
  \item{plotkinspec}{}
  \item{residplot}{}
  \item{residtraces}{}
  \item{samespecline}{}
  \item{specinterpol}{}
  \item{specinterpolbspline}{}
  \item{specinterpolpoints}{}
  \item{specinterpolseg}{}
  \item{stderrclp}{}
  \item{summaryplotcol}{}
  \item{summaryplotrow}{}
  \item{sumnls}{}
  \item{superimpose}{}
  \item{title}{}
  \item{trilinear}{}
  \item{triStart}{}
  \item{writeclperr}{}
  \item{writecon}{}
  \item{writedata}{}
  \item{writefit}{}
  \item{writefitivo}{}
  \item{writenormspec}{}
  \item{writespec}{}
  \item{writespecinterpol}{}
  \item{xlab}{}
  \item{xlim}{}
  \item{xlimspec}{}
  \item{ylab}{}
  \item{ylimspec}{}
  \item{ylimspecplus}{}
}
}

\details{See \code{\link{opt-class}} for
the specification of fitting/plotting options that are not specific to the
class type.}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{
\code{\link{examineFit}}, \code{\link{fitModel}}, \code{\link{opt-class}}, \code{\link{specopt-class}}
}
\keyword{classes}
