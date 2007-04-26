\name{sumKinSpecEst}
\alias{sumKinSpecEst}

\title{Makes a summary plot of spectra associated with kinetic components
alongside a plot showing parameter estimates}
\description{Makes a summary plot of spectra associated with kinetic components
alongside a plot showing parameter estimates for, by default, kinetic 
parameters.  If the analysis had more parameters in the \code{addEst} slot 
of the arguement \code{opt}, then more parameters are displayed. 
Note that this summary leaves out the spectra associated with coherent 
artifact or scatter. 
}
\usage{
sumKinSpecEst(listFits, addtitle = TRUE, customtitle = "", preps = "", 
ylimlist=list(), kinspecerr=TRUE)
}
\arguments{
  \item{listFits}{list of objects returned by the \code{fitModel} function}
  \item{addtitle}{logical regarding whether to add a title; if TRUE and
  \code{customtitle} is not given then the title is  
  \code{ "Summary of EADS for: "} plus the analysis titles}
  \item{customtitle}{character vector containing a title}
  \item{preps}{character vector describing the prefix of the postscript
  filename given as output}
   \item{ylimlist}{list with elements \code{list(ind, ylim)}.  \code{ind} is
   an index into \code{listFits}; \code{ylim} is the desired \code{ylim} for
   the plot for that analysis}
 \item{kinspecerr}{logical regarding whether to add error bars for to the 
estimated spectra. 
}
}
\details{
This looks best with less than five objects in \code{listFits}.
}
\value{

}

\seealso{ \code{\link{fitModel}}, \code{\link{examineFit}} }

\author{Katharine M. Mullen, Ivo H. M. van Stokkum} 
\keyword{file}
