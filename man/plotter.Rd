\name{plotter-methods}
\docType{methods}
\alias{plotter-methods}
\alias{plotter} 
\alias{plotter,kin-method}
\alias{plotter,spec-method}
\alias{plotter,mass-method}
\title{Generic function plotter in Package `TIMP'}
\description{
Methods for function \code{plotter} in Package `TIMP' that 
call plotting and output functions. 
}
\usage{
plotter(model, multimodel, multitheta, plotoptions)
}
\arguments{
\item{model}{Object of class \code{dat}; function switches on this 
argument. }
\item{multimodel}{Object of class \code{multimodel}}
\item{multitheta}{Object of class \code{multitheta}}
\item{plotoptions}{list of output options 
input to \code{fitModel} as the argument \code{opt}}
}
\keyword{methods}
\seealso{\code{\link{dat-class}}}

\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
