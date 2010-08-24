\name{getClpindepX-methods}
\docType{methods}
\alias{getClpindepX-methods}
\alias{getClpindepX,kin-method} 
\alias{getClpindepX,spec-method}
\alias{getClpindepX,mass-method}
\alias{getClpindepX,amp-method}
\alias{getClpindepX}
\title{Generic function getClpindepX in Package `TIMP'}
\description{
Gets the matrix associated with nonlinear parameter estimates for the 
case that this matrix is not re-calculated per conditionally linear
parameter.
}
\usage{
getClpindepX(model, multimodel, theta, returnX, rawtheta, dind)
} 
\arguments{
  \item{model}{Object of class \code{dat}; function switches on this 
argument. 
  }
\item{multimodel}{Object of class \code{multimodel} used in standard error
determination}
\item{theta}{Vector of nonlinear parameter estimates.}
\item{returnX}{logical indicating whether to return a vectorized version of 
the \code{X} matrix}
\item{rawtheta}{vector of nonlinear parmeters; used in standard error
determination}
\item{dind}{numeric indicating the dataset index; used in standard error
determination}
}
\keyword{methods}
\seealso{ \code{\link{dat-class}}}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}