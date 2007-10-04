\name{residPart-methods}
\docType{genericFunction}
\alias{residPart-methods}
\alias{residPart}
\alias{residPart,kin-method}
\alias{residPart,spec-method}
\alias{residPart,mass-method}
\title{Generic function residPart in Package `TIMP' }
\description{
Methods for function \code{residPart} in Package `TIMP' determine the 
part of the residual vector associated with a single `part' of the
dataset(s). 
}
\usage{
residPart(model, group, multimodel, thetalist, clpindepX, finished, returnX,
rawtheta)
} 
\arguments{
 \item{model}{Object of class \code{dat}; switches on this 
argument. }
  \item{group}{list of vector pairs (x2 index, dataset index) for which 
the part of the residual vector is to be determined}
   \item{multimodel}{Object of class \code{multimodel}} 
 \item{thetalist}{Object of class \code{multitheta}} 
 \item{clpindepX}{Object of class \code{matrix} containing the matrix
 determined directly by the nonlinear parameters (e.g., a concentration matrix
in the case of a kinetic model) in the case that this matrix does not depend
on the \code{x2} index} 
 \item{finished}{logical determining whether fitting is finished that
 triggers the storage of results} 
  \item{returnX}{logical determining whether to just return the matrix 
\code{X} directly dependent on nonlinear parameters; this is used in the finite
difference derivative of \code{X} used to get standard error estimates on the 
conditionally linear parameters. }
\item{rawtheta}{numeric vector of nonlinear parameters to be optimized 
by \code{nls}; this is used in the finite
difference derivative of \code{X} used to get standard error estimates on the 
conditionally linear parameters.
} 
}
\keyword{methods}
\seealso{ \code{\link{dat-class}}, \code{\link{spec-class}},
\code{\link{kin-class}} } 

