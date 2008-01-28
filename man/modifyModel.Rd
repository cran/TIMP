\name{modifyModel}
\alias{modifyModel}
\title{Allows the starting values for parameters associated with a model
to be updated with the values found in fitting the model. }
\description{  Allows the starting values for parameters associated with 
a model to be updated with the values found in fitting the model.  That
is, a model is specified with \code{initModel}.  Then \code{fitModel} 
is used to optimize the starting values for parameters.  \code{modifyModel}
allows modifification of the starting values in the model specification
with the optimized values found via \code{fitModel}.  
}
\usage{
modifyModel(model = list(), newest = list(), exceptslots = vector() )
}
\arguments{
  \item{model}{an object of class \code{dat} returned by \code{initModel};
  if this argument is of \code{length(0)}, which is the default, then the 
  last model fit is used (which is found in the global variable 
  \code{.currModel@model})} 
  \item{newest}{ an object of class \code{theta} containing new parameter
  estimates;    if this argument is of \code{length(0)}, which is the default, 
  then the parameter estimates associated with dataset 1 in the last model fit
  are used (which are found in 
  the global variable \code{.currTheta[[1]]})}
  \item{exceptslots}{a vector of character vector of slot names whose 
  corresponding slots are to be left out of the update}
}

\value{an object of class \code{dat} that returns the results of 
calling \code{initModel} with the new starting values. 
}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{ \code{\link{initModel}}, \code{\link{fitModel}}}

\keyword{file}
