\name{modifyModel}
\alias{modifyModel}
\title{Allows the starting values for parameters associated with a model
to be updated with the values found in fitting the model. }
\description{  Allows the starting values for parameters associated with 
a model to be updated with the values found in fitting the model.  
A call \code{model_w_new_starting_vals <- modifyModel(old_model)}
will plug in the optimized parameter values the last model fit
so that are the starting values in the model specification 
\code{model_w_new_starting_vals}.  
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

