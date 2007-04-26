\name{initModel}
\alias{initModel}
\title{Defines the model to be used in analysis.}
\description{Allows definition of a model of class "dat" to be used 
in analysis.  The arguments specify the model. 
}
\usage{
initModel(...)
}
\arguments{
  \item{\dots}{specify the model class via the character string
   e.g., \code{\link{kin-class}} or \code{\link{spec}} and 
   any of the slots associated with that model type (which is 
   a subclass of class \code{dat}, so that all slots in \code{dat} 
   may also be specified),   
   e.g., \code{mod_type = "kin"} will initialize a model with 
   class \code{kin}, for a kinetic model.  }
}

\value{an object of class \code{dat} with the sub-class given by the value of 
the \code{mod_type} input.  
}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\details{For examples, see the help files for \code{\link{dat-class}} and 
\code{\link{fitModel}}} 
\seealso{ \code{\link{dat-class}}, \code{\link{kin-class}}, 
\code{\link{spec-class}}, 
\code{\link{fitModel}}}    

\keyword{file}

