\name{multimodel-class}
\docType{class}
\alias{multimodel-class}
\alias{multimodel}
\title{Class "multimodel" for storage of multidataset models, data and the 
results of fitting. }
\description{ 
\code{multimodel} is the class to store data, a generally applicable model, a
list of per-data models, a specification of per-dataset model differences, and 
results for the analysis of possibly many datasets.  After a call to 
\code{fitModel}
an object \code{.currModel} is initialized of the \code{multimodel} class. }

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("multimodel", ...)} or 
\code{multimodel(...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"list"}  of objects of class \code{dat} containing data}
    \item{\code{model}:}{Object of class \code{"dat"}  of class dat containing a model specification to be
applied to all datasets}
    \item{\code{modellist}:}{Object of class \code{"list"}  of length n where n is the number of datasets given in
\code{data}, and each element i is an object of class \code{dat} giving 
the dataset-specific model applicable to \code{data[[i]]}}
    \item{\code{modeldiffs}:}{Object of class \code{"list"}  of per-dataset model differences input as an argument 
to the \code{\link{fitModel}} function}
    \item{\code{fit}:}{Object of class \code{"fit"}  containing a list of
    results per-dataset as well as the output of optimization returned by the
    \code{nls} function.}
  }
}
\details{after a call to \code{\link{fitModel}}, an object of class 
\code{multimodel} exists in the global environment as the variable 
\code{.currModel} }
\seealso{ \code{\link{fitModel} }}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}

\keyword{classes}
