\name{getResults}
\alias{getResults}
\alias{onls}
\alias{sumnls}
\alias{parEst}
\alias{getCLPList}
\alias{getDataList}
\alias{getTracesList}
\alias{getdim1List}
\alias{getdim2List}
\alias{getResidualList}
\alias{getXList}
\title{Functions to print and return parts of the object returned
by the fitting routines. }
\description{Functions to print and return parts of the object returned
  by \code{fitModel}.  \code{onls} returns the output of the
  \code{nls}
  function.  \code{sumonls} returns the result of calling \code{summary}
  on \code{onls} function.  \code{parEst} returns a summary of model
  parameter estimates.  The remaining functions return lists
  representing various aspects of the results returned by the function
  fitModel.     
}
\usage{
onls(result)
sumnls(result) 
parEst(result, param = "", dataset = NA, verbose = TRUE)
getXList(result)
getCLPList(result, getclperr = FALSE) 
getDataList(result, numsing = 2)
getResidualList(result, numsing = 2)
getTracesList(result)
getdim1List(result) 
getdim2List(result)
}
\arguments{
  \item{result}{return value of \code{fitModel}}
  \item{param}{character vector of the particular parameters to return;
    if \code{param==""} then all parameters are given. }
  \item{dataset}{index of the dataset from which to return results; by
    default \code{dataset=NA} in which case results from all datasets
    are returned}
  \item{verbose}{logical that defaults to \code{TRUE} that determines
    whether \code{parEst} just returns a list invisibly or prints as well. }
  \item{getclperr}{logical that defaults to \code{FALSE} that determines
  whether a list containing the standard error estimates associated with
  the conditionally linear parameters, as opposed to the conditionally
  linear parameters themselves}
\item{numsing}{integer that defaults to 2; determines the number of
  singular vectors to return}
}
\value{
  \code{sumnls} returns an object of class \code{"summary.nls"}.
  
  \code{onls} returns an object of class \code{"nls"}.
  
  \code{parEst} returns an object of class \code{"list"} representing
  the parameter estimates.

  \code{XList} returns a \code{"list"} of length equal to the number of
  datasets modeled, where each element represents the matrix determined
  by the nonlinear parameters (under a kinetic model, the concentrations). 
  
  \code{CLPList} returns a \code{"list"} of length equal to the number of
  datasets modeled, where each element represents the matrix determined
  as conditionally linear parameters (under a kinetic model, the spectra).

  \code{getDataList} returns a \code{"list"} of length 2.
  The first element is named \code{svddatalist} and is a 
 \code{"list"} of length equal to the number of
 datasets modeled, where each element is a list with elements
 \code{values}, \code{left} and \code{right}, where  \code{values}
 contains the singular values, \code{left} contains \code{numsing}
 left singular vectors, and  \code{right} contains \code{numsing}
 right singular vectors, all of the residuals. 
 The second
 element is named "datalist" and   
 is a \code{"list"} of length equal to the number of
 datasets modeled, where each element represents the matrix of
 unweighted data.
 
 \code{getResidualList} returns a \code{"list"} of length 2.
  The first element is named \code{svdresidlist} and is a 
  \code{"list"} of length equal to the number of
  datasets modeled, where each element is a list with elements
  \code{values}, \code{left} and \code{right}, where  \code{values}
  contains the singular values, \code{left} contains \code{numsing}
  left singular vectors, and  \code{right} contains \code{numsing}
  right singular vectors, all of the residuals. 
  The second
  element is named "residlist" and   
  is a \code{"list"} of length equal to the number of
  datasets modeled, where each element represents the matrix of
  residuals.

  \code{getTracesList}  returns a \code{"list"} of length equal to the
  number of datasets modeled, where each element is a  \code{"list"} of
  length equal to the number of \code{x2} values in the dataset
  (wavelengths for kinetic models).  Each of the \code{x2} elements
  represents the trace, that is, fit with respect to time, at the
  corresponding \code{x2}-value.  

  \code{getdim1List}  returns a \code{"list"} of length equal to the
  number of datasets modeled, where each element is a  \code{"vector"} of
  length equal to the number of \code{x} values in the dataset
  (times for kinetic models).

  \code{getdim2List}  returns a \code{"list"} of length equal to the
  number of datasets modeled, where each element is a  \code{"vector"} of
  length equal to the number of \code{x2} values in the dataset
  (wavelengths for kinetic models).
}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{ \code{\link{fitModel}} }
\keyword{file}
