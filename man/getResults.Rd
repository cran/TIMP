\name{getResults}
\alias{getResults}
\alias{onls}
\alias{sumnls}
\alias{parEst}
\alias{getCLPList}
\alias{getData}
\alias{getSVDData}
\alias{getSVDResiduals}
\alias{getTraces}
\alias{getdim1}
\alias{getdim2}
\alias{getResiduals}
\alias{getSVDResiduals}
\alias{getXList}
\alias{getX}
\alias{getCLP}
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
parEst(result, param = "", dataset = NA, verbose = TRUE, file="")
getXList(result, group = vector())
getCLPList(result, getclperr = FALSE)
getX(result, group = vector(), dataset=1)
getCLP(result, getclperr = FALSE, dataset=1) 
getData(result, dataset = 1, weighted = FALSE)
getResiduals(result, dataset = 1)
getSVDResiduals(result, numsing = 2, dataset = 1)
getTraces(result, dataset = 1)
getdim1(result, dataset = 1) 
getdim2(result, dataset = 1)
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
\item{weighted}{logical indicating whether to return weighted or
  unweighted data}
\item{file}{character vector; if not \code{""} writes the results to a
  file with name \code{file}. }
\item{group}{The value at which to determine the X matrix (maybe a
  wavelenth index, for example)}
  
}
\value{
  \code{sumnls} returns an object of class \code{"summary.nls"}.
  
  \code{onls} returns an object of class \code{"nls"}.
  
  \code{parEst} returns an object of class \code{"list"} representing
  the parameter estimates.

  \code{getXList} returns a \code{"list"} of length equal to the number of
  datasets modeled, where each element represents the matrix determined
  by the nonlinear parameters (under a kinetic model, the concentrations). 
  
  \code{getCLPList} returns a \code{"list"} of length equal to the number of
  datasets modeled, where each element represents the matrix determined
  as conditionally linear parameters (under a kinetic model, the spectra).

  \code{getX} returns a numeric \code{"matrix"} 
  that represents the matrix determined
  by the nonlinear parameters (under a kinetic model, the concentrations). 
  
  \code{getCLPList} returns a numeric \code{"matrix"}
  that represents the matrix determined
  as conditionally linear parameters (under a kinetic model, the spectra).
  
  \code{getSVDData} 
  returns a \code{"list"} of length 3 with named elements
  \code{values}, \code{left} and \code{right}, where  \code{values}
  contains the singular values, \code{left} contains \code{numsing}
  left singular vectors, and  \code{right} contains \code{numsing}
  right singular vectors, all of the unweighted data. The number of singular
  vectors returned is determined by \code{numsing}. 
  
 \code{getData} returns the dataset specified by the
 argument \code{dataset} (weighted data in the case
 that \code{weighted=TRUE}) as a \code{"matrix"}
  
 \code{getResiduals} returns a \code{"matrix"} of residuals for the
  dataset with index given by the argument \code{dataset}; the matrix
  returned has the dimension of the dataset itself. 

  \code{getSVDResiduals}
  returns a \code{"list"} of length 3 with named elements
  \code{values}, \code{left} and \code{right}, where  \code{values}
  contains the singular values, \code{left} contains \code{numsing}
  left singular vectors, and  \code{right} contains \code{numsing}
  right singular vectors, all of the residuals. The number of singular
  vectors returned is determined by \code{numsing}. 
  
  \code{getTraces}  returns a \code{"matrix"} of model estimates for the
  dataset with index given by the argument \code{dataset}; the matrix
  returned has the dimension of the dataset itself. 

  \code{getdim1}  returns a \code{"vector"} of
  \code{x} values in the dataset (times for kinetic models).

  \code{getdim2}  returns a  \code{"vector"} of
  \code{x2} values (wavelengths for kinetic models).
}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{ \code{\link{fitModel}} }
\keyword{file}
