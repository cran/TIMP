\name{outlierCorr}
\alias{outlierCorr}
\title{Finds and removes outliers from a datasets}
\description{Finds and removes outliers from datasets 
 given the 
results of fitting as returned by \code{fitModel}.  Uses the 
residuals in the fitted results to return a list of corrected 
datasets to be used in place of the datasets used in the call
to \code{fitModel} as well as a list of weights.  The data returned
contains the fitted values at pointed that are outliers and will be 
assigned zero weight in subsequent fits. 
}
\usage{
outlierCorr(oldRes, thresh=.8) 
}
\arguments{
 \item{oldRes}{Object returned by \code{fitModel} function }
 \item{thresh}{Object of class \code{"numeric"} determining what 
points to consider outliers.  }
}
\details{The points that are outliers are those points \code{xx} 
at a given value \code{j} of \code{x2} that satisfy
 \code{thresh < resid[j,]/max(fitted[j,xx], max(fitted[j,xx])/10)} .
Increasing \code{thresh} will assign fewer points as outliers. 
}
\value{list containing the elements \code{dt}, a list of 
corrected datasets, and \code{weightList}, a list of new weight 
matrices.
}
\seealso{ \code{\link{fitModel}}, \code{\link{preProcess}}}
\keyword{file}
