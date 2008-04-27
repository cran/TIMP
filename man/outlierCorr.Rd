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
outlierCorr(oldRes, fence=3, saturCorr=FALSE, saturThresh=.05,
                          saturMin=NA, saturDivMax=3, outlierCorr=TRUE,
                          newM = TRUE) 
}
\arguments{
  \item{oldRes}{Object returned by \code{fitModel} function }
 \item{fence}{Object of class \code{"numeric"} determining what 
   points to consider outliers.  }
 \item{saturCorr}{whether to correct for saturation}
 \item{saturThresh}{See code.}
 \item{saturMin}{See code.}
 \item{saturDivMax}{See code.}
 \item{outlierCorr}{whether to perform outlier correction}
 \item{newM}{whether to add to the outliers and saturation points
 detected previousely}
}
\details{We calculate the fourth spread at a given value of
  \code{x2} in a dataset.  Those points that are less than the
  first quartile minus the fourth spread times \code{fence} are outliers,
  as are those points that are more than the third quartile plus the
  fourth spread times \code{fence}.  Outliers are assigned a weight of
  zero and are assigned the values found in fitting for the purpose of
  generating smooth-looking plots. } 
\value{list containing the elements \code{dt}, a list of 
  corrected datasets, and \code{weightList}, a list of new weight 
  matrices.
}
\seealso{ \code{\link{fitModel}}, \code{\link{preProcess}}}
\keyword{file}
