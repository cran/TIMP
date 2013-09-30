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
\alias{getC}
\alias{getCLP}
\alias{getDAS}
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
parEst(result, param = "", dataset = NA, verbose = TRUE, file="",
 stderr=TRUE)
getXList(result, group = vector(), file="")
getCLPList(result, getclperr = FALSE, file="")
getX(result, group = vector(), dataset=1, file="", lreturnA = FALSE, lreturnC = FALSE)
getC(result, dataset=1, file="") 
getCLP(result, getclperr = FALSE, dataset=1, file="") 
getDAS(result, getclperr = FALSE, dataset=1, file="") 
getData(result, dataset = 1, weighted = FALSE)
getResiduals(result, dataset = 1)
getSVDResiduals(result, numsing = 2, dataset = 1)
getTraces(result, dataset = 1, file="")
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
\item{lreturnA}{logical indicating whether to return an A matrix instead}
\item{lreturnC}{logical indicating whether to return a C matrix instead}
\item{file}{character vector; if not \code{""} writes the results to a
  file with name \code{file}. }
\item{group}{The value at which to determine the X matrix (maybe a
  wavelenth index, for example)}
\item{stderr}{Whether to return standard error estimates on parameters,
if they were calculated in fitting.}  
}
\value{
  \code{sumnls} returns an object of class \code{"summary.nls"}.
  
  \code{onls} returns an object of class \code{"nls"}.
  
  \code{parEst} returns an object of class \code{"list"} representing
  the parameter estimates and the standard errors if \code{stderr=TRUE}
  and they have been calculated.

  \code{getXList} returns a \code{"list"} of length equal to the number of
  datasets modeled, where each element represents the matrix determined
  by the nonlinear parameters (under a kinetic model, the concentrations). 
  
  \code{getCLPList} returns a \code{"list"} of length equal to the number of
  datasets modeled, where each element represents the matrix determined
  as conditionally linear parameters (under a kinetic model, the spectra).

  \code{getX} returns a numeric \code{"matrix"} 
  that represents the matrix determined
  by the nonlinear parameters (under a kinetic model, the concentrations).
  However, in case lreturnC = TRUE it returns the C matrix, and in case
  lreturnA = TRUE it returns the A matrix that is used to compute 
  the C matrix in case the kinetic model differs from parallel decays.
  
  \code{getC} returns (under a kinetic model) a numeric \code{"matrix"} 
  that represents the raw matrix of concentrations of the dataset determined
  by the nonlinear parameters. 
  
 \code{getDAS} returns (under a kinetic model) a numeric \code{"matrix"}
  that represents the Decay Associated Spectra (DAS).
  
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
\examples{

\dontrun{

## Example showing the addition of non-negativity constraints to 
## conditionally linear parameters (here the spectra associated with
## a kinetic model)

## For the 1st simulated dataset, the constraints offer a modest improvement
## in the estimated spectra, whereas for the 2nd simulated dataset, they
## prevent a catastrophe in which the estimated components are hugely
## compensating. 

##############################
## load TIMP
##############################

require(TIMP)

##############################
## set random seed for reproducability of noise
##############################

set.seed(80)

##############################
## SIMULATE DATA, noise realization 1
##############################

 dt4 <- simndecay_gen(kinpar = c(.4, .8, 2), seqmod = FALSE, tmax
     = 2, deltat = .04, specpar = list(c(25000, 3000, .01), c(22000,
     3000, .01), c(18000, 3000, .01)), lmin=350, lmax=550, deltal = 2,
     sigma=.01)

##############################
## SPECIFY INITIAL MODEL
##############################

mod1 <- initModel(mod_type = "kin", kinpar = c(.4, .8, 2),
seqmod=FALSE)

##############################
## FIT INITIAL MODEL
##############################

sT <- fitModel(list(dt4), list(mod1), opt=kinopt(iter=50, plot=FALSE))

##############################
## EXTRACT ESTIMATED SPECTRA 
## these spectra have some negative values
############################## 
                                   
sTcp <- getCLP(sT)

## plot the estimated spectra with the values used in
## simulation  (before adding noise) for comparison 
 matplot(dt4@x2, sTcp, xlab = "wavelength (nm)", col = 2:4, type="l",
        ylab="",lty=1, main =
        paste("Estimated spectra, adding no constraints\n"))
 matplot(dt4@x2,dt4@E2, add=TRUE, type="l", col=1, lty=2)
 abline(0,0)


##############################
## FIT INITIAL MODEL 
## adding constraints to non-negativity of the
## spectra via the opt option nnls=TRUE
##############################

sV <- fitModel(list(dt4), list(mod1), opt=kinopt(iter=50, nnls=TRUE,
                                        plot=FALSE))

##############################
## EXTRACT ESTIMATED SPECTRA 
## these spectra have no negative values
############################## 
                                   
sVcp <- getCLP(sV)

## plot the estimated spectra with the values used in
## simulation  (before adding noise) for comparison 
 matplot(dt4@x2, sVcp, xlab = "wavelength (nm)", col = 2:4, type="l",
 ylab="",lty=1,
         main = paste("Estimated spectra, with non-negativity constraints\n"))
 matplot(dt4@x2,dt4@E2, add=TRUE, type="l", col=1, lty=2)
 abline(0,0)

##############################
## SIMULATE DATA, noise realization 2
##############################

 dt4_2 <- simndecay_gen(kinpar = c(.4, .8, 2), seqmod = FALSE, tmax
     = 2, deltat = .04, specpar = list(c(25000, 3000, .01), c(22000,
     3000, .01), c(18000, 3000, .01)), lmin=350, lmax=550, deltal = 2,
     sigma=.01)

##############################
## SPECIFY INITIAL MODEL
##############################

mod1 <- initModel(mod_type = "kin", kinpar = c(.4, .8, 2),
seqmod=FALSE)

##############################
## FIT INITIAL MODEL
##############################

sT <- fitModel(list(dt4_2), list(mod1), opt=kinopt(iter=50,plot=FALSE))
    
##############################
## EXTRACT ESTIMATED SPECTRA 
## these spectra have some negative values
############################## 
                                   
sTcp <- getCLP(sT)

## plot the estimated spectra with the values used in
## simulation (before adding noise) for comparison 
 matplot(dt4@x2, sTcp, xlab = "wavelength (nm)", col = 2:4, type="l",
        ylab="",lty=1, main =
        paste("Estimated spectra, adding no constraints\n"))
 matplot(dt4@x2,dt4@E2, add=TRUE, type="l", col=1, lty=2)
 abline(0,0)

##############################
## FIT INITIAL MODEL 
## adding constraints to non-negativity of the
## spectra via the opt option nnls=TRUE
##############################

sV <- fitModel(list(dt4_2), list(mod1), opt=kinopt(iter=50, nnls=TRUE,
                                          plot=FALSE))

##############################
## EXTRACT ESTIMATED SPECTRA 
## these spectra have no negative values
############################## 
                                   
sVcp <- getCLP(sV)

## plot the estimated spectra with the values used in
## simulation (before adding noise) for comparison 
 matplot(dt4@x2, sVcp, xlab = "wavelength (nm)", col = 2:4, type="l",
 ylab="",lty=1,
         main = paste("Estimated spectra, with non-negativity constraints\n"))
 matplot(dt4@x2,dt4@E2, add=TRUE, type="l", col=1, lty=2)
 abline(0,0)

}
}
\seealso{ \code{\link{fitModel}} }
\keyword{file}
