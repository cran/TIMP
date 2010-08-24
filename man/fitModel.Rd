\name{fitModel}
\alias{fitModel}
\title{Performs optimization of (possibly multidataset) models.}
\description{Performs optimization of (possibly multidataset) models and 
outputs plots and files representing the fit of the model to the data. }
\usage{
fitModel(data, modspec=list(), datasetind = vector(),
modeldiffs = list(), opt = opt(),lprogress=FALSE )
}
\arguments{
  \item{data}{list of objects of class \code{dat} containing the data 
  to be modeled}
  \item{modspec}{list whose elements are models of class \code{dat} 
  describing the models as results from a
  call to the function \code{initModel} }
  \item{datasetind}{vector that has the same length as \code{data};
  for each dataset in \code{data} specify the model it should have as 
  an index into \code{modspec}; default mapping is that all datasets 
  use the first model given in \code{modspec}  }
  \item{modeldiffs}{ list whose elements specify any dataset-specific 
  model differences. 
  \itemize{
\item{linkclp}{ list of vectors containing the indices of datasets.  If the 
  two dataset indices are in the same vector, their conditionally linear 
  parameters will be equated if they represent the same condition (e.g., a 
  wavelength) within \code{thresh}.  For example, 
  \code{linkclp = list(1:10, 11:15)} will let datasets 1-10 and 11-15 
  have the same clp.  Note that if \code{linkclp} is not given, it will 
  default to \code{list{1:length(data)}}, so that the clp from all datasets
  are equated when they represent conditions within \code{thresh} of each 
  other.  

  Consider the situation where the clp from many different datasets are 
  equated.  \bold{
  Then it is important to note that the specification of constraints
  applicable to the clp will also be equated, and will be read from the model
  assigned to the first dataset in the group. }

  }  
  \item{dscal}{ list of lists specifying linear scaling relations between 
   datasets; each list has elements \code{to, from, value}.  The index of the 
   dataset to be scaled is given in \code{to}; the index of the dataset on 
   which the scaling is to be based is given in \code{from}.  The starting 
   value parameterizing the relationship is given as \code{value}. For 
   example, \code{dscal = list(list(to=2,from=1,value=.457))}. }    
   \item{thresh}{ numeric describing the tolerance with which clp from 
   different datasets are to be considered as equal.  
   For instance, for 
   two datasets containing data at 750 and 751 nm, respectively, 
   \code{thresh=1.5} will equate the clp at 750 and 751 between datasets.
   Specify a negative
   value of \code{thresh} to estimate clp per-dataset. 
   See Section 2.2 of the paper in the references for the model equations. } 
  \item{free}{ list of lists specifying individual 
  parameters to free for a given dataset. each sublist has 
  named elements 
  \describe{ 
  \item{what}{character string naming parameter type, e.g., "kinpar"}
  \item{ind}{vector of indices into parameter vector or list, e.g.,
  \code{c(2,3)} or \code{4}}
  \item{dataset}{dataset index in which parameter is to be freed}
  \item{start}{starting value for freed parameter}
  }  
  For example, \code{free = list(
list(what = "irfpar", ind = 1, dataset = 2, start=-.1932),
list(what = "kinpar", ind = 5, dataset = 2, start=.0004),
list(what = "kinpar", ind = 4, dataset = 2, start= .0159))}. 
  }
   \item{remove}{ list of lists specifying individual 
  parameters to remove from 
  parameter groups for a given dataset. each sublist has 
  named elements 
   \itemize{ 
   \item{what}{ character string naming parameter type, e.g., "kinpar"}
   \item{dataset}{ dataset index in which parameter group is to be removed}
   \item{ind}{ vector of indices into parameter vector or list, e.g.,
    \code{c(2,3)} or \code{4} where parameter should be removed }
  }
  }  
  \item{add}{ list of lists specifying individual 
  parameters to add to parameter groups for a given dataset. each sublist has 
  named elements 
   \itemize{ 
   \item{what}{ character string naming parameter type, e.g., "kinpar"}
   \item{dataset}{ dataset index in which parameter group is to change}
   \item{start}{ starting value for added parameter}
   \item{ind}{ vector of indices into parameter vector or list, e.g.,
    \code{c(2,3)} or \code{4} where parameter should be added. }
  }
  }  
  \item{change}{list of lists specifying entire parameter groups to change 
  for a given dataset.
  each sublist has named elements 
  \itemize{ 
   \item{what}{ character string naming parameter type, e.g., "kinpar"}
   \item{dataset}{ dataset index in which parameter group is to change}
   \item{spec}{ new specification that in initModel would follow "what", 
   e.g., for \code{c(.1, .3)} if what="kinpar"}
  }}
  \item{rel}{ 
  list of lists specifying parameters to relate between datasets
  each sublist has named elements 
  \itemize{ 
    \item{what1}{ character string naming parameter type to be determined in 
      relation to some other parameter type , e.g., "kinpar"}
    \item{what2}{ character string naming parameter type on which another 
      parameter type is to depend, e.g., "kinpar"}
    \item{ind1}{ vector of indices into parameter vector or 
      list, e.g., \code{c(2,3)} or \code{4} of the dependent parameter. }
    \item{ind2}{ vector or numeric of indices into parameter vector or 
      list, e.g., \code{c(2,3)} or \code{4} of the parameter on which another 
      parameter will depend }
    \item{dataset1}{ dataset index of the dependent parameter}
    \item{dataset2}{ dataset index of the parameter on which another parameter
      will depend}
    \item{rel}{ optional character string describing functional relationship 
      between parameters; defaults to "lin" for linear relationship }
    \item{start}{ starting value or vector of values parameterizing 
      relationship between parameters  }
  }}
  \item{weightlist}{ List of new weights for the datasets returned
  by the function \code{outlierCorrs} (as the element
  \code{weightList} of the list that is the return value of this function). 
  }
  }
 }
  \item{opt}{Object of class \code{kinopt} or \code{specopt} specifying 
  fitting and plotting options. }
  \item{lprogress}{Logical specifying 
  whether textual output of fitting progress is returned }
}
\details{
This function applies the \code{\link{nls}} function internally to 
optimize nonlinear parameters and to solve for conditionally linear parameters
(clp) via the partitioned variable projection algorithm. 
}
\value{ A list is returned containing the following elements: 
\itemize{
  \item{currTheta}{ is a list of objects
    of class \code{theta} whose elements contain the parameter estimates
    associated with each dataset modeled.}
  \item{currModel}{ is an object of class \code{multimodel}
    containing the results of fitting as well as the model
    specification}
\item{toPlotter}{ is a list containing all arguments used by the plotting 
function; it is used to regenerate plots and other output by the 
\code{examineFit}  function}
\item{nlsprogress}{ if lprogress = TRUE textual output of the fitting progress 
is returned as an array of strings}
}}

\references{Mullen KM, van Stokkum IHM (2007). 
``TIMP: an R package for modeling
multi-way spectroscopic measurements.'' Journal of Statistical Software,
18(3). \url{http://www.jstatsoft.org/v18/i03/.}} 
\author{Katharine M. Mullen, Ivo H. M. van Stokkum} 
\examples{

## 2 simulated concentration profiles in time 
C <- matrix(nrow = 51, ncol = 2)
k <- c(.5, 1)
t <- seq(0, 2, by = 2/50) 
C[, 1] <- exp( - k[1] * t) 
C[, 2] <- exp( - k[2] * t) 

## 2 simulated spectra in wavelength
E <- matrix(nrow = 51, ncol = 2)
wavenum <- seq(18000,28000, by=200)
location <- c(25000, 20000) 
delta <- c(5000, 7000)
amp <- c(1, 2)  
E[, 1] <- amp[1] * exp( - log(2) * (2 * (wavenum - location[1])/delta[1])^2)
E[, 2] <- amp[2] * exp( - log(2) * (2 * (wavenum - location[2])/delta[2])^2)

## simulated time-resolved spectra 
sigma <- .001
Psi_q  <- C \%*\% t(E) + sigma * rnorm(nrow(C) * nrow(E))

## as an object of class dat 
Psi_q_data <- dat(psi.df = Psi_q, x = t, nt = length(t), x2 = wavenum, nl =
length(wavenum)) 

## model for the data in the time-domain 
kinetic_model <- initModel(mod_type = "kin", seqmod = FALSE, 
kinpar = c(.1, 2)) 

## fit the model 
kinetic_fit <- fitModel(data = list(Psi_q_data), 
modspec = list(kinetic_model), opt = kinopt(iter=4, plot=FALSE))
}
\seealso{ \code{\link{readData}}, \code{\link{initModel}}, 
\code{\link{examineFit}} }
\keyword{file}
