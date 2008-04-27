\name{dat-class}
\docType{class}
\alias{dat-class}
\alias{dat}
\title{Class "dat" for model and data storage }
\description{	 
\code{dat} is the super-class of other classes representing models and data, so
that other model/data classes (e.g., \code{kin} and \code{spec} 
for kinetic and spectral
models respectively) also have the slots defined here.  These slots may
be specified in the \code{...} 
argument of the  \code{\link{initModel}} function.
}
\section{Objects from the Class}{
Objects from the class 
can be created by calls of the form \code{new("dat", ...)} or 
\code{dat(...)}, but 
most are most often made by invoking another function such as 
\code{readData} or \code{initModel}. 
}
\section{Slots}{
	 \describe{
    \item{\code{psi.df}:}{ Object of class \code{"matrix"}  dataset from 1 experiment}
    \item{\code{psi.weight}:}{Object of class \code{"matrix"} weighted  dataset from 1 experiment}
    \item{\code{x}:}{Object of class \code{"vector"} time or other independent
    variable.  }
    \item{\code{nt}:}{Object of class \code{"integer"} length \code{x}}
    \item{\code{x2}:}{Object of class \code{"vector"}  vector of points in 
    2nd independent dimension, such as wavelengths of wavenumbers}
    \item{\code{nl}:}{Object of class \code{"integer"}  length \code{x2} }
    \item{\code{C2}:}{Object of class \code{"matrix"} concentration matrix for
    simulated data}
    \item{\code{E2}:}{Object of class \code{"matrix"} matrix of spectra for
    simulated data}
    \item{\code{sigma}:}{Object of class \code{"numeric"} noise level in 
    simulated data }
    \item{\code{mod_type}:}{ Object of class \code{"character"} character string defining the model type, e.g., 
  \code{"kin"} or \code{"spec"}}
    \item{\code{parnames}:}{Object of class \code{"vector"} vector of
    parameter names, used internally}
    \item{\code{finished}:}{Object of class \code{"logical"} describes 
    whether optimization is complete}
    \item{\code{simdata}:}{ Object of class \code{"logical"}  logical that is \code{TRUE} if the data is simulated, 
  \code{FALSE} otherwise; will determine whether values in \code{C2} and 
  \code{E2} are plotted with results}
    \item{\code{weightpar}:}{ Object of class \code{"list"}  list of vectors 
  \code{c(first_x, last_x, first_x2, last_x2, weight)}, 
  where each vector is of length 5 and 
  specifies an interval in which to weight the data.  
  \itemize{
  \item{first\_x}{first(absolute, not an index) \code{x} to weight}
  \item{last\_x}{last (absolute, not an index) \code{x} to weight} 
  \item{first\_x2}{first (absolute, not an index)  \code{x2} to weight}
  \item{last\_x2}{last (absolute, not an index)  \code{x2} to weight} 
  \item{weight}{numeric by which to weight data} 
  }
  Note that if vector elements 1-4 are \code{NA} (not a number), the firstmost 
  point of the data is taken for elements 1 and 3, and the lastmost points
  are taken for 2 and 4. 
  For example, \code{ weightpar = list(c(40, 1500, 400, 600, .9), 
  c(NA, NA, 700, 800, .1))}  will weight data between times 40 and 1500 
  picoseconds and 700 and 800 wavelengths by .9, and will weight data at 
  all times between wavelength 700 and 800 by .1.  
  Note also that for single photon counting data 
  \code{weightpar = list(poisson = TRUE)} will apply poisson weighting to all 
  non-zero elements of the data. }
    \item{\code{weight}:}{Object of class \code{"logical"} \code{TRUE} when the specification in 
  \code{weightpar} is to be applied and \code{FALSE} otherwise}
    \item{\code{weightM}:}{Object of class \code{"matrix"} weights }
    \item{\code{weightsmooth}:}{Object of class \code{"list"} type of smoothing to apply with weighting; not currently
  used}
    \item{\code{fixed}:}{ Object of class \code{"list"} list of lists or vectors giving the parameter values 
  to fix (at their starting values) during optimization. }
 \item{\code{free}:}{ Object of class \code{"list"} list of lists or vectors giving the parameter values 
  to free during optimization; if this list is present then all parameters
not specified in it are fixed, e.g., \code{free = list(irfpar = 2)} will fix
every parameter at its starting value except for the 2nd \code{irfpar}.  If 
\code{fix = list(none=TRUE)}  (or if the element \code{none}  has length
greater than 0) then all parameters in the model are fixed.  Note that this
option only should be applied to multiexperiment models in which at least one 
parameter appling to some other dataset is optimized (\code{nls} always must 
have at least one parameter to optimize).      }
 \item{\code{constrained}:}{ Object of class \code{"list"} list whose elements
 are lists containing a character vector \code{what}, a vector \code{ind}, and
 either (but not both) a character vector \code{low} and \code{high}.
 \code{what} should specify the parameter type to constrain.  \code{ind}
 should give the index of the parameter to be constrained, e.g., \code{1} if
 indexing into a vector, and \code{c(1,2)} if indexing into a list.
 \code{low} gives a number that the parameter should always remain lower
 than and \code{high} gives a number that the parameter should always remain
 higher than (so that \code{low} bounds the parameter value from above and
 \code{high} bounds the parameter value from below).  It is not now possible to
 specify both \code{low} and \code{high} for a single parameter value.  An
 example of a complete \code{constrained} specification is 
 \code{constrained = list(list("what = "kinpar", ind = 2, low = .3), 
list(what = "parmu", ind = c(1,1), high = .002))} 
}
    \item{\code{clp0}:}{ Object of class \code{"list"}  list of lists with elements \code{low}, \code{high} and 
  \code{comp}, specifying the least value in  \code{x2} to constrain 
  to zero, the greatest value in  \code{x2} to 
  constrain to zero, and the component to which to apply the zero constraint, 
  respectively.  e.g., \code{clp0 = list(list(low=400, high = 600, comp=2), 
  list(low = 600, high = 650, comp=4))} applies zero constraints to the spectra 
  associated with components 2 and 4. }
\item{\code{autoclp0}:}{ Object of class \code{"list"} that has two
  elements; \code{oldRes},
  the output of \code{fitModel} and an index \code{ind} representing the
  index of the dataset to use in \code{oldRes}; \code{ind} defaults to
  one.   The clp that are negative in \code{oldRes} are constrained to
  zero in the new model; this is primarily useful when fitting a model,
  finding some negative clp, and constraining them to zero by fitting
  again with this option.  See  also the help page for \code{opt} for
  other ways to constrain the clp to non-negativity. 
  }
\item{\code{makeps}:}{ Object of class \code{"character"} 
  specifyies the prefix of files written to 
  postscript
    }
    \item{\code{clpequspec}:}{ Object of class \code{"list"} list of lists each of which has elements \code{to, 
  from, low, high}, and optional element \code{dataset} to specify the dataset
  from which to get the reference clp (that is, a spectrum for kinetic 
  models).  \code{to} is the component to be fixed in relation to some other 
  component; from is the reference component.  \code{low} and \code{high} 
  are the 
  least and greatest absolute values of the \code{clp} vector to constrain. 
  e.g., 
  \code{clpequspec = list(list(low = 400, high = 600, to = 1, from = 2))} 
  will constrain the first component to equality to the second component 
  between wavelengths 400 and 600.  Note that equality constraints are
  actually constraints to a linear relationship.  For each of the equality
  constraints specfied as a list in the \code{clpequspec} list, specify a
  starting value parameterizing this linear relation in the vector
  \code{clpequ}; if true equality is desired then fix the corresponding
  parameter in \code{clpequ} to 1.  Note that if multiple components are
  constrainted, the \code{from} in the sublists should be increasing order, 
  (i.e., \code{(list(to=2, from=1, low=100, high=10000), 
list(to=3, from=1, low=10000, high=100))}, not \code{list(to=3, from=1, low=10000, high=100), 
list(to=2, from=1, low=10000, high=100)})}
    \item{\code{lclp0}:}{Object of class \code{"logical"} \code{TRUE} if specification in \code{clp0} 
  is to be applied and \code{FALSE} otherwise }
    \item{\code{lclpequ}:}{Object of class \code{"logical"} \code{TRUE} if specification in clpequspec 
  is to be applied and \code{FALSE} otherwise}
    \item{\code{title}:}{ Object of class \code{"character"} displayed on output plots}
    \item{\code{mhist}:}{Object of class \code{"list"}  list describing fitting history}
    \item{\code{datCall}:}{Object of class \code{"list"} list of calls to functions}
   \item{drel}{ vector of starting parameters for dataset scaling relations}
     \item{\code{dscalspec}:}{ Object of class \code{"list"} }
    \item{\code{drel}:}{ Object of class \code{"vector"}  vector of starting parameters for dataset scaling relations}
    \item{\code{clpequ}:}{Object of class \code{"vector"}  describes the
    parameters governing the clp equality constraints specified in \code{clpequspec}}
    \item{\code{scalx}:}{ Object of class \code{"numeric"}  numeric by which to scale the \code{x} axis in plotting}
  \item{prel}{vector of starting values for the relations described in 
  prelspec}
    \item{\code{prel}:}{ Object of class \code{"vector"}  vector of starting values for the relations described in 
  prelspec}
    \item{\code{prelspec}:}{ Object of class \code{"list"}  list of lists to specify the functional
  relationship between parameters, each of which has elements 
  \itemize{ 
  \item{what1}{character string describing the parameter type to relate, 
  e.g., \code{"kinpar"}} 
  \item{what2}{the parameter type on which the relation is based; usually 
  the same as \code{what1}}
  \item{ind1}{index into \code{what1}}
  \item{ind2}{index into \code{what2}}
  \item{rel}{character string, 
  optional argument to specify functional relation type, 
  by default linear} }
   e.g., 
  \code{prelspec = list(list(what1 = "kinpar", what2 = "kinpar", ind1 = 1, 
  ind2 = 5))}  relates the 1st element of \code{kinpar} to the 5th element of 
  \code{kinpar}.  The starting values parameterizing the relationship are 
  given in the \code{prel} vector}
    \item{\code{fvecind}:}{Object of class \code{"vector"}  vector containing indices of fixed parameters} 
    \item{\code{pvecind}:}{Object of class \code{"vector"}  used internally to
    store indices of related parameters. }
    \item{\code{groups}:}{Object of class \code{"list"}  list containing lists of pairs c(x2 index, dataset index). 
  the \code{x2} values (which are solved for as conditionally linear 
  parameters) are equated for all pairs in a list.}
    \item{\code{iter}:}{Object of class \code{"numeric"}  describing the 
    number of iterations that is run; this is sometimes 
   stored after fitting, but has not effect as an argument to  
   \code{\link{initModel}}}
    \item{\code{clpCon}:}{Object of class \code{"list"}  used internally to enforce constraints on the clp}
    \item{\code{ncomp}:}{Object of class \code{"numeric"}  describing the number of components in a model}
    \item{\code{clpdep}:}{Object of class \code{"logical"}  describing whether a model is dependent on the index
  of \code{x2}}
    \item{\code{inten}:}{Object of class \code{"matrix"}  for use with FLIM data; represents the number of photons
per pixel measured over the course of all 
times $t$ represented by the dataset.  See the help for the \code{readData}
function for more information. }
    \item{\code{positivepar}:}{ Object of class \code{"vector"}  containing
    character strings of those parameter vectors to constrain to positivity,
    e.g., \code{positivepar=c("kinpar")}}
\item{\code{datafile}:}{Object of class \code{"character"}  
containing the name of a datafile associated with the \code{psi.df} }
\item{\code{clpType}:}{Object of class \code{"character"}  
that is "nt" if the model has clp in the "x" dimension and "nl" otherwise  
(so that, e.g., if \code{mod\_type = "kin"}, then \code{clpType = "nl"}).} 
  }
}

\author{Katharine M. Mullen, Ivo H. M. van Stokkum}  
\seealso{
\code{\link{kin-class}}, \code{\link{spec-class}} 
}
\examples{
# simulate data 

 C <- matrix(nrow = 51, ncol = 2)
 k <- c(.5, 1)
 t <- seq(0, 2, by = 2/50)
 C[, 1] <- exp( - k[1] * t)
 C[, 2] <- exp( - k[2] * t) 
 E <- matrix(nrow = 51, ncol = 2)
 wavenum <- seq(18000, 28000, by=200)
 location <- c(25000, 20000)
 delta <- c(5000, 7000)
 amp <- c(1, 2)
 E[, 1] <- amp[1] * exp( - log(2) * (2 * (wavenum - location[1])/delta[1])^2)
 E[, 2] <- amp[2] * exp( - log(2) * (2 * (wavenum - location[2])/delta[2])^2)
 sigma <- .001
 Psi_q  <- C %*% t(E) + sigma * rnorm(dim(C)[1] * dim(E)[1]) 

 # initialize an object of class dat 
 Psi_q_data <- dat(psi.df = Psi_q, x = t, nt = length(t), 
 x2 = wavenum, nl = length(wavenum))

 # initialize an object of class dat via initModel 
 # this dat object is also a kin object
 kinetic_model <- initModel(mod_type = "kin", seqmod = FALSE, 
 kinpar = c(.1, 2))
}
\keyword{classes}
