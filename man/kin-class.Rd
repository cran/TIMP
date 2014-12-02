\name{kin-class}
\docType{class}
\alias{kin-class}
\alias{kin}
\title{Class "kin" for kinetic model storage.}
\description{ 
\code{kin} is the class for kinetic models; 
an object 
of class "kin" is initialized if 
\code{mod_type = "kin"} is an 
argument of \code{initModel}.
All objects of class \code{kin} are sub-classes of 
class \code{dat}; see documentation for \code{dat} 
for a description of 
these slots.  }  
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("kin", ...)} or 
 \code{kin(...)}.  Slots whose
description are marked with *** may
be specified in the \code{...} 
argument of the  \code{\link{initModel}} function.
}
\section{Slots}{
  \describe{
    \item{anipar}{}
    \item{anispec}{}
    \item{autoclp0}{}
    \item{C2}{}
    \item{chinde}{}
    \item{clinde}{}
     \item{clp0}{}
    \item{clpCon}{}
    \item{clpdep}{}
    \item{clpequ}{}
    \item{clpequspecBD}{}
    \item{clpType}{}
    \item{cohcol}{}
    \item{cohirf}{}
    \item{datafile}{}
    \item{datCall}{}
    \item{drel}{}
    \item{dscalspec}{}
    \item{E2}{}
    \item{fixed}{}
    \item{fixedkmat}{}
    \item{free}{}
    \item{fvecind}{}
    \item{getX}{}
    \item{getXsuper}{}
    \item{highcon}{}
    \item{inten}{}
    \item{kin2scal}{}
    \item{kinpar2}{}
    \item{kinscalspecial}{}
    \item{kinscalspecialspec}{}
    \item{lclp0}{}
    \item{lclpequ}{}
    \item{title}{}
    \item{parnames}{}
    \item{prel}{}
    \item{prelspec}{}
    \item{psi.df}{}
    \item{psi.weight}{}
    \item{pvecind}{}
    \item{satMat}{}
    \item{scalx}{}
    \item{usecompnames0}{}
    \item{usecompnamesequ}{}
    \item{usekin2}{}
    \item{weight}{}
    \item{weightList}{}
    \item{weightM}{}
    \item{weightpar}{}
    \item{weightsmooth}{}
    \item{x}{}
    \item{x2}{}
    \item{clpequspec}{}
    \item{compnames}{}
    \item{constrained}{}
    \item{iter}{}
    \item{lightregimespec}{}
    \item{lowcon}{}
    \item{makeps}{}
    \item{mhist}{}
    \item{mod_type}{}
    \item{mvecind}{}
    \item{ncomp}{}
    \item{nl}{}
    \item{nt}{}
    \item{nvecind}{}
    \item{outMat}{}
    \item{positivepar}{}
    \item{sigma}{}
    \item{simdata}{}
    \item{speckin2}{}
    \item{kinpar}{*** vector of rate constants to be used as starting 
      values for the exponential decay of components; the length of this
	vector determines the number of components of the kinetic
	model. }		 
      \item{\code{specpar}:}{*** Object of class \code{"list"}  parameters for
	spectral constraints}
    \item{\code{seqmod}:}{*** Object of class \code{"logical"}  that is \code{TRUE} if a sequential model is to be applied
and \code{FALSE} otherwise}
    \item{\code{irf}:}{Object of class \code{"logical"}   that is \code{TRUE} is an IRF is modeled and \code{FALSE} otherwise}
    \item{\code{mirf}:}{Object of class \code{"logical"}  that is \code{TRUE} if a measured IRF is modeled and \code{FALSE}
otherwise}
    \item{\code{measured_irf}:}{*** Object of class \code{"vector"}  containing a measured IRF}
    \item{\code{convalg}:}{*** Object of class \code{"numeric"}  1-3 determining the numerical convolution algorithm 
used in the case of modeling a measured IRF; if \code{3} then supply a
reference lifetime in the slot \code{reftau}.}
 \item{\code{reftau}:}{*** Object of class \code{"numeric"}  containing a
 reference lifetime to be used when \code{convalg=3}}
    \item{\code{irffun}:}{*** Object of class \code{"character"}  describing the 
function to use to describe the IRF, by default "gaus"}
    \item{\code{irfpar}:}{*** Object of class \code{"vector"}  of IRF parameters; for the common Gaussian IRF this 
vector is ordered \code{c(location, width)}}
    \item{\code{dispmu}:}{Object of class \code{"logical"}   that is \code{TRUE} if dispersion of the parameter for IRF 
location is to be modeled and \code{FALSE} otherwise}
    \item{\code{dispmufun}:}{***Object of class \code{"character"}  describing the functional form of the 
dispersion of the IRF location parameter; if equal to "discrete" then the 
IRF location is shifted per element of \code{x2} and \code{parmu} should have the same 
length as \code{x2}.  defaults to a polynomial description}
    \item{\code{parmu}:}{*** Object of class \code{"list"}   of starting values for the dispersion model for the 
IRF location}
    \item{\code{disptau}:}{Object of class \code{"logical"}  that is \code{TRUE} if dispersion of the parameter for 
IRF width is to be modeled and \code{FALSE} otherwise}
    \item{\code{disptaufun}:}{*** Object of class \code{"character"}  describing the functional form of the 
dispersion of the IRF width parameter; if equal to \code{"discrete"} then the 
IRF width is parameterized per element of \code{x2} and \code{partau} should have the same 
length as \code{x2}.  defaults to a polynomial description}
    \item{\code{partau}:}{*** Object of class \code{"vector"}  of starting values for the dispersion model for the 
IRF FWHM }
    \item{\code{fullk}:}{Object of class \code{"logical"}  that is \code{TRUE} if the data are to be modeled using a 
compartmental model defined in a K matrix and \code{FALSE} otherwise}
    \item{\code{kmat}:}{*** Object of class \code{"array"}  containing the K matrix descriptive of  a compartmental 
model}
    \item{\code{jvec}:}{*** Object of class \code{"vector"}  containing the J vector descriptive of the inputs to a 
compartmental model}
    \item{\code{ncolc}:}{Object of class \code{"vector"}  describing the number of columns of the C matrix for 
each clp in \code{x2}}
    \item{\code{kinscal}:}{*** Object of class \code{"vector"}   of starting values for branching parameters in a 
compartmental model}
    \item{\code{kmatfit}:}{Object of class \code{"array"}  of fitted values for a compartmental model}
    \item{\code{cohspec}:}{*** Object of class \code{"list"}   describing the model for 
coherent artifact/scatter component(s) containing the element  \code{type}
and optionally the element \code{numdatasets}.  The element
\code{type} can be set as follows: 
	 \describe{
      \item{\code{"irf"}:}{if \code{type="irf"}, the coherent artifact/scatter has the 
time profile of  the IRF.} 
  \item{\code{"freeirfdisp"}:}{if \code{type="freeirfdisp"}, the  coherent 
artifact/scatter has  a
Gaussian time profile whose location and width are parameterized in the 
vector \code{coh}.  }
 \item{\code{"irfmulti"}:}{if \code{type="irfmulti"} the time profile of 
the IRF is used for
the coherent artifact/scatter model, but the IRF parameters are taken per 
dataset (for the multidataset case), and the integer argument 
\code{numdatasets} must be equal to the 
number of datasets modeled.}  
 \item{\code{"seq"}:}{if \code{type="seq"} a sequential exponential decay 
model is applied, whose starting value are contained in an additional
list element \code{start}. This
often models oscillating behavior well, where the number of
oscillations is the number of parameter starting values given in
\code{start}.  The starting values after optimization will be found in
the slot \code{coh} of the object of class \code{theta} corresponding
to each dataset modeled. }
\item{\code{"mix"}:}{if \code{type="mix"} if \code{type="mix"} a sequential exponential decay 
model is applied along with a model that follows the time profile of the IRF;
the coherent artifact/scatter is then a linear superposition of these two 
models; see the above description of \code{seq} for how to supply the
starting values. }}}
    \item{\code{coh}:}{*** Object of class \code{"vector"}  of starting values for the parameterization of a  
coherent artifact}
 \item{\code{oscspec}:}{*** Object of class \code{"list"} describing the model for 
additional oscillation component(s) containing the element  \code{type}
and optionally the element \code{start}. The element \code{start} can be used to specificy the starting values for the oscillation function. The element \code{type} can be set as follows: 
   \describe{
      \item{\code{"harmonic"}:}{if \code{type="harmonic"}, the oscillation function is a damped harmonic oscillator.} }
}
    \item{\code{oscpar}:}{*** Object of class \code{"vector"}  of starting values for the oscillation parameters}
    \item{\code{wavedep}:}{Object of class \code{"logical"}  describing whether the kinetic model is dependent on
\code{x2} index (i.e., whether there is clp-dependence)}
    \item{\code{lambdac}:}{*** Object of class \code{"numeric"}  for the center wavelength to be used in a polynomial 
description of \code{x2}-dependence }
 \item{\code{amplitudes}:}{*** Object of class \code{"vector"}  
that may be used to multiply the concentrations by a square diagonal 
matrix with the number of columns that the concentration matrix has;
the diagonal is given in \code{amplitudes} and these values will be
treated as parameters to be optimized.
}
\item{\code{streak}:}{*** Object of class \code{"logical"}  
that defaults to \code{FALSE}; if \code{streak=TRUE} then 
the period of the laser is expected via \code{streakT}.}
\item{\code{streakT}:}{*** Object of class \code{"numeric"}  
the period of the laser; this will be used to add a 
backsweep term to the concentration matrix and should be 
set in conjunction \code{streak=TRUE}.} 
\item{\code{doublegaus}:}{*** Object of class \code{"logical"}  
that defaults to \code{FALSE} and determines whether a double
Gaussian should be used to model the IRF. If   \code{doublegaus=TRUE}
then \code{irfpar} should contain four numeric values corresponding to
the location (mean) of the IRF, the FWHM of the first Gaussian, the 
FWHM of the second Gaussian, and the relative amplitude of the 
second Gaussian, respectively. 
}
\item{\code{multiplegaus}:}{*** Object of class \code{"logical"}  
that defaults to \code{FALSE} and determines whether multiple
Gaussians should be used to model the IRF. If \code{multiplegaus=TRUE}
then \code{irfpar} should contain: two numeric values corresponding to
the location (mean) and the FWHM of the first Gaussian of the IRF, and
three numeric values for \bold{each} additional gaussian modeled, corresponding to
the relative scaling to the first gaussian, the shift (in time) relative to the
first gaussian and the FWHM of the additonal Gaussian, respectively. 
}
\item{\code{numericalintegration}:}{*** Object of class \code{"logical"}  
that defaults to \code{FALSE} and determines whether a kinetic theory
model of a reaction mechanism should be numerically integrated (using 
\pkg{deSolve}) to find the concentrations. 
If \code{numericalintegration=TRUE} then 
\code{initialvals} should specify the initial concentrations and
\code{reactantstoichiometrymatrix} and 
\code{stoichiometrymatrix} should specify the reaction mechanism, 
as per Puxty et. al. (2006).
}
\item{\code{initialvals}:}{*** Object of class \code{"vector"}  
giving the concentrations at the initial time step.
}
\item{\code{reactantstoichiometrymatrix}:}{*** Object of class \code{"vector"}  
giving the (integer) \dfn{stoichiometric coefficients} for the reactants; this
is the matrix \bold{X}r of Puxty et. al. (2006) with \code{dim=NULL}.
}
\item{\code{stoichiometrymatrix}:}{*** Object of class \code{"vector"}  
giving the (integer) stoichiometric coefficients for the reactions; this 
is the matrix \bold{X} of Puxty et. al. (2006) with \code{dim=NULL}.
}

}}
\section{Extends}{
Class \code{\link{dat-class}}, directly.
}

\details{See \code{\link{dat-class}} for an 
example of the initialization of a 
\code{kin} object via the \code{initModel} function. } 
\examples{ 
## Example in modeling second order kinetics, by
## David Nicolaides.

## On simulated data.

##############################
## load TIMP
##############################

library("TIMP")

##############################
## SIMULATE DATA
##############################

## set up the Example problem, a la in-situ UV-Vis spectroscopy of a simple
## reaction.
## A + 2B -> C + D, 2C -> E

cstart <- c(A = 1.0, B = 0.8, C = 0.0, D = 0.0, E = 0.0)
times <- c(seq(0,2, length=21), seq(3,10, length=8))
k <- c(kA = 0.5, k2C = 1)

## stoichiometry matrices

rsmatrix <- c(1,2,0,0,0,0,0,2,0,0)
smatrix <- c(-1,-2,1,1,0,0,0,-2,0,1)
concentrations <- calcD(k, times, cstart, rsmatrix, smatrix)

wavelengths <- seq(500, 700, by=2)
spectra <- matrix(nrow = length(wavelengths), ncol = length(cstart))
location <- c(550, 575, 625, 650, 675)
delta <- c(10, 10, 10, 10, 10)
spectra[, 1] <- exp( - log(2) *
(2 * (wavelengths - location[1])/delta[1])^2)
spectra[, 2] <- exp( - log(2) *
(2 * (wavelengths - location[2])/delta[2])^2)
spectra[, 3] <- exp( - log(2) *
(2 * (wavelengths - location[3])/delta[3])^2)
spectra[, 4] <- exp( - log(2) *
(2 * (wavelengths - location[4])/delta[4])^2)
spectra[, 5] <- exp( - log(2) *
(2 * (wavelengths - location[5])/delta[5])^2)

sigma <- .001
Psi_q <- concentrations \%*\% t(spectra) + sigma *
  rnorm(dim(concentrations)[1] * dim(spectra)[1])

## store the simulated data in an object of class "dat"
kinetic_data <- dat(psi.df=Psi_q , x = times, nt = length(times),
 x2 = wavelengths, nl = length(wavelengths))

##############################
## DEFINE MODEL 
##############################

## starting values
kstart <- c(kA = 1, k2C = 0.5)

## model definition for 2nd order kinetics
kinetic_model <- initModel(mod_type = "kin", seqmod = FALSE,
                           kinpar = kstart,
                           numericalintegration = TRUE,
                           initialvals = cstart, 
                           reactantstoichiometrymatrix = rsmatrix, 
                           stoichiometrymatrix = smatrix )

##############################
## FIT INITIAL MODEL 
## adding constraints to non-negativity of the
## spectra via the opt option nnls=TRUE
##############################

kinetic_fit <- fitModel(data=list(kinetic_data),
                        modspec = list(kinetic_model),
                        opt = kinopt(nnls = TRUE, iter=80,
                        selectedtraces = seq(1,kinetic_data@nl,by=2)))

## look at estimated parameters

parEst(kinetic_fit)

## various results

## concentrations 

conRes <- getX(kinetic_fit)

matplot(times, conRes, type="b", col=1,pch=21, bg=1:5, xlab="time (sec)",
        ylab="concentrations", main="Concentrations (2nd order kinetics)")
                        
                        
## spectra 

specRes <- getCLP(kinetic_fit)

matplot(wavelengths, specRes, type="b", col=1,pch=21, bg=1:5,
        xlab="wavelength (nm)",
        ylab="amplitude", main="Spectra")

## see help(getResults) for how to get more results information from
## kinetic_fit
}

\references{
Puxty, G., Maeder, M., and Hungerbuhler, K. (2006) Tutorial on the fitting of 
kinetics models to mulivariate spectroscopic measurements 
with non-linear least-squares regression, \emph{Chemometrics and Intelligent 
Laboratory Systems} \bold{81}, 149-164.
}

\author{Katharine M. Mullen, David Nicolaides, Ivo H. M. van Stokkum}
\seealso{
\code{\link{dat-class}}, \code{\link{spec-class}}
}
\keyword{classes}
