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
\examples{

##############################
## READ IN PSI 1
##############################

data(denS4)

##############################
## PREPROCESS PSI 1
##############################

denS4<-preProcess(data = denS4, scalx2 = c(3.78, 643.5))

##############################
## READ IN PSI 2
##############################

data(denS5)

##############################
## PREPROCESS PSI 2
##############################

denS5<-preProcess(data = denS5, scalx2 = c(3.78, 643.5))

##############################
## DEFINE INITIAL MODEL
##############################

model1<- initModel(mod_type = "kin", 
kinpar= c(7.9, 1.08, 0.129, .0225, .00156) , 
irfpar=c( -.1018, 0.0434), 
disptau=FALSE, dispmu=TRUE, parmu = list(c(.230)), 
lambdac = 650,
seqmod=TRUE,
positivepar=c("kinpar"),
title="S4", 
cohspec = list( type = "irf"))

##############################
## FIT INITIAL MODEL
##############################

denRes1 <- fitModel(data=list(denS4, denS5), list(model1), 
opt=kinopt(iter=5, divdrel = TRUE, linrange = .2,
makeps = "den1", selectedtraces = c(1,5,10), plotkinspec =TRUE,
output="pdf", xlab = "time (ps)", ylab = "wavelength"))

##############################
## REFINE INITIAL MODEL, RE-FIT
## adding some per-dataset parameters 
##############################

denRes2 <- fitModel(data = list(denS4, denS5), modspec = list(model1),
modeldiffs = list(dscal = list(list(to=2,from=1,value=.457)), 
free = list(
list(what = "irfpar", ind = 1, dataset = 2, start=-.1932),
list(what = "kinpar", ind = 5, dataset = 2, start=.0004), 
list(what = "kinpar", ind = 4, dataset = 2, start= .0159)
)),
opt=kinopt(iter=5, divdrel = TRUE, linrange = .2,
xlab = "time (ps)", ylab = "wavelength", output="pdf",
makeps = "den2", selectedtraces = c(1,5,10)))

##############################
## REFINE MODEL FURTHER AS NEW MODEL OBJECT 
##############################

model2 <- initModel(mod_type = "kin", 
kinpar= c(7.9, 1.08, 0.129, .0225, .00156), 
irfpar=c( -.1018, 0.0434), 
parmu = list(c(.230)), 
lambdac = 650,
positivepar=c("kinpar", "coh"), 
cohspec = list( type = "seq", start = c(8000, 1800)))

##############################
## FIT NEW MODEL OBJECT
##############################

denRes3 <- fitModel(data = list(denS4, denS5), list(model2),
modeldiffs = list(dscal = list(list(to=2,from=1,value=.457)), 
free = list(
list(what = "irfpar", ind = 1, dataset = 2, start=-.1932),
list(what = "kinpar", ind = 5, dataset = 2, start=.0004), 
list(what = "kinpar", ind = 4, dataset = 2, start= .0159)
)),
opt=kinopt(iter=5, divdrel = TRUE, linrange = .2,
makeps = "den3", selectedtraces = c(1,5,10), plotkinspec =TRUE, 
stderrclp = TRUE, kinspecerr=TRUE, output="pdf",
xlab = "time (ps)", ylab = "wavelength", 
breakdown = list(plot=c(643.50, 658.62, 677.5))))

} 
\details{For examples, see the help files for \code{\link{dat-class}} and 
\code{\link{fitModel}}} 
\seealso{ \code{\link{dat-class}}, \code{\link{kin-class}}, 
\code{\link{spec-class}}, 
\code{\link{fitModel}}}    

\keyword{file}

