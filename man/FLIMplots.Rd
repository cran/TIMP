\name{FLIMplots}
\alias{FLIMplots}
\alias{plotHistAmp}
\alias{plotHistNormComp}
\alias{plotIntenImage}
\alias{plotSelIntenImage}
\alias{plotNormComp}
\alias{plotTau}
\title{Functions to plot FLIM results. }
\description{Functions to plot FLIM results. }
\usage{
    plotHistAmp(multimodel, t, i=1)
    plotHistNormComp(multimodel, t, i=1)
    plotIntenImage(multimodel, t, i=1,  tit=c("Intensity Image"))
    plotSelIntenImage(multimodel, t, i=1, tit=c("Region of Interest"),
     cex=1)
    plotTau(multimodel, t, i=1, tit=" < tau > ", plotoptions=kinopt(),
     lifetimes=TRUE)
    plotNormComp(multimodel, t, i=1)
}
\arguments{
  \item{multimodel}{the \code{currModel} element of the list
    returned by \code{fitModel} }
  \item{t}{the \code{currTheta} element of the list
    returned by \code{fitModel} }
  \item{i}{dataset index to make plot for}
  \item{tit}{Character vector giving the title}
  \item{plotoptions}{object of class \code{kinopt} giving the plotting
    options}
  \item{cex}{A numerical value giving the amount by which plotting text
    and symbols should be magnified relative to the default}
  \item{lifetimes}{A logical value indicating whether the averages
    per-pixel should be for lifetimes or their inverse, decay rates.  }

}
\value{No return value, called for side effects}
\examples{
\donttest{
##############################
## READ IN DATA,  PREPROCESS DATA
##############################

## data representing only donor tagged

data("donorTagged")

D1 <- preProcess(c001, sel_time=c(25,230))
D2 <- preProcess(c003, sel_time=c(25,230))

## data representing donor-acceptor tagged

data("donorAcceptorTagged")

DA1 <- preProcess(cy005c, sel_time=c(25,230))
DA2 <- preProcess(cy006, sel_time=c(25,230))

##############################
## READ IN MEASURED IRF,  PREPROCESS IRF
##############################

data("mea_IRF")
mea_IRF <- baseIRF(mea_IRF, 100, 150)[25:230]

##############################
## SPECIFY INITIAL MODEL
##############################

modelC <- initModel(mod_type = "kin",
## starting values for decays
kinpar=c(1.52, 0.36),
## numerical convolution algorithm to use
convalg = 2,
## measured IRF
measured_irf = mea_IRF,
lambdac = 650,
## shift of the irf is fixed
parmu = list(0), fixed = list(parmu=1),
## one component represents a pulse-following with the IRF shape
cohspec = list(type = "irf"),
## parallel kinetics
seqmod=FALSE,
## decay parameters are non-negative
positivepar=c("kinpar"),
title="Global CFP bi-exp model with pulse-follower")

##############################
## FIT MODEL FOR DONOR ONLY DATA
##############################

fitD <- fitModel(list(D1,D2),
                 list(modelC),
                 ## estimate the linear coeefficients per-dataset
                 modeldiffs = list(linkclp=list(1,2)),
                 opt=kinopt(iter=1, linrange = 10,
                   addfilename = TRUE,
                   output = "pdf",
                   makeps = "globalD",
                   notraces = TRUE,
                   selectedtraces = seq(1, length(c001@x2), by=11),
                   summaryplotcol = 4, summaryplotrow = 4,
                   ylimspec = c(1, 2.5),
                   xlab = "time (ns)", ylab = "pixel number",
                   FLIM=TRUE))

##############################
## FIT MODEL FOR DONOR-ACCEPTOR DATA
##############################

fitDA <- fitModel(list(DA1,DA2),
                  list(modelC),
                  ## estimate the linear coeefficients per-dataset
                 modeldiffs = list(linkclp=list(1,2)),
                 opt=kinopt(iter=1, linrange = 10,
                   addfilename = TRUE,
                   output = "pdf",
                   makeps = "globalDA",
                   notraces = TRUE,
                   selectedtraces = seq(1, length(c001@x2), by=11),
                   summaryplotcol = 4, summaryplotrow = 4,
                   ylimspec = c(1, 2.5),
                   xlab = "time (ns)", ylab = "pixel number",
                   FLIM=TRUE))

##############################
## COMPARE THE DECAY RATES
##############################

parEst(fitD)

parEst(fitDA)

##############################
## ADDITIONAL FIGURES
##############################
oldpar <- par(no.readonly = TRUE)

par(mfrow=c(2,2), mar=c(1,3,1,12))

par(cex=1.5)
plotIntenImage(fitD$currModel, fitD$currTheta, 1, tit="")

par(cex=1.5)
plotIntenImage(fitDA$currModel, fitD$currTheta, 1, tit="")

par(cex=1.5)
plotIntenImage(fitD$currModel, fitD$currTheta, 2, tit="")

par(cex=1.5)
plotIntenImage(fitDA$currModel, fitD$currTheta, 2, tit="")

par(oldpar)
###############

plo <- kinopt(ylimspec = c(.25,1.1), imagepal=grey(seq(1,0,length=100)))

par(mfrow=c(2,2), mar=c(1,3,1,12))

par(cex=1.5)
plotTau(fitD$currModel, fitD$currTheta, 1, tit="",plotoptions=plo,
        lifetimes=FALSE)

par(cex=1.5)
plotTau(fitDA$currModel, fitD$currTheta, 1, tit="",plotoptions=plo,
        lifetimes=FALSE)

par(cex=1.5)
plotTau(fitD$currModel, fitD$currTheta, 2, tit="",plotoptions=plo,
        lifetimes=FALSE)

par(cex=1.5)
plotTau(fitDA$currModel, fitD$currTheta, 2, tit="", plotoptions=plo,
        lifetimes=FALSE)

par(oldpar)
} # end donttest

##############################
## CLEANUP GENERATED FILES
##############################
# This removes the files that were generated in this example
# (do not run this code if you wish to inspect the output)
file_list_cleanup = c('globalDA_paramEst.txt', 'globalDA_spec_dataset_1.txt',
  'globalDA_spec_dataset_2.txt', 'globalD_paramEst.txt',
  'globalD_spec_dataset_1.txt', 'globalD_spec_dataset_2.txt',
  Sys.glob("*paramEst.txt"), Sys.glob("*.ps"), Sys.glob("Rplots*.pdf"))

# Iterate over the files and delete them if they exist
for (f in file_list_cleanup) {
  if (file.exists(f)) {
    unlink(f)
  }
}


}
\author{Katharine M. Mullen, Sergey Laptenok, Ivo H. M. van Stokkum}
\seealso{ \code{\link{fitModel}} }
\keyword{hplot}
