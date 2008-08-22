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
    plotSelIntenImage(multimodel, t, i=1, tit=c("Region of Interest"), cex=1)
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
\author{Katharine M. Mullen, Sergey Laptenok, Ivo H. M. van Stokkum}
\seealso{ \code{\link{fitModel}} }
\keyword{hplot}
