\name{opt-class}
\docType{class}
\alias{opt-class}
\alias{opt}
\title{Class "opt" stores options for fitting and plotting}
\description{Class "opt" stores options for fitting and plotting applicable to
all model types}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("opt", ...)} or
\code{opt(...)}.  
}
\section{Slots}{
	 \describe{
    \item{\code{writecon}:}{Object of class \code{"logical"}  that defaults to
    FALSE; if true then 
    concentrations are written to a txt file; row labels are \code{x}}
    \item{\code{writespec}:}{Object of class \code{"logical"} that defaults to
    \code{FALSE}; if \code{TRUE} then 
    spectra are written to a txt file; row labels are \code{x2}}
    \item{\code{writenormspec}:}{Object of class \code{"logical"} that 
    defaults to \code{FALSE}; if \code{TRUE} then normalized spectra are 
    written to a txt file; row labels are \code{x2}}
    \item{\code{writefit}:}{Object of class 
    \code{"logical"} that defaults to \code{FALSE}; if \code{TRUE} then fit 
    is written to a txt file; row and column labels are \code{x} and 
    \code{x2}}
    \item{\code{output}:}{Object of class 
    \code{"character"} that defaults to \code{"ps"}, which means that 
    plots written to file are postscript.  Alternatively, specify
    \code{output = "pdf"}, and plots are written as pdf files} 
     \item{\code{addfilename}:}{Object of class 
    \code{"logical"} that, for each data file, tries to add the 
    filename to plots associated with output for that data. }
    \item{\code{residplot}:}{Object of class \code{"logical"} defaults to 
    \code{FALSE}; if \code{TRUE} generate a plot of residuals in a 
    separate window.}
    \item{\code{plot}:}{Object of class \code{"logical"} that defaults to 
    \code{TRUE}; if \code{FALSE} then do not write output in the form of 
    plots and other windows to the screen. }
    \item{\code{divdrel}:}{Object of class \code{"logical"} that defaults to 
    \code{FALSE}; if \code{TRUE}, plot traces and concentration 
   profiles divided by the dataset scaling parameters where they apply; this
   allows for the fit of datasets having different intensities on the same 
   scale.}
    \item{\code{plotkinspec}:}{Object of class \code{"logical"} that defaults 
    to \code{FALSE}; if \code{TRUE}, generates a separate plot of the spectra
    associated with the components that are not a part of a coherent
    artifact/scatter model. }
    \item{\code{superimpose}:}{Object of class \code{"vector"} containing 
    dataset indices whose results should be superimposed in plots}
    \item{\code{xlab}:}{Object of class \code{"character"} containing 
    label for x-axis, e.g., \code{"nanoseconds"}
    or \code{"picoseconds"}}
    \item{\code{ylab}:}{Object of class \code{"character"} containing label
    for y-axis, e.g., \code{"wavelength"}}
    \item{\code{title}:}{Object of class \code{"character"} containing title 
    to write at the top of plots.}
    \item{\code{makeps}:}{Object of class \code{"character"} containing 
    prefix to plot files written to postscript; 
  if present postscript will be written. Note that this string is also 
  used as the preffix of txt output files }
    \item{\code{linrange}:}{Object of class \code{"numeric"} giving linear
  range of time axis for plotting; time will be plotted linearly from
  -linrange to linrange and plotted on a logarithmic (base 10) axis elsewhere}
    \item{\code{summaryplotrow}:}{Object of class \code{"numeric"} giving  
    number of rows in summary plot; defaults to \code{4}}
      \item{\code{summaryplotcol}:}{Object of class \code{"numeric"} giving  
    number of columns in summary plot; defaults to \code{4}}
        \item{\code{iter}:}{Object of class \code{"numeric"} giving  number of iterations to optimize model parameters}
    \item{\code{paropt}:}{Object of class \code{"list"} 
    of graphical parameters in format \code{par(...)} 
    to apply to plots. }
 \item{\code{stderrclp}:}{Object of class \code{"logical"} that defaults 
    to \code{FALSE}; if \code{TRUE}, estimates of the standard error of 
    conditionally linear parameters are made}
 \item{\code{addest}:}{Object of class \code{"vector"} containing 
    character strings of which parameter estimates should be added to the 
   summary plot, e.g., \code{addest = c("kinpar", "irfpar")}}
 \item{kinspecerr}{Object of class \code{"logical"} that defaults to 
    \code{FALSE}; if \code{TRUE}, add standard error estimates to the clp
    a plot generated with \code{kinspecest=TRUE} or 
    \code{plotkinspec=TRUE}.  This option can only be 
    used if the estimates were generated during fitting via the option 
    \code{stderrclp=TRUE}}
 \item{xlimspec}{Object of class \code{"vector"} that defaults to 
\code{vector()}; if changed, it should specify the desired x-limits of 
the plot of clp}
 \item{ylimspec}{Object of class \code{"vector"} that defaults to 
\code{vector()}; if changed, it should specify the desired y-limits of 
the plot of clp. In the case of plotting the results of FLIM image 
analysis, \code{ylimspec} can be used to determine the range used in the 
image plot of lifetimes.  }
\item{ylimspecplus}{Object of class \code{"vector"} that defaults to 
\code{vector()}; if changed, the first value should specify a vector to
add to the y-limits of the plot of clp}
\item{samespecline}{Object of class \code{"logical"} that defaults to 
\code{FALSE}; if \code{TRUE}, then the line-type for clp is the same 
for all datasets
}
 \item{specinterpol}{Object of class \code{"logical"} that defaults to 
    \code{FALSE}; if \code{TRUE}, use spline instead of lines between 
    the points representing estimated clp}
   \item{specinterpolpoints}{Object of class \code{"logical"} that defaults to 
    \code{TRUE}; if \code{TRUE}, add points representing the actual estimates
    for clp to plots of the curves respresenting smoothed clp}
\item{specinterpolseg}{Object of class \code{"numeric"} that defaults to 
    \code{50}; represents the number of segments used in a spline-based
    representation of clp}  
 \item{specinterpolbspline}{Object of class \code{"logical"} that defaults
to \code{FALSE}; determines whether a B-spline based representation of
clp is used (when \code{specinterpol=TRUE}) or a piecewise polynomial 
	representation} 
 \item{normspec}{Object of class \code{"logical"} that determines whether
clp are normalized in plots}
 \item{writespecinterpol}{Object of class \code{"logical"} that defaults to 
    \code{FALSE}; if \code{TRUE}, a spline-based representation of clp
    is written to ASCII files}
}
  
}


}
\details{See \code{\link{kinopt-class}} and  \code{\link{specopt-class}} for 
the specification of fitting/plotting options that are specific to the 
class type. }
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{
\code{\link{kinopt-class}}, \code{\link{specopt-class}}
}
\keyword{classes}
