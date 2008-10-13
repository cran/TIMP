\name{divergeZimage}
\alias{divergeZimage}
\title{Plots a matrix with a diverging palette, with the center value of
the palettle possible to set}
\description{An image plot of a matrix is a way of visualizing data;
  when the data represents a quantity like transient absorption, where
  negative values represent a different phenomena than positive values,
  it can be useful to set values at zero in the image plot to grey,
  whereas positive values are assigned to red, and negative 
 values are assigned to blue.  Alternately, when comparing image plots
 of several matrices, it may be useful to set the value assigned to grey
 uniformly, with values above this threshold assigned to red, and below
 this threshold assigned to blue. 
}
\usage{
divergeZimage(ob, out=FALSE, file="divergeZimage.pdf",
                            lin = 1, title = "", center = 0,
                            x2 = vector(), x= vector(),
                            plainmat = FALSE, ylab="wavelength (nm)",
                            xlab = "time (ns)") 

}
\arguments{
\item{ob}{either an object of class \code{dat} or a numeric matrix; if a
numeric matrix is given then set \code{plainmat=TRUE} and specify labels
for the columns of matrix in \code{x2} and for the rows of the matrix in
\code{x} }
\item{out}{a logical indicating whether to write to the screen in the
case that this is possible or to a file; if \code{TRUE}, writes to a pdf
file} 
\item{file}{a character vector giving a filename to write to in the case
that \code{out=TRUE}}
\item{lin}{range of \code{x} to plot linearly; values not
between \code{-lin} and \code{lin} are plotted on a log scale} 
\item{title}{character vector giving a title for the plot}
\item{center}{point assigned to grey in the diverging palette.}
\item{x2}{vector of labels for the columns of the matrix; used only if
\code{plainmat=TRUE}}
\item{x}{vector of labels for the rows of the matrix; used only if
\code{plainmat=TRUE}}
\item{plainmat}{logical indicating whether \code{ob} is a matrix, as
opposed to an object of class \code{dat} }
\item{ylab}{character vector giving a label to put on the y-axis}
\item{xlab}{character vector giving a label to put on the x-axis}
}
\examples{

exd <- dat(psi.df=matrix(rnorm(1000), ncol=100, nrow=100),
x=1:100,x2=1:100, nl=as.integer(100), nt=as.integer(100))

## by default linear range until 1 is used, logarithmic thereafter
divergeZimage(exd)

## can change this as desired
divergeZimage(exd, lin=10, title="plot linearly to 10")

}

\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{ \code{\link{dat}} }
\keyword{hplot}

