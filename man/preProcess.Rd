\name{preProcess}
\alias{preProcess}
\title{Performs preprocessing on data stored as an objects of class dat.}
\description{Performs data sampling, selection, baseline correction,  
scaling, and data correction on an object of class \code{dat}. 
}
\usage{
preProcess(data, sample = 1, sample_time = 1, sample_lambda = 1, 
    sel_time = vector(), sel_lambda = vector(), baselinetime = vector(), 
    baselinelambda = vector(), scalx = NULL, scalx2 = NULL, 
    sel_lambda_ab = vector(), sel_time_ab = vector(), rm_x2=vector(), 
    rm_x = vector(), svdResid = list(), numV = 0, sel_special = list(),
    doubleDiff = FALSE, doubleDiffFile = "doubleDiff.txt")
}
\arguments{
  \item{data}{Object of class \code{dat}}
  \item{sample}{integer describing sampling interval to take in both time and 
  \code{x2}; e.g., \code{sample=2} 
  will sample every 2nd time and every 2nd point in 
   \code{x2}.}
  \item{sample_time}{integer describing sampling interval in time; e.g.,
  \code{sample_time=2} will sample every 2nd element of the time vector. }
  \item{sample_lambda}{ integer describing sampling interval in \code{x2}; 
  e.g., \code{sample_lambda=2} will sample every 2nd element in the 
  \code{x2} vector. }
  \item{sel_time}{vector of length 2 describing the first and last time 
  index of data to select; e.g., \code{sel_time=c(5,120)} will select 
  data at times indexed 5-120. }
  \item{sel_lambda}{vector of length 2 describing the first and last \code{x2}  
  index of data to select; e.g., \code{sel_lambda=c(5,120)} 
  will select data at \code{x2}
  indexed 5-120. }
  \item{baselinetime}{a vector of form \code{c(timeIndexmin, timeIndexmax, 
  lambdaIndexmin, lambdaIndexmax)}.  The average of data between 
   \code{x2} indexes  
   \code{lambdaIndexmin} and \code{lambdaIndexmax} 
   is subtracted from data with 
   time index between \code{timeIndexmin} and \code{timeIndexmax}.  }
  \item{baselinelambda}{  a vector of form \code{c(timeIndexmin, timeIndexmax, 
  lambdaIndexmin, lambdaIndexmax)}.  The average of data between time indexes  
  \code{timeIndexmin} and \code{timeIndexmax} 
  is subtracted from data with \code{x2} index 
   between \code{lambdaIndexmin} and \code{lambdaIndexmax}.}
  \item{scalx}{numeric by which to linearly scale the \code{x} axis
  (which often represents time), so that newx = oldx * scalx }
  \item{scalx2}{ vector of length 2 by which to linearly scale the 
  \code{x2} axis, so that newx2 = oldx2 * scalx2[1] + scalx2[2]}
  \item{sel_lambda_ab}{vector of length 2 describing the absolute values
  (e.g., wavelengths, wavenumbers, etc.) between which data should be
  selected.   e.g., \code{sel_lambda_ab = c(400, 600)} will select data  
  associated with \code{x2} values between 400 and 600. } 
  \item{sel_time_ab}{ vector of length 2 describing the absolute times 
  between which data should be
  selected.   e.g., \code{sel_time_ab = c(50, 5000)} will select data  
  associated with time values between 50 and 5000 picoseconds.} 
  \item{rm_x2}{vector of \code{x2} indices to remove from the data} 
 \item{rm_x}{vector of \code{x} indices to remove from the data}
  \item{svdResid}{list returned from the \code{getResid} function, containing
  residuals to be used in data correction. } 
 \item{numV}{numeric specifying how many singular vectors to use in data
   correction.  Maximum is five. }
 \item{sel_special}{list of lists specifying \code{x} indices to remove
   from individual wavelength ranges, e.g.,
   \code{sel_special = list(c(400,600,10,12),c(600,800,11,13))}
 indicates that between wavelength 400 and 600, time indices between 10
 and 12 should be removed from the data, and between wavelengths 600 and
800, time indices between 11 and 13 should be removed from the data.
Note that the number of time indices to remove should be the same in
each wavelength interval specified.  Also note that the time vector
associated with the data after the first set of indices is removed will
be associated with the resulting dataset. }
 \item{doubleDiff}{logical indicating whether the data should be
   converted to represent differences between times. }
 \item{doubleDiffFile}{character string indicating the file name of
   time difference data to create in the case that \code{doubleDiff=TRUE}.}
}
\examples{
##############################
## READ DATA
##############################

data("target")

##############################
## PREPROCESS DATA
##############################

# select certain wavelengths for modeling 

C1_1 <- preProcess(data = C1, baselinelambda = c(1,12,1,32) )
C1_1 <- preProcess(data = C1_1, sel_lambda = c(8, 27))
C1_1 <- preProcess(data = C1_1, rm_x = c(40, 41, 101, 116))
C1_1 <- preProcess(data = C1_1, sel_time_ab = c(-10, 100000))

C2_1 <- preProcess(data = C2, sel_lambda = c(2, 32))
C2_1 <- preProcess(data = C2_1, baselinelambda = c(1,12,1,32) )
C2_1 <- preProcess(data = C2_1, sel_time_ab = c(-10, 100000))

C3_1 <- preProcess(data = C3, sel_lambda = c(1, 25))
C3_1 <- preProcess(data = C3_1, baselinelambda = c(1,12,1,32) )

\dontrun{

##############################
## SPECIFY K Matrix and J vector 
##############################


## initialize 2 7x7 arrays to 0 

delK <- array(0, dim=c(7,7,2))

## the matrix is indexed: 
## delK[ ROW K MATRIX, COL K MATRIX, matrix number] 

## in the first matrix, put the index of compartments 
## that are non-zero
## the transfer rate of the compartment is governed by
## kinpar[index] 

delK[1,1,1] <- 4
delK[5,1,1] <- 1
delK[2,2,1] <- 4
delK[5,2,1] <- 2
delK[3,3,1] <- 4
delK[5,3,1] <- 3
delK[4,4,1] <- 4
delK[6,5,1] <- 5
delK[7,6,1] <- 6
delK[7,7,1] <- 7

## print out the resulting array to make sure it's right

delK

jvector <- c(.48443195136500550341, .28740782363398824522, 
.13749071230100625137, 0.9066953510E-01, 0, 0, 0)

datalist <- list(C1, C2, C3)
## for plotting selected traces, get a vector of all the wavenumbers 
allx2 <- vector()
for(i in 1:length(datalist)) 
	allx2 <- append(allx2,datalist[[i]]@x2)
allx2 <- sort(unique(allx2))

##############################
## SPECIFY INITIAL MODEL
## note that low is the larger wavenumber in the clpequ spec! 
##############################

model1 <- initModel(mod_type = "kin", 
kinpar=c( 0.13698630, 0.3448275849E-01, 0.1020408142E-01, 0.2941176528E-02, 
0.17000,  0.015,  0.1074082902E-03), 
fixed = list(prel = 1:6, clpequ=1:3, kinpar=1:7, irfpar=1, parmu=1),  
irfpar=c(0.4211619198, 0.6299000233E-01), 
prelspec = list(
list(what1="kinpar", ind1=1, what2 = "kinpar", ind2=4,
start=c(-1,0.1369863003)), 
list(what1="kinpar", ind1=2, what2 = "kinpar", ind2=4,
start=c(-1,0.3448275849E-01)), 
list(what1="kinpar", ind1=3, what2 = "kinpar", ind2=4,
start=c(-1,0.1020408142E-01))
), 
parmu = list(c(-0.1411073953)), 
lambdac = 1290,
kmat = delK,
jvec = jvector,
positivepar="kinpar",
weightpar=list( c(-20,1.4,1,2000,.2)), 
clpequspec =list(
list(to=2, from=1, low=100, high=10000), 
list(to=3, from=1, low=100, high=10000),
list(to=4, from=1, low=100, high=10000)),
clpequ = c(1,1,1),
cohspec = list( type = "irf"))

##############################
## GET RESID 
## same format as call to fitModel, but does not plot
##############################

serResid <- getResid(list(C1_1, C2_1, C3_1), list(model1), 
modeldiffs = list(thresh =  0.00005, 
dscal = list(list(to=2,from=1,value=4), 
list(to=3,from=1,value=0.8000000119)),  
free = list(
list(what="irfpar", ind=1, start= c(0.1231127158), dataset=2),
list(what="parmu", ind=c(1,1), start= c(0.1219962388), dataset=2),
list(what="irfpar", ind=1, start= c(0.3724052608), dataset=3),
list(what="parmu", ind=c(1,1), start= c(0.8844097704E-01), dataset=3)), 
change  = list(
list(what="fixed", spec=list(clpequ=1:3, kinpar=1:7, irfpar=1:2, 
parmu=1, drel = 1, prel=1:6), dataset=2:3))),  
opt=kinopt(iter=0, title="Cosimo Spectra, Not Normalized, with Error", 
stderrclp=TRUE, kinspecerr=TRUE, writespec = TRUE, 
plotkinspec = TRUE,plotcohcolspec=FALSE,
selectedtraces = seq(1, length(allx2), by=2), 
specinterpol = TRUE, specinterpolpoints=FALSE,
divdrel=TRUE, xlab="wavenumber",writeclperr = TRUE,
makeps = "err", linrange = 1, superimpose=1:3))

##############################
## MAKE CORRECTED DATASETS USING RESID INFO
##############################

C1_3 <- preProcess(data = C1_1, svdResid = serResid[[1]], numV = 2)
C2_3 <- preProcess(data = C2_1, svdResid = serResid[[2]], numV = 2)
C3_3 <- preProcess(data = C3_1, svdResid = serResid[[3]], numV = 2)

##############################
## FIT MODEL 
##############################

serRes<-fitModel(list(C1_3, C2_3, C3_3), list(model1), 
modeldiffs = list(thresh =  0.00005, 
dscal = list(list(to=2,from=1,value=4), 
list(to=3,from=1,value=0.8000000119)),  
free = list(
list(what="irfpar", ind=1, start= c(0.1231127158), dataset=2),
list(what="parmu", ind=c(1,1), start= c(0.1219962388), dataset=2),
list(what="irfpar", ind=1, start= c(0.3724052608), dataset=3),
list(what="parmu", ind=c(1,1), start= c(0.8844097704E-01), dataset=3)), 
change  = list(
list(what="fixed", spec=list(clpequ=1:3, kinpar=1:7, irfpar=1:2, 
parmu=1, drel = 1, prel=1:6), dataset=2:3))),  
opt=kinopt(iter=0, title="Cosimo Spectra, Not Normalized, with Error", 
stderrclp=TRUE, kinspecerr=TRUE, writespec = TRUE, 
plotkinspec = TRUE,plotcohcolspec=FALSE, writerawcon = TRUE,
selectedtraces = seq(1, length(allx2), by=2), 
specinterpol = TRUE, specinterpolpoints=FALSE,
divdrel=TRUE, xlab="wavenumber",writeclperr = TRUE,
makeps = "h20", linrange = 1, superimpose=1:3))

}
}
 \value{
object of class \code{dat}. 
}
\author{Katharine M. Mullen, Ivo H. M. van Stokkum}
\seealso{ \code{\link{readData}}, \code{\link{getResid} }}
\keyword{file} 
