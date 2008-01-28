#######################
## This simulated data was created by Sergey Laptenok.
#######################

#######################
## Note that in order to plot these results, the packages 
## "gclus" and "fields" must be installed from CRAN --- 
## under the Windows RGui, choose "Install packages from CRAN"
## under the Package menu and install these packages before optimizing.
## Under Linux/Unix-alikes, as su say 
## install.packages("fields", "gclus") in R 
#######################

## uncomment 2 lines below if gclus and fields are installed 
## require(gclus)
## require(fields) 
require(TIMP)

## read in a vector (stored in an ascii file) representing a measured IRF
measuredIRF <-scan("xtoh_256.txt")

## Subtract a baseline in the IRF
measuredIRFcor <- baseIRF(measuredIRF, 100, 150) 

## read in simulated FLIM data; note that the FLIM format is documented 
## at http://www.nat.vu.nl/~kate/FLIM_format/ 
serT<-readData("25_650-75_2700.txt")

## selected a section of the dataset 
selSerT<-preProcess(serT, sel_time=c(33,230))

## Note that parmu in the context of a measured IRF represents 
## and IRF shift parameter.  
## Options for numerical convolution algorithms are 1-4
## Algorithm 1 seems to have the best performace.

modelC<- initModel(mod_type = "kin", 
kinpar=c(2, 0.4), convalg = 1, parmu = list(0),  
measured_irf = measuredIRFcor,   
seqmod=FALSE, positivepar=c("kinpar"),
makeps="2_05_1", 
title="Sergey convolve test")

## uncomment below to test FLIM plotting if 
## gclus and fields are installed 
## serRes<-fitModel(list(selSerT), list(modelC), 
## opt=kinopt(iter=0, linrange = 10,
## makeps = "2_05_1", 
## notraces = TRUE, 
## xlab = "time (ns)", ylab = "pixel number", 
## FLIM=TRUE))

serRes<-fitModel(list(selSerT), list(modelC), 
opt=kinopt(iter=1, linrange = 10,
makeps = "2_05_1", 
notraces = TRUE, 
xlab = "time (ns)", ylab = "pixel number", 
plot=FALSE))





