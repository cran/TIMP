## Example showing the addition of non-negativity constraints to 
## conditionally linear parameters (here the spectra associated with
## a kinetic model)

## For the 1st simulated dataset, the constraints offer a modest improvement
## in the estimated spectra, whereas for the 2nd simulated dataset, they
## prevent a catastrophe in which the estimated components are hugely
## compensating. 

##############################
## load TIMP
##############################

require(TIMP)

##############################
## set random seed for reproducability of noise
##############################

set.seed(80)

##############################
## SIMULATE DATA, noise realization 1
##############################

 dt4 <- simndecay_gen(kinpar = c(.4, .8, 2), seqmod = FALSE, tmax
     = 2, deltat = .04, specpar = list(c(25000, 3000, .01), c(22000,
     3000, .01), c(18000, 3000, .01)), lmin=350, lmax=550, deltal = 2,
     sigma=.01)

##############################
## SPECIFY INITIAL MODEL
##############################

mod1 <- initModel(mod_type = "kin", kinpar = c(.4, .8, 2),
seqmod=FALSE)

##############################
## FIT INITIAL MODEL
##############################

sT <- fitModel(list(dt4), list(mod1), opt=kinopt(iter=50, plot=FALSE))

##############################
## EXTRACT ESTIMATED SPECTRA 
## these spectra have some negative values
############################## 
                                   
sTcp <- getCLPList(sT)[[1]]

## plot the estimated spectra with the values used in
## simulation  (before adding noise) for comparison 
 matplot(dt4@x2, sTcp, xlab = "wavelength (nm)", col = 2:4, type="l",
        ylab="",lty=1, main =
        paste("Estimated spectra, adding no constraints\n"))
 matplot(dt4@x2,dt4@E2, add=TRUE, type="l", col=1, lty=2)
 abline(0,0)


##############################
## FIT INITIAL MODEL 
## adding constraints to non-negativity of the
## spectra via the opt option nnls=2
##############################

sV <- fitModel(list(dt4), list(mod1), opt=kinopt(iter=50, nnls=2, plot=FALSE))

##############################
## EXTRACT ESTIMATED SPECTRA 
## these spectra have no negative values
############################## 
                                   
sVcp <- getCLPList(sV)[[1]]

## plot the estimated spectra with the values used in
## simulation  (before adding noise) for comparison 
 matplot(dt4@x2, sVcp, xlab = "wavelength (nm)", col = 2:4, type="l",
 ylab="",lty=1,
         main = paste("Estimated spectra, with non-negativity constraints\n"))
 matplot(dt4@x2,dt4@E2, add=TRUE, type="l", col=1, lty=2)
 abline(0,0)

##############################
## SIMULATE DATA, noise realization 2
##############################

 dt4_2 <- simndecay_gen(kinpar = c(.4, .8, 2), seqmod = FALSE, tmax
     = 2, deltat = .04, specpar = list(c(25000, 3000, .01), c(22000,
     3000, .01), c(18000, 3000, .01)), lmin=350, lmax=550, deltal = 2,
     sigma=.01)

##############################
## SPECIFY INITIAL MODEL
##############################

mod1 <- initModel(mod_type = "kin", kinpar = c(.4, .8, 2),
seqmod=FALSE)

##############################
## FIT INITIAL MODEL
##############################

sT <- fitModel(list(dt4_2), list(mod1), opt=kinopt(iter=50,plot=FALSE))
    
##############################
## EXTRACT ESTIMATED SPECTRA 
## these spectra have some negative values
############################## 
                                   
sTcp <- getCLPList(sT)[[1]]

## plot the estimated spectra with the values used in
## simulation (before adding noise) for comparison 
 matplot(dt4@x2, sTcp, xlab = "wavelength (nm)", col = 2:4, type="l",
        ylab="",lty=1, main =
        paste("Estimated spectra, adding no constraints\n"))
 matplot(dt4@x2,dt4@E2, add=TRUE, type="l", col=1, lty=2)
 abline(0,0)

##############################
## FIT INITIAL MODEL 
## adding constraints to non-negativity of the
## spectra via the opt option nnls=2
##############################

sV <- fitModel(list(dt4_2), list(mod1), opt=kinopt(iter=50, nnls=2,plot=FALSE))

##############################
## EXTRACT ESTIMATED SPECTRA 
## these spectra have no negative values
############################## 
                                   
sVcp <- getCLPList(sV)[[1]]

## plot the estimated spectra with the values used in
## simulation (before adding noise) for comparison 
 matplot(dt4@x2, sVcp, xlab = "wavelength (nm)", col = 2:4, type="l",
 ylab="",lty=1,
         main = paste("Estimated spectra, with non-negativity constraints\n"))
 matplot(dt4@x2,dt4@E2, add=TRUE, type="l", col=1, lty=2)
 abline(0,0)
