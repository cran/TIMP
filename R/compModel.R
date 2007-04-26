"compModel" <-
function (k, kinscal = vector(), x, irfpar = vector(), irf = FALSE, 
    seqmod = FALSE, fullk = FALSE, kmat = matrix(), jvec = vector(), 
    dscalspec = list(), drel = vector(), cohspec = list(), coh = vector(), 
    lamb = 1, dataset = 1, cohirf = vector(), mirf = FALSE, 
    measured_irf = vector(), convalg = 1, shiftmea = vector(), 
    speckin2 = list(), usekin2 = FALSE, kinpar2 = vector(), 
    kin2scal = vector()) 
{
    if (fullk) {
        eig <- fullKF(k, kinscal, kmat, jvec)
        k <- -eig$values

    }
    if (irf) {
        if(length(shiftmea) != 0) shiftmea <- shiftmea[[1]]
        c.temp <- calcCirf(k=k, x=x, irfpar=irfpar, mirf=mirf, 
	    measured_irf=measured_irf, convalg=convalg, shiftmea=shiftmea, 
	    lamb=lamb)
    }
    else c.temp <- calcC(k, x)
    if (seqmod) 
        c.temp <- c.temp %*% calcB(k)
    else if (fullk) 
        c.temp <- c.temp %*% eig$A
     if(usekin2) {
	c.temp <- cbind( kin2scal[1] * c.temp, kin2scal[2] * 
	getKin2(x=x, irfpar=irfpar, irf=irf, lamb=lamb, dataset=dataset, 
	speckin2=speckin2, mirf = mirf, measured_irf = measured_irf, 
	convalg = 1, shiftmea = shiftmea, 
	kinpar2=kinpar2))
    }
    if(!is.null(cohspec$type) && cohspec$type != "")
        c.temp <- cbind(c.temp, compCoh(irfpar, x, cohspec, coh, 
            dataset, cohirf))
    if (length(drel) != 0) {
        if (dscalspec$perclp) 
            c.temp <- drel[lamb] * c.temp
        else c.temp <- drel * c.temp
    }
    c.temp
}
