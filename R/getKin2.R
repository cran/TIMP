"getKin2" <-
function (kinscal = vector(), x, irfpar = vector(), irf = FALSE, 
    lamb = 1, dataset = 1, mirf = FALSE, measured_irf = vector(), convalg = 1, 
    shiftmea = vector(), speckin2 = list(), kinpar2 = vector()) 
{
    if (speckin2$fullk) {
        eig <- fullKF(kinpar2, kinscal, speckin2$kmat, speckin2$jvec)
        kinpar2 <- -eig$values
    }
    if (irf) {
        if(length(shiftmea) != 0) shiftmea <- shiftmea[[1]]
        c.temp <- calcCirf(k=kinpar2, x=x, irfpar=irfpar, mirf=mirf,
	    measured_irf=measured_irf, convalg=convalg, shiftmea=shiftmea,
	    lamb=lamb)
    }
    else c.temp <- calcC(kinpar2, x)
    if (speckin2$seqmod) 
        c.temp <- c.temp %*% calcB(kinpar2)
    else if (speckin2$fullk) 
        c.temp <- c.temp %*% eig$A
    c.temp
}
