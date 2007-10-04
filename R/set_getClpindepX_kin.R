    setMethod("getClpindepX", signature(model = "kin"), function(model, 
        multimodel, theta, returnX, rawtheta, dind) {
	if(returnX) 
		 theta <-  getThetaCl(rawtheta, multimodel)[[dind]]
        x <- compModel(k = theta@kinpar, kinscal = model@kinscal, 
            x = model@x, irfpar = theta@irfpar, irf = model@irf, 
	    seqmod = model@seqmod, 
            convalg = model@convalg, fullk = model@fullk, kmat = model@kmat, 
            jvec = model@jvec, dscalspec = model@dscalspec, drel = theta@drel, 
            mirf = model@mirf, measured_irf = model@measured_irf, 
	    speckin2 = model@speckin2, 
	    usekin2 = model@usekin2, kinpar2 = theta@kinpar2, 
	    kin2scal = theta@kin2scal, reftau = model@reftau, 
	    anispec = model@anispec, anipar = theta@anipar, 
	    cohcol = model@cohcol)
	if(returnX) 
		    x <- as.vector(x) 
	
	x
		    
    })
