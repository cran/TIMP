"compCoh" <-
function (irfpar, x, cohspec, coh, dataset, cohirf, mirf = FALSE, 
measured_irf = vector(), convalg = 1, shiftmea = vector(), lamb = 1, 
ani = anispec(), anipar = vector(), cohcol = vector()) 
{
	type <- cohspec$type 
	if(type == "irf") {
		if(mirf) {
			 if (length(shiftmea) != 0) {
			    if (length(shiftmea) == 1) 
			       lamb <- 1
			   xspace <- x[2] - x[1]
			   measured_irf <- .C("ShiftCurve", 
			   source = as.double(measured_irf), 
			   as.double(measured_irf), 
			   as.double(shiftmea[lamb]/xspace), 
			   as.integer(length(x)), PACKAGE="TIMP")$source
			 }
			 cohcols <- measured_irf
	        }
		else
			cohcols <- dnorm(x, irfpar[1], irfpar[2])
	        
	
	}
	if(type == "freeirfdisp") 
		cohcols <- dnorm(x, cohirf[1], cohirf[2])
	if(type == "irfmulti") { 
		cohcols <- matrix(0, nrow = length(x), 
		           ncol = cohspec$numdatasets)
		cohcols[, dataset] <- dnorm(x, irfpar[1], irfpar[2])
	}
	if(type == "seq") 
		cohcols <- calcCirf(coh, x, irfpar) %*% calcB(coh) 
	if(type == "mix") 
		cohcols <- cbind(dnorm(x, irfpar[1], 
			   irfpar[2]), calcCirf(coh, x, irfpar) %*% calcB(coh))
	# constant anisotropy
	if(ani$calcani) {
	       if(ani$rammanest) {
		if(mirf) {
			 if (length(shiftmea) != 0) {
			    if (length(shiftmea) == 1) 
			       lamb <- 1
			   xspace <- x[2] - x[1]
			   measured_irf <- .C("ShiftCurve", 
			   source = as.double(measured_irf), 
			   as.double(measured_irf), 
			   as.double(shiftmea[lamb]/xspace), 
			   as.integer(length(x)), PACKAGE="TIMP")$source
			 }
			 ramman <- measured_irf
	        }
		else
			ramman <- dnorm(x, irfpar[1], irfpar[2])
		cohcols <- cbind(cohcols, ramman)
	       }
		if(ani$angle[dataset]  != "MA") {
			 if(ani$angle[dataset] == "PAR") 
				gamma <- 2
			 if(ani$angle[dataset] == "PERP")
				gamma <- -1 
			for(i in 1:length(cohcol))
			      cohcols <- cohcols * (1+(gamma * anipar[ani$parperm[[i]][1]])) 
		}
	}
	cohcols
}

