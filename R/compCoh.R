"compCoh" <-
function (irfpar, x, cohspec, coh, dataset, cohirf) 
{
	type <- cohspec$type 
	if(type == "irf") 
		cohcols <- dnorm(x, irfpar[1], irfpar[2])
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
	
	cohcols
}

