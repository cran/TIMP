"getCoh" <-
function (model) 
{
	## type is "irf", "freeirfdisp" "irfmulti" "seq", or "mix" 
	numcohcol <- 0 
	if(model@cohspec$type == "seq") {
		numcohcol <- length(model@cohspec$start) 
		model@coh <- model@cohspec$start   
		model@ncolc <- 
			    array(model@ncomp + length(model@coh), model@nl) 
	}
	if(model@cohspec$type == "irf" || 
           model@cohspec$type == "freeirfdisp") {
		numcohcol <- 1
		model@ncolc <- array(model@ncomp + 1, model@nl) 
	}

	if(model@cohspec$type == "irfmulti") {
		model@ncolc <- array(model@ncomp + model@cohspec$numdatasets, 
		model@nl)
		numcohcol <- model@cohspec$numdatasets 
	}
	if(model@cohspec$type == "mix") {
		numcohcol <- length(model@cohspec$start) + 1
		model@coh <- model@cohspec$start  
		model@ncolc <- array(model@ncomp + length(model@coh) + 1, 
			       model@nl) 
	}
	if(length(model@anispec$rammanest) != 0) {
	    if(model@anispec$rammanest) {
		numcohcol <- numcohcol + 1
		model@ncolc <- model@ncolc + 1
	     }
	}
	else model@anispec$rammanest <- FALSE
	if(numcohcol > 0) {
	           model@cohcol <- (model@ncolc[1]-numcohcol+1):model@ncolc[1]
	}
	model

}

