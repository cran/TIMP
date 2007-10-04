"compModelMass" <-
function (theta, model)
{
	peakpar <- theta@peakpar
	amp <- theta@amplitudes
	if(model@peakfunct == "expmodgaus") {
		fn1 <- function(x,ind) x[[ind]]
		lpp <- length(peakpar)
		locations <- unlist(lapply(peakpar, fn1, ind=1))
		widths <- unlist(lapply(peakpar, fn1, ind=2))
		rates <- unlist(lapply(peakpar, fn1, ind=3))
		massm <- rep(0, model@nt * lpp) 
		massm <- as.matrix(.C("calcCirf_multi", 
		     cmat = as.double(massm), 
		     as.double(rates), as.double(model@x), 
		     as.double(widths), 
		     as.double(locations), 
		     as.integer(lpp), 
            	     as.integer(model@nt), PACKAGE="TIMP")$cmat)
           	dim(massm) <- c(model@nt, lpp)
		massm <- t(t(massm)*amp)
	}
	if(model@lextracomp){
		con <- theta@extracomp[[1]] 
		massm <- cbind(massm, rep(con, model@nt))
	} 
	
	massm
}
