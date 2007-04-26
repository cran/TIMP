"decayirf" <-
function (k, x, tau, mu) 
{
    if(k == 0) 
	 decay <- rep(1, length(x))
    else {
	 decay <- vector(length = length(x))
	 alpha <- k * tau / sqrt(2)
	 beta <- (x - mu) / (tau * sqrt(2)) 
	 thresh <- beta - alpha
	 for(i in 1:length(x)){
	       if(thresh[i] < -1) {
		 decay[i] <- .5 * .C("erfce", 
		             y = as.double(-thresh[i]), PACKAGE="TIMP")$y *
			     exp(- beta[i]^2)
		 
	       }
	       else {
		 decay[i] <- .5 * (1 + erf(thresh[i])) * exp(alpha * 
			     (alpha- 2 * beta[i]))
		
	      }
	}
   }				  

   decay 
}

