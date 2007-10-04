"getResidRet" <- 
function(X, psi, rlist, returnX, finished, nnls)
{
	if(returnX)  return(as.vector(X))
	if(finished) {
		  rlist$QR <- qr(X)
		  rlist$psi <- psi 
		  return(rlist) 
	}
	if(!nnls) {
	       	qty.temp <- qr.qty( qr(X) , psi)
               	residQspace <- qty.temp[-(1:ncol(X))]
               	retval <- residQspace
      	}
	else {
	    	startval <- rep(0, ncol(X))
    		fn1 <- function(par1) sum( ( psi - X %*% par1)^2)
    		cp <- optim( startval, fn = fn1, lower = c(0,0),  
			method="L-BFGS-B")$par
		retval <- psi - X %*% cp
        }
     	retval 
}
