"getResidRet" <- 
function(X, psi, rlist, returnX, finished, nnls, nnlscrit=list(), group=0)
{
	if(returnX)  return(as.vector(X))
	if(finished) {
		  rlist$QR <- qr(X)
		  rlist$psi <- psi 
		  return(rlist) 
	}
	if(nnls == 0) { ## just varpro
                qty.temp <- qr.qty( qr(X) , psi)
               	residQspace <- qty.temp[-(1:ncol(X))]
               	retval <- residQspace
      	}
	else {
          if (nnls == 1) { ## include nonneg cp via Byrd
            startval <- rep(0, ncol(X))
            fn1 <- function(par1) sum( ( psi - X %*% par1)^2)
            cp <- optim(startval, fn = fn1, lower = c(0,0),  
			method="L-BFGS-B")$par
          }
          else if(nnls == 2) {
            if(length(nnlscrit$negpos) > 0) {
              con <- nnlscrit$spec[[as.character(group[[1]][1])]]
              cp <- coef(nnnpls(A = X, b = psi, con=con))
            }
            else {
              sol <- nnls(A = X, b = psi)
              cp <- coef(sol)
              #if(length(sol$bound) > 0) {
              #  cp <- cp[-sol$bound]
              #  X <- X[, - sol$bound]
              #}
              ##xx <-nnls(A = X, b = psi)
              ##cat("B:", xx$bound, "CP:", cp,"NSETP", xx$nsetp, "\n")
            }
          }
          else if(nnls == 3) {
            dvec <- t(X) %*% psi 
            bvec <- rep(0, ncol(X))
            Dmat <- crossprod(X,X)
            Amat <- diag(ncol(X))
            cp <- solve.QP(dvec =dvec, bvec = bvec, Dmat=Dmat,
                           Amat=Amat)$solution
            
          }
          retval <- psi - X %*% cp
        }
     	retval
}
