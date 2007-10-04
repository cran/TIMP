"fillResult" <- function (group, multimodel, thetalist, clpindepX, rlist,
	        rawtheta)
{
    irfvec <- rlist$irfvec
    QR.temp <- rlist$QR 
    psi <- rlist$psi 
    m <- multimodel@modellist 
    dset <- group[[1]][2]
    if(multimodel@nonnegclp || multimodel@nnls) {
	X <- qr.X(QR.temp)
    	startval <- rep(0, ncol(X))
    	fn1 <- function(par1) sum( ( psi - X %*% par1)^2)
    	cp <- optim( startval, fn = fn1, lower = c(0,0),  
			method="L-BFGS-B")$par
	fitted <- X %*% cp 
	resid <- psi - fitted
   }
   else {
	cp <- qr.coef(QR.temp, psi)
   	resid <- qr.resid(QR.temp, psi)
   	fitted <- qr.fitted(QR.temp, psi)   
   }
   cnt <- 1
   for (i in 1:length(group)) {
	dset <- group[[i]][2] 
	clpind <- group[[i]][1]
	res <- multimodel@fit@resultlist[[dset]]
	if(m[[dset]]@mod_type == "kin") { 
	  irfvec[[i]][which(is.na(irfvec[[i]]))] <- 0
	  if (length(m[[dset]]@cohspec$type) != 0) {
		  if (m[[dset]]@cohspec$type == 
		  "freeirfdisp") 
		     res@cohirf[[clpind]] <- rlist$cohirf[[i]] 
	  }
        }
	res@irfvec[[clpind]] <- irfvec[[i]] 
	if(m[[dset]]@clpType == "x2") 
           nt_or_nl <- m[[dset]]@nt
	else  nt_or_nl <- m[[dset]]@nl
	res@cp[[clpind]] <- cp
        res@fitted[[clpind]] <- fitted[cnt:(cnt + nt_or_nl - 1)]
        res@resid[[clpind]] <- resid[cnt:(cnt + nt_or_nl - 1)]        
	cnt <- cnt + nt_or_nl 
	multimodel@fit@rss <- multimodel@fit@rss + sum(res@resid[[clpind]]^2)
	multimodel@fit@resultlist[[dset]] <- res
   }
   if(multimodel@stderrclp) {
    ## note that this option does not return correct values if 
    ## multimodel@nonnegclp || multimodel@nnls
    ds <- group[[1]][2] 
    sigma_2 <- multimodel@fit@nlsres$sumonls$sigma^2
    R <- multimodel@fit@nlsres$onls$m$Rmat()
    R_inv <-  backsolve(R, diag(ncol(R))) 
    R_X <- qr.R(QR.temp)
    R_X_inv <- backsolve(R_X,diag(ncol(R_X)))
    X_pseudo <- tcrossprod(R_X_inv, qr.Q(QR.temp))
    numenv <- new.env()
    assign("model", m[[ds]], env = numenv)
    assign("group", group, env = numenv)
    assign("rawtheta", rawtheta, env = numenv)
    assign("multimodel", multimodel, env = numenv)
    assign("thetalist", thetalist, env = numenv)
    assign("clpindepX", clpindepX, env = numenv)
    assign("finished", FALSE, env = numenv)
    assign("returnX", TRUE, env = numenv)
    if(m[[ ds ]]@clpdep) 
	s_e <- body(selectMethod("residPart", m[[ ds ]]@mod_type)@.Data)
    else s_e <- body(selectMethod("residPart", m[[ ds ]]@mod_type)@.Data)
    X_gradient <- attr( numericDeriv(expr=s_e, rho = numenv, 
	theta = c("rawtheta")), "gradient")
   dim(X_gradient) <- c(dim(QR.temp$qr),length(rawtheta)) 
    G <- matrix(nrow=length(cp), ncol = length(rawtheta)) 
    for(i in 1:length(rawtheta)) 
	 G[,i] <- X_pseudo %*%  ( as.matrix(X_gradient[,,i]) %*% cp) 
    G_R_inv <- G %*% R_inv
   ##COMMENT  
   # if A R = G, R^T A^T = G^T 
   # G R_inv R_inv^T G^T 
   # A A^T  
   ## END COMMENT 
    A_T <- solve(t(R), t(G))
    Bloc1 <- tcrossprod(X_pseudo, X_pseudo) 
    std_err_clp <- sqrt( sigma_2 * diag(Bloc1 + crossprod(A_T, A_T)))
    for (i in 1:length(group)) 
       multimodel@fit@resultlist[[ group[[i]][2] ]]@std_err_clp[[clpind]] <- std_err_clp
 }
 multimodel
}

