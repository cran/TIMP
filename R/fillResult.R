"fillResult" <- function (group , multimodel, thetalist, clpindepX, rlist,
	        rawtheta)
{
    irfvec <- rlist$irfvec
    QR.temp <- rlist$QR 
    psi <- rlist$psi 
    cp <- qr.coef(QR.temp, psi)
    resid <- qr.resid(QR.temp, psi)
    fitted <- qr.fitted(QR.temp, psi)   
    cnt <- 1
    m <- multimodel@modellist 
    for (i in 1:length(group)) {
	res <- .currModel@fit@resultlist[[group[[i]][2]]]
	if(m[[group[[i]][2] ]]@mod_type != "spec") { 
	  irfvec[[i]][which(is.na(irfvec[[i]]))]<-0
	  if (length(m[[group[[i]][2]]]@cohspec$type) != 0) {
		  if (m[[group[[i]][2]]]@cohspec$type == 
		  "freeirfdisp") 
		     res@cohirf[[group[[i]][1]]] <- rlist$cohirf[[i]] 
	  }
        }
	res@irfvec[[group[[i]][1]]] <- irfvec[[i]] 
	if(m[[group[[i]][2]]]@mod_type != "spec") 
           nt_or_nl <- m[[group[[i]][2]]]@nt
	else  nt_or_nl <- m[[group[[i]][2]]]@nl
	res@cp[[group[[i]][1]]] <- cp
        res@fitted[[group[[i]][1]]] <- fitted[cnt:(cnt + nt_or_nl - 1)]
        res@resid[[group[[i]][1]]] <- resid[cnt:(cnt + nt_or_nl - 1)]
        cnt <- cnt + nt_or_nl 
	.currModel@fit@resultlist[[group[[i]][2]]] <<- res
   }
   if(multimodel@modeldiffs$stderrclp) {
    sigma_2 <- multimodel@fit@nlsres$sumonls$sigma^2
    R <- multimodel@fit@nlsres$onls$m$Rmat()
    R_inv <-  backsolve(R, diag(dim(R)[2])) 
    R_X <- qr.R(QR.temp)
    R_X_inv <- backsolve(R_X,diag(dim(R_X)[2]))
    X_pseudo <- tcrossprod(R_X_inv, qr.Q(QR.temp))
    numenv <- new.env()
    assign("model", m[[1]], env = numenv)
    assign("group", group, env = numenv)
    assign("rawtheta", rawtheta, env = numenv)
    assign("multimodel", multimodel, env = numenv)
    assign("thetalist", thetalist, env = numenv)
    assign("clpindepX", clpindepX, env = numenv)
    assign("finished", FALSE, env = numenv)
    assign("returnX", TRUE, env = numenv)
    if(m[[group[[i]][2]]]@clpdep) 
	s_e <- body(selectMethod("residPart", m[[1]]@mod_type)@.Data)
    else s_e <- body(selectMethod("residPart", m[[1]]@mod_type)@.Data)
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
       .currModel@fit@resultlist[[group[[i]][2]]]@std_err_clp[[group[[i]][1]]] <<- std_err_clp
 }

}

