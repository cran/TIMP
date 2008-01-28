"fillResult" <- function (group, multimodel, thetalist, clpindepX, rlist,
	        rawtheta)
{
  irfvec <- rlist$irfvec
  QR.temp <- rlist$QR 
  psi <- rlist$psi 
  m <- multimodel@modellist 
  dset <- group[[1]][2]
  #start getting the clp
  nnls <- multimodel@nnls 
  nnlscrit <- multimodel@nnlscrit 
  if(nnls > 0) {
    X <- qr.X(QR.temp)
    if(nnls == 1) {
      startval <- rep(0, ncol(X))
      fn1 <- function(par1) sum( ( psi - X %*% par1)^2)
      cp <- optim( startval, fn = fn1, lower = c(0,0),  
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
    fitted <- X %*% cp 
    resid <- psi - fitted
  }   
  else {
    cp <- qr.coef(QR.temp, psi)
    resid <- qr.resid(QR.temp, psi)
    fitted <- qr.fitted(QR.temp, psi)   
  }
  cnt <- 1
  # fill in results
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
    ## nnls > 0 
     multimodel <- getStdErrClp(multimodel, m, group, QR.temp,thetalist,
                           clpindepX, rlist, rawtheta, cp) 
   }
  multimodel
}

