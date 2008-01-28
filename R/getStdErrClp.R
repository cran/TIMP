"getStdErrClp" <- function(multimodel, m, group, QR.temp, thetalist,
                           clpindepX, rlist, rawtheta, cp) {
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
    multimodel@fit@resultlist[[group[[i]][2]]]@std_err_clp[[group[[i]][1]]] <- std_err_clp
 
  multimodel 
}
