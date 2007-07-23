			    "set_residPart_kin" <-
function () 
{
    setMethod("residPart", signature(model = "kin"), function(model,
        group, multimodel, thetalist, clpindepX, finished, returnX, 
	rawtheta) {
	psi <- vector()
        concen <- matrix()
	if(returnX) 
		 thetalist <-  getThetaCl(rawtheta, multimodel)
	if (finished)
	    rlist <- list(irfvec=vector("list",length(group)))
	    		  
        for (i in 1:length(group)) {
            m <- multimodel@modellist[[group[[i]][2]]]
            t <- thetalist[[group[[i]][2]]]
            psi <- append(psi, m@psi.weight[, group[[i]][1]])
            if (m@wavedep) {
                irfvec <- irfparF(t@irfpar, m@lambdac, m@x2[group[[i]][1]], 
                    group[[i]][1], m@dispmu, t@parmu[[1]], m@disptau, 
                    t@partau, m@dispmufun, m@disptaufun, m@irffun)
                if (length(m@cohspec) != 0) {
                  if (m@cohspec$type == "freeirfdisp") 
                    cohirf <- irfparF(t@irfpar, m@lambdac,
		    m@x2[group[[i]][1]], group[[i]][1], m@dispmu,
		    t@parmu[[2]], m@disptau, t@partau, m@dispmufun,
		    m@disptaufun, m@irffun)
                  else cohirf <- vector()
                }
                concen_i <- doClpConstr(compModel(k = t@kinpar, 
                  kinscal = m@kinscal, x = m@x, irfpar = irfvec, 
                  irf = m@irf, seqmod = m@seqmod, fullk = m@fullk, 
                  kmat = m@kmat, jvec = t@jvec, shiftmea =  t@parmu, 
		  dscalspec = m@dscalspec, 
                  drel = t@drel, cohspec = m@cohspec, coh = t@coh, 
                  cohirf = cohirf, lamb = group[[i]][1], 
		  dataset = group[[i]][2], 
                  mirf = m@mirf, measured_irf = m@measured_irf, 
                  convalg = m@convalg, speckin2 = m@speckin2, 
		  usekin2 = m@usekin2, kinpar2 = t@kinpar2, 
		  kin2scal = t@kin2scal, reftau = m@reftau, 
		  anispec = m@anispec, anipar = t@anipar, 
		  cohcol = m@cohcol), 
		  clp_ind = group[[i]][1], 
                  clpCon = m@clpCon, clpequ = t@clpequ, 
		  dataset = group[[i]][2])
                if (m@weight) 
                  concen_i <- weightNL(concen_i, m, group[[i]][1])
                if (dim(concen_i)[2] != 0) {
                  concen <- if (!identical(concen, matrix())) 
                    rbind(concen, concen_i)
                  else concen_i
                }
            }
            else {
                if (identical(concen, matrix())) 
                  concen <- clpindepX[[group[[i]][2]]]
                else concen <- rbind(concen, clpindepX[[group[[i]][2]]])
            }
            if (finished) {
	      if(m@wavedep) 
		rlist$irfvec[[i]] <- irfvec
	      else rlist$irfvec[[i]] <- c(0,0)
	      if (length(m@cohspec$type) != 0) {
		  if (m@cohspec$type == "freeirfdisp"){ 
		     if(i == 1) rlist$cohirf <- vector("list", length(group))  
		     rlist$cohirf[[i]] <- cohirf
		  } 
              }
	    }
        }
	if(returnX)  
		    retval <- as.vector(concen) 
        else {
	   QR.temp <- qr(concen)
	   if(finished) {
		       rlist$QR <- QR.temp 
		       rlist$psi <- psi 
		       return(rlist) 
	   }
	   qty.temp <- qr.qty(QR.temp, psi)
           residQspace <- qty.temp[-(1:dim(concen)[2])]
           retval <- residQspace
      }
      retval 
    })
}
