"initModellist" <-
function (m) 
{
    for (i in 1:length(m)) {
        m[[i]]@parnames <- sort(intersect(
	          setdiff(slotNames(theta()), c("prel","drel")), 
	          slotNames(m[[i]])))
        m[[i]]@weight <- length(m[[i]]@weightpar) != 0
	m[[i]]@lclp0 <- length(m[[i]]@clp0) != 0
        m[[i]]@lclpequ <- length(m[[i]]@clpequspec) != 0
        m[[i]]@clpCon <- getClpConstr(if (m[[i]]@mod_type != "spec") m[[i]]@x2
			 else m[[i]]@x, m[[i]]@clp0, m[[i]]@clpequspec, i)
        if (m[[i]]@mod_type != "spec") {
            m[[i]]@mirf <-  length(m[[i]]@measured_irf) != 0
	    m[[i]]@irf <-  length(m[[i]]@irfpar) != 0 || m[[i]]@mirf
	    m[[i]]@dispmu <-  length(m[[i]]@parmu) != 0
	    m[[i]]@disptau <-  length(m[[i]]@partau) != 0
	    m[[i]]@fullk <-   length(dim(m[[i]]@kmat)) != 1 
	    if(m[[i]]@fullk) 
		m[[i]]@seqmod <- FALSE
	    m[[i]]@wavedep <- (m[[i]]@dispmu || m[[i]]@disptau || 
                m[[i]]@weight || m[[i]]@lclp0 || m[[i]]@lclpequ || 
                length(m[[i]]@parmu) > 0)
            m[[i]]@clpdep <- m[[i]]@wavedep
	    if(m[[i]]@fullk) 
		m[[i]]@ncomp <- dim(m[[i]]@kmat)[1] + length(m[[i]]@kinpar2)
	     else 
		m[[i]]@ncomp <- length(m[[i]]@kinpar) + length(m[[i]]@kinpar2)
             m[[i]]@ncolc <- array(m[[i]]@ncomp, m[[i]]@nl)
	     if (length(m[[i]]@cohspec$type) == 0) 
			m[[i]]@cohspec$type <- ""
	   if(length(m[[i]]@speckin2$seqmod) == 0)
		m[[i]]@speckin2$seqmod <- FALSE
	   if(length(m[[i]]@speckin2$jvec) == 0)
	        m[[i]]@speckin2$fullk <- FALSE
	   else m[[i]]@speckin2$fullk <- TRUE
	   m[[i]]@usekin2 <- if( length(m[[i]]@kinpar2) == 0) FALSE
	   else TRUE
	}
        else {
	    m[[i]]@specdisp <- length(m[[i]]@specdisppar) != 0 
            m[[i]]@timedep <- m[[i]]@weight || m[[i]]@lclp0 || 
                m[[i]]@specdisp
            m[[i]]@clpdep <- m[[i]]@timedep
            m[[i]]@ncomp <- length(m[[i]]@specpar)
            m[[i]]@ncole <- array(m[[i]]@ncomp, m[[i]]@nt)
        }
        if(m[[i]]@mod_type != "spec") {
	  if (length(m[[i]]@cohspec) != 0) 
            m[[i]] <- getCoh(m[[i]])
	    m[[i]] <- getAnisotropy(m[[i]])    	
	}
        m[[i]]@x <- m[[i]]@x * m[[i]]@scalx
        m[[i]]@fvecind <- getFixed(m[[i]])
        m[[i]]@pvecind <- getPrel(m[[i]])
	m[[i]]@mvecind <- m[[i]]@nvecind <- getMvec(m[[i]])
	m[[i]] <- getConstrained(m[[i]])
        m[[i]] <- addPrel(m[[i]])
    }
    m
}