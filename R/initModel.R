"initModel" <-
function (...) 
{
    dots <- list(...)
    if ("mod_type" %in% names(dots)) {
        if (dots$mod_type == "kin") 
            model <- kin(...)
        if (dots$mod_type == "spec") 
            model <- spec(...)
        if (dots$mod_type == "het") 
            model <- het(...)
        if (dots$mod_type == "fc") 
            model <- fc(...)
    }
    model@datCall <- append(model@datCall, match.call())   
    model@weight <- length(model@weightpar) != 0
    model@lclp0 <- length(model@clp0) != 0
    model@lclpequ <- length(model@clpequspec) != 0
    model@clpCon <- getClpConstr(if (model@mod_type != "spec") 
        model@x2
    else model@x, model@clp0, model@clpequ, model@clpequspec)
    if (model@mod_type != "spec") {
        model@mirf <-  length(model@measured_irf) != 0
	model@irf <-  length(model@irfpar) != 0 || model@mirf
	model@dispmu <-  length(model@parmu) != 0
	model@disptau <-  length(model@partau) != 0
	model@fullk <-  length(dim(model@kmat)) != 1
	if(model@fullk) 
		model@seqmod <- FALSE
        model@wavedep <- (model@dispmu || model@disptau || model@weight || 
            model@lclp0 || model@lclpequ || length(model@parmu) > 
            0)
        model@clpdep <- model@wavedep
        if(model@fullk) 
	    model@ncomp <- dim(model@kmat)[1] + length(model@kinpar2)
	else 
	     model@ncomp <- length(model@kinpar) + length(model@kinpar2)
        model@ncolc <- array(model@ncomp, model@nl)
        if (length(model@cohspec$type) == 0) 
            model@cohspec$type <- ""
	if(length(model@speckin2$seqmod) == 0)
		model@speckin2$seqmod <- FALSE
	if(length(model@speckin2$jvec) == 0)
	        model@speckin2$fullk <- FALSE
	else model@speckin2$fullk <- TRUE
	model@usekin2 <- if( length(model@kinpar2) == 0) FALSE
	   else TRUE
    }
    else {
        model@specdisp <- length(model@specdisppar) != 0 
	model@timedep <- model@weight || model@lclp0 || model@specdisp
	model@clpdep <- model@timedep
        model@ncomp <- length(model@specpar)
        model@ncole <- array(model@ncomp, model@nt)
    }
    if (model@mod_type != "spec") {
        if (length(model@cohspec) != 0) 
            model <- getCoh(model)
	    model <- getAnisotropy(model)  
    }
    model@x <- model@x * model@scalx
    model@fvecind <- getFixed(model)
    model@pvecind <- getPrel(model)
    model <- getConstrained(model)
    model <- addPrel(model)
    model
}
