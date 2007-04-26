"init.dat" <-
function () 
{
    setClass("dat", representation(psi.df = "matrix", psi.weight = "matrix", 
        x = "vector", nt = "integer", x2 = "vector", nl = "integer", 
        C2 = "matrix", E2 = "matrix", sigma = "numeric", 
	mod_type = "character", parnames = "vector",
        simdata = "logical", weightpar = "list", 
        weight = "logical", weightM = "matrix", weightsmooth = "list", 
        fixed = "list", clp0 = "list", makeps = "character", 
        clpequspec = "list", lclp0 = "logical", lclpequ = "logical", 
        title = "character", mhist = "list", datCall = "list", 
        dscalspec = "list", 
        drel = "vector", clpequ = "vector", scalx = "numeric", 
        prel = "vector", prelspec = "list", fvecind = "vector", 
        pvecind = "vector", iter = "numeric", 
        clpCon = "list", ncomp = "numeric", clpdep = "logical", 
	inten = "matrix", positivepar="vector"), 
        prototype = list(psi.df = matrix(), psi.weight = matrix(), 
            x = vector(), nt = integer(), x2 = vector(), nl = integer(), 
            C2 = matrix(), E2 = matrix(), sigma = numeric(), 
            mod_type = character(), simdata = logical(), 
            weightpar = list(), weight = FALSE, weightM = matrix(), 
            weightsmooth = list(), fixed = list(), clp0 = list(), 
            clpequspec = list(), clpCon = list(), lclp0 = logical(), 
            lclpequ = logical(), makeps = character(), title = character(), 
            mhist = list(), datCall = list(), 
            dscal = list(), drel = vector(), 
            scalx = 1, prel = vector(), prelspec = list(), fvecind = vector(), 
            pvecind = vector(), clpequ = vector(), 
            iter = 1, ncomp = numeric(), clpdep = logical(),inten = matrix(), 
	    parnames=vector(), positivepar=vector()))
}

