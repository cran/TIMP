"init.spec" <-
function () 
{
    setClass("spec", representation("dat", clpequ = "vector", 
        specpar = "list", specfun = "character", specref = "numeric", 
        specCon = "list", ncole = "vector", specdisp = "logical", 
        specdisppar = "list", specdispindex = "vector", nupow = "numeric", 
	timedep = "logical", parmufunc = "character"), 
        prototype = list(specpar = list(), specfun = "gaus", 
            specCon = list(), ncole = vector(), specdisp = FALSE, 
            specdisppar = list(), specdispindex = vector(), specref =
	    numeric(), nupow = 5, clpequ = vector(), 
	    timedep = FALSE, parmufunc = "poly")) 
}


