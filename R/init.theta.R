"init.theta" <-
function () 
{
    setClass("theta", representation(kinpar = "vector", specpar = "list", 
        irfpar = "vector", parmu = "list", partau = "vector", 
        clpequ = "vector", specdisppar = "list", kinscal = "vector", 
	prel = "vector", coh="vector", drel = "vector", cohirf="vector", 
	kinpar2 = "vector", kin2scal="vector"), 
	prototype = list(kinpar = vector(), specpar = list(), 
	irfpar = vector(), parmu = list(), partau = vector(), 
	clpequ = vector(), specdisppar = list(), drel = vector(), 
	coh = vector(), kinscal = vector(), prel = vector(),
	cohirf= vector(), kinpar2 = vector(), kin2scal = vector()))
}

