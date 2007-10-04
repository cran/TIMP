  setMethod("initModelClass", signature(model="mass"), 
    function (model) 
    {
	model@clpdep <- (model@weight || model@lclp0 || model@lclpequ)
	if(length(model@extracomp) > 0) 
		model@lextracomp <- TRUE
	model@ncomp <- length(model@peakpar) + length(model@extracomp)
	model@ncolc <- model@ncomp	
	model
   }) 

