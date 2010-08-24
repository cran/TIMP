  setMethod("initModelClass", signature(model="spec"), 
    function (model) 
    {
	model@specdisp <- length(model@specdisppar) != 0 
	model@timedep <- model@weight || model@lclp0 || model@specdisp
	model@clpdep <- model@timedep
        model@ncomp <- length(model@specpar)
        model@ncole <- array(model@ncomp, model@nt)
	model
   }) 
