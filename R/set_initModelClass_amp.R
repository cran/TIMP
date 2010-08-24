  setMethod("initModelClass", signature(model="amp"), 
    function (model) 
    {
      model@ncomp <- length(model@amps)
      model@ncolc <- array(model@ncomp, model@nl)
      
      model
   }) 

