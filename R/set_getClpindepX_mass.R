    setMethod("getClpindepX", signature(model = "mass"), function(model, 
        multimodel, theta, returnX, rawtheta, dind) {
	if(returnX) 
		 theta <-  getThetaCl(rawtheta, multimodel)[[dind]]
	x <- compModelMass(theta, model)	
        if(any(is.infinite(x))){
          t1<<-theta
          t2<<-model
        }
          
        if(returnX) 
		    x <- as.vector(x) 
	
	x
		    
    })
