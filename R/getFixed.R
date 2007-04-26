"getFixed" <-
function (model) 
{
	# model@fixed is a list of lists 
	# eg
	# fixed = list(kinpar = list( c(1,2), c(2,3)), irfpar = list(1,2)) 
	# get the fixed parameters indices as vector indices 
	# into the unlisted parameter vector 

	fixed <- model@fixed 
	ppars <- sort(intersect(slotNames(theta()), slotNames(model)))  
	finde <- vector("list", length(ppars)) 
	for(f in 1:length(finde)) finde[[f]]<-vector()
	names(finde) <- ppars 
	
	if(length(fixed) > 0) {
	   for(r in 1:length(fixed)) {
		if(!is.list(fixed[[r]])){ 
			finde[[ names(fixed)[r] ]] <- as.vector(fixed[[r]])
		}
		else 
		     for(s in 1:length(fixed[[r]]))
		     	finde[[ names(fixed[r]) ]] <-
			append(finde[[ names(fixed[r]) ]], 
			if(fixed[[r]][[s]][1] > 1) 
			(length(unlist(slot(model, names(fixed[r]))[[1:(
			fixed[[r]][[s]][1] - 1)]])) + fixed[[r]][[s]][2]) 
			else fixed[[r]][[s]][2])
	   }
        }
	finde

}

