"getPrel" <-
function (model) 
{
	# dependent params. are specified as list or vector
	# in prel; get their indices in the parameter group 
        # in its vectorized form 

	prelspec <- model@prelspec
	ppars <- intersect(slotNames(theta()), slotNames(model))
        pinde <- vector("list", length(ppars))
	for(p in 1:length(pinde)) pinde[[p]] <- vector()
        names(pinde) <- ppars
	
	for(rel in prelspec) {
		if(length(rel$ind1) == 1) 
			pinde[[rel$what1]] <- append(pinde[[rel$what1]], 
					       rel$ind1)
		else 
		     	pinde[[rel$what1]] <-
			append(pinde[[rel$what1]], ifelse(rel$ind1[1] > 1, 
			length(unlist(slot(model, rel$what1)[[1:(rel$ind1[1] 
			- 1)]]) + rel$ind1[2]), rel$ind1[2]))
       }

       pinde
}

