"getPar" <-
function (model, po, th, theta=theta() ) 
{
	 removepar <- po$rm 
	 if(length(unlist(slot(model, po$name))) - length(removepar) != 0)
		 parvec <- th[po$ind]
         else 
		parvec <- vector()
	 if(po$name %in% model@positivepar && length(parvec) != 0) 
		parvec <- exp(parvec)
	 for(fx in removepar){
		if(fx %in% model@fvecind[[po$name]])
		    parvec <- append(parvec, unlist(slot(model, po$name))[fx], 
			        after=(fx-1))
                 else 
		    parvec <- append(parvec, 0, after=(fx-1))
	 }
	 parvec
}

