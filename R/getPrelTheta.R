"getPrelTheta" <-
function (th, modellist, diffs, d, parorder) 
{
	model <- modellist[[d]]
	fixed <- model@fvecind
	removepar <- fixed[["prel"]]
	if(length(unlist(slot(model, "prel"))) - length(removepar) != 0)
		if(diffs$what %in%  modellist[[d]]@positivepar)
		   parapp <- log(unlist(slot(model, "prel"))[-removepar])
		else
		   parapp <- unlist(slot(model, "prel"))[-removepar] 
	else 
		   parapp <- vector() 
	if(length(parapp) != 0) 
	       ind <- (length(th) + 1):(length(th) + length(parapp))
	else ind <- vector()
	parorder[[length(parorder)+1]] <- list(name="prel",
			  ind=ind, dataset = diffs$dataset, rm=removepar)
	th <- append(th, parapp)
	list(th=th, parorder=parorder)
}

