"removePrelBetweenDatasets" <- function (modellist, diffsrel) 
{
	## rel has structure 
	## list(list(what1, ind1, dataset1,
	##           what2, ind2, dataset2,
	##           rel, start, fixed), ...) 
	## dataset1 can be a vector of indices into dataset list
	## dataset2 can be a vector of indices into dataset list
	## length(dataset1) == length(dataset2)

	for(diffs in diffsrel){
	   for(i in diffs$dataset1) {
		 pinde <- slot(modellist[[i]], "pvecind")
		 if(length(diffs$ind1) == 1) 
			pinde[[diffs$what1]] <- pinde[[diffs$what1]][- which(pinde[[diffs$what1]] == diffs$ind1)[1]]

		slot(modellist[[i]], "pvecind") <- pinde
	   }
        } 
	modellist
}

