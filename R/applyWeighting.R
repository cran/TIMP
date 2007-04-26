"applyWeighting" <-
function (modellist) 
{
    for (i in 1:length(modellist)) 
	modellist[[i]] <- applyWeightingModel(modellist[[i]]) 

    modellist
}







