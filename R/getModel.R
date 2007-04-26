"getModel" <- function (data, modelspec, modeldiffs, datasetind, opt) 
{
    if(length(datasetind) != length(data))
	datasetind <- rep(1, length(data))
    modellist <- vector("list", length(data))
    resultlist <- vector("list", length(data))
    plugin <- c("psi.df", "x", "x2", "nt", "nl", "simdata", "inten")
    for(i in 1:length(data)) {
	  resultlist[[i]] <- res() 
	  modellist[[i]] <- modelspec[[datasetind[i] ]]
	  for (sl in plugin) 
		slot(modellist[[i]], sl) <- slot(data[[i]], sl)
    }
    
    modellist <- addDscal(modellist, modeldiffs$dscal)
    if (length(modeldiffs$free) != 0) 
        modellist <- diffFree(modellist, modeldiffs$free)
    if (length(modeldiffs$remove) != 0) 
        modellist <- diffRemove(modellist, modeldiffs$remove)
    if (length(modeldiffs$add) != 0) 
        modellist <- diffAdd(modellist, modeldiffs$add)
    if (length(modeldiffs$change) != 0) {
        reCh <- diffChange(modellist, modeldiffs$change)
	modellist <- reCh$modellist
	modeldiffs$change <- reCh$diffsadd
    }
    if (length(modeldiffs$rel) != 0) 
        modellist <- diffRel(modellist, modeldiffs$rel)
    if (length(modeldiffs$dscal) == 0) 
        modeldiffs$dscal <- list()
    modellist <- applyWeighting(modellist)
    modellist <- initModellist(modellist)
    groups <- getGroups(modellist, modeldiffs)
    modeldiffs$groups <- groups$groups
    modeldiffs$linkclp <- groups$linkclp
    modeldiffs$stderrclp <- opt@stderrclp
    multimodel(modellist = modellist, data = data, datasetind = datasetind, 
    modelspec = modelspec, modeldiffs = modeldiffs, 
    fit = fit(resultlist=resultlist))
}