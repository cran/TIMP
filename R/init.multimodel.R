setClass("multimodel", representation(data = "list", datasetind = "vector", 
modelspec = "list", modellist = "list", modeldiffs = "list", fit = "fit", 
parorder = "list", parorderdiff = "list", parorderchange = "list", 
finished = "logical", groups = "list", stderrclp = "logical", 
getXsuper = "logical", nnls = "logical",
nonnegclp = "logical"), 
prototype = list(data=list(), modelspec = list(), modellist = list(), 
modeldiffs = list(), datasetind = vector(), fit = fit(), 
parorder = list(),parorderdiff = list(),parorderchange = list(), 
finished=FALSE, groups = list(), nnls = FALSE,
stderrclp = FALSE, getXsuper = FALSE, 
nonnegclp = FALSE))


