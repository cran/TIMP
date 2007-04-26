"init.multimodel" <-
function() { 

setClass("multimodel", representation(data = "list", datasetind = "vector", 
modelspec = "list", modellist = "list", modeldiffs = "list", fit = "fit", 
parorder = "list", parorderdiff = "list", parorderchange = "list", 
finished = "logical"), 
prototype = list(data=list(), modelspec = list(), modellist = list(), 
modeldiffs = list(), datasetind = vector(), fit = fit(), 
parorder = list(),parorderdiff = list(),parorderchange = list(), 
finished=FALSE))

}

