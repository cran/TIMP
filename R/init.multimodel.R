setClass("multimodel", representation(data = "list", datasetind =
"vector", modelspec = "list", modellist = "list", modeldiffs = "list",
fit = "fit", parorder = "list", parorderdiff = "list", parorderchange
= "list", finished = "logical", groups = "list", stderrclp =
"logical", getXsuper = "logical", nnls = "numeric", nnlscrit =
"list", optlist = "list", trilinear = "logical"),

         prototype = list(data=list(), modelspec = list(),
modellist = list(), modeldiffs = list(), datasetind = vector(), fit =
fit(), parorder = list(),parorderdiff = list(),parorderchange =
list(), finished=FALSE, groups = list(), nnls = 0, stderrclp = FALSE,
getXsuper = FALSE, nnlscrit = list(), optlist = list(), trilinear=FALSE ))


