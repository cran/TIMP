"init.het" <-
function(){
setClass("het", representation("dat", kinpar = "list", seqmod = "list", 
irf = "list", posk = "logical",  irfpar = "list", dispmu = "list", disptau
= "list", parmu = "list", partau = "list", lambdac = "list",  fullk = "list",
kmat = "list", jvec = "list", ncolc = "list", discrirfmu =
"list", discrirftau = "list", kinrel = "list", kinscal = "list", 
kmatfit = "list", coh = "list"), prototype = list(kinpar = list(), 
seqmod = list(), 
irf = list(), posk = logical(),  irfpar = list(), dispmu = list(), disptau
= list(), parmu = list(), partau = list(), lambdac = list(),  fullk = list(),
kmat = list(), jvec = list(), ncolc = list(), discrirfmu =
list(), discrirftau = list(), kinrel = list(), kinscal = list(), 
kmatfit = list(), coh = list() ))
}

