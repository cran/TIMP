## all of these functions act on the output of the fitModel function to
## return various results
getCLPList <- function(result, getclperr = FALSE) 
  getSpecList(result$currModel, result$currTheta, getclperr)
getData <- function(result, dataset = 1, weighted = FALSE) {
  if(weighted)   
    datamat <- result$currModel@data[[dataset]]@psi.weight
  else
    datamat <- result$currModel@data[[dataset]]@psi.df
  datamat
}
getSVDData <- function(result, numsing = 2, dataset) {
  datamat <- getData(result, dataset) 
  doSVD(datamat, numsing, numsing)
}
getResiduals <- function(result, dataset = 1) {
  residlist <- result$currModel@fit@resultlist[[dataset]]@resid
  residmat <- unlist(residlist)
  dim(residmat) <- c(length(residlist[[1]]), length(residlist) )
  residmat
}
getSVDResiduals <- function(result, numsing = 2, dataset = 1) {
  residmat <- getResiduals(result, dataset) 
  doSVD(residmat, numsing, numsing)
}
getTraces <- function(result, dataset=1) {
  fitted <- result$currModel@fit@resultlist[[dataset]]@fitted 
  tracesmat <- unlist(fitted)
  dim(tracesmat) <- c(length(fitted[[1]]), length(fitted) )
  tracesmat
}
getdim1List <- function(result) {
  m <- result$currModel@modellist
  timesList <- vector("list", length(m))
  for(i in 1:length(m))
    timesList[[i]] <- m[[i]]@x
  timesList
}
getdim2List <- function(result) {
  m <- result$currModel@modellist
  waveList <- vector("list", length(m))
  for(i in 1:length(m))
    waveList[[i]] <- m[[i]]@x2
  waveList
}
parEst <- function(result, param = "", dataset = NA, verbose = TRUE) {
  currTheta <- result$currTheta
  reslist <- list()
  if(param == "")
    x <- slotNames(theta()) 
  if(is.na(dataset))
    dataset <- 1:length(currTheta)
  
  for(nm in x) {
    for(j in dataset) {
      if(length( slot(currTheta[[j]], nm)) > 0) {
        if(is.null(reslist[[nm]])) {
          reslist[[nm]] <- list()
          if(verbose) cat("Parameters:", nm, "\n")
        }
        reslist[[nm]][[length(reslist[[nm]])+1]] <- slot(currTheta[[j]], nm)
        if(verbose) cat("dataset ", j, ": ", toString(slot(currTheta[[j]], nm)),
                        "\n", sep="")   
      }
    }
  }
  invisible(reslist)
}
sumnls <- function(result) {
  result$currModel@fit@nlsres[[2]]
}
onls <- function(result) {
  result$currModel@fit@nlsres[[1]]
}
