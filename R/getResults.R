## all of these functions act on the output of the fitModel function to
## return various results
getCLPList <- function(result, getclperr = FALSE) 
  getSpecList(result$currModel, result$currTheta, getclperr)
getCLP <- function(result, getclperr = FALSE, dataset=1) 
  getSpecList(result$currModel, result$currTheta, getclperr)[[dataset]]
getData <- function(result, dataset = 1, weighted = FALSE) {
  if(weighted)   
    datamat <- result$currModel@data[[dataset]]@psi.weight
  else
    datamat <- result$currModel@data[[dataset]]@psi.df
  datamat
}
getSVDData <- function(result, numsing = 2, dataset=1) {
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
getdim1 <- function(result, dataset=1) 
  result$currModel@modellist[[dataset]]@x
getdim2 <- function(result, dataset=1) 
  result$currModel@modellist[[dataset]]@x2
parEst <- function(result, param = "", dataset = NA, verbose = TRUE,
                   file = "") {
  currTheta <- result$currTheta
  reslist <- list()
  if(param == "")
    param <- slotNames(theta()) 
  if(is.na(dataset))
    dataset <- 1:length(currTheta)
  
  for(nm in param) {
    for(j in dataset) {
      if(length( slot(currTheta[[j]], nm)) > 0) {
        if(is.null(reslist[[nm]])) {
          reslist[[nm]] <- list()
          if(verbose) cat("Parameters:", nm, "\n", file=file)
        }
        reslist[[nm]][[length(reslist[[nm]])+1]] <- slot(currTheta[[j]], nm)
        if(verbose) cat("dataset ", j, ": ", toString(slot(currTheta[[j]], nm)),
                        "\n", sep="", file=file)   
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
