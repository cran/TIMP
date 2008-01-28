## all of these functions act on the output of the fitModel function to
## return various results
getCLPList <- function(result, getclperr = FALSE) 
  getSpecList(result$currModel, result$currTheta, getclperr)
getDataList <- function(result, numsing = 2) {
  resultlist <- result$currModel@fit@resultlist
  m <- result$currModel@modellist
  multimodel <- result$currModel
  svddatalist <- datalist <- vector("list", length(m)) 
  for (i in 1:length(m)) {
    svddatalist[[i]] <- doSVD(multimodel@data[[i]]@psi.df, numsing, numsing)
    datalist[[i]] <- multimodel@data[[i]]@psi.df
  }
  list(svddatalist = svddatalist, datalist = datalist)
}
getTracesList <- function(result) {
  m <- result$currModel@modellist   
  t <- result$currTheta 
  res <- result$currModel@fit@resultlist
  fittedList <- vector("list", length(m))
  for(i in 1:length(m)) 
    fittedList[[i]] <-  res[[i]]@fitted
  fittedList
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
getResidualList <- function(result, numsing = 2) {
  resultlist <- result$currModel@fit@resultlist
  m <- result$currModel@modellist
  residlist <- svdresidlist <-  vector("list", length(m)) 
  for (i in 1:length(m)) {
    residuals <- matrix(nrow = m[[i]]@nt, ncol = m[[i]]@nl)
    for (j in 1:length(resultlist[[i]]@resid)) {
      residuals[, j] <- resultlist[[i]]@resid[[j]]
    }
    svdresidlist[[i]] <- doSVD(residuals, numsing, numsing)
    residlist[[i]] <- residuals
  }
  list(svdresidlist = svdresidlist, residlist = residlist)
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
