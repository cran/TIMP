"outlierCorr" <- function(oldRes, thresh=.8) {
  # this function takes the result returned by fitModel
  # and returns the list of datasets with outlier correction applied 
  m <- oldRes$currModel@modellist
  res <- oldRes$currModel@fit@resultlist
  dt <- oldRes$currModel@data
  weightList <- vector("list", length(m))
 
  for(i in 1:length(m)){
    cnt <- 0
    x2 <- m[[i]]@x2
    x <- m[[i]]@x
    weightList[[i]] <- matrix(1, nrow=m[[i]]@nt, ncol=m[[i]]@nl)
    fitted <- resid <- matrix(nrow = m[[i]]@nt, ncol = m[[i]]@nl)
    for(j in 1:length(x2)){
      fitted[,j] <- res[[i]]@fitted[[j]]
      resid[,j] <- res[[i]]@resid[[j]]  
    }
    for(j in 1:length(x)) {
      maxthr <- max(fitted[j,])/10
      xx <- resid[j,]/max(fitted[j,],maxthr)
      outind <- which(xx > thresh) 
      if(length(outind)>0) {
        dt[[i]]@psi.df[j,outind] <- fitted[j,outind]
        weightList[[i]][j,outind] <- 1e-10
        cnt <- cnt + length(outind)
      }
    }
    cat("Removed", cnt, "points in dataset", i,"\n") 
  }
 
  list(dt=dt, weightList = weightList)
}
