getXList <- function(result) {
  modtype <- result$currModel@modellist[[1]]@mod_type
  resultlist <- result$currModel@fit@resultlist
  m <- result$currModel@modellist
  t <- result$currTheta
  XList <- vector("list", length=length(m))
  tauList <- muList <- list()

  if(modtype == "kin") {
    groups <- result$currModel@groups
    multimodel <- result$currModel
    f1<-function(x){x[[1]]}   
    f2<-function(x){x[[2]]}
    ## will plot the first concentration from each dataset
    grtoplot <- vector("list", length(m)) 
    for(i in 1:length(m)) {
      cnt <- 1
      notfound <- TRUE 
      while(notfound) {
        for(j in 1:length(groups[[cnt]])) {
              if(groups[[cnt]][[j]][2] == i) {
                grtoplot[[i]]<-list(groups[[cnt]],j)
                notfound<-FALSE
              }
            }
        cnt<-cnt+1
      }	
    }
    for(i in 1:length(m)) {
      group <- grtoplot[[i]][[1]]
      place <-  grtoplot[[i]][[2]]
      dset <- group[[place]][2]
      irfmu <- unlist(lapply(resultlist[[i]]@irfvec, f1))
      irftau <- unlist(lapply(resultlist[[i]]@irfvec, f2))
      muList[[i]] <- irfmu 
      tauList[[i]] <- irftau
      XList[[i]] <- getConToPlot(getKinConcen(
      group, multimodel, t, oneDS = place), m[[i]]@cohspec, m[[i]]@cohcol)
    }
  }
  if(modtype=="spec") {
    for(i in 1:length(m)) {
      if(m[[i]]@timedep)
        specpar <- specparF(t[[i]]@specpar, m[[i]]@x[1], 
                            1, m[[i]]@specref, m[[i]]@specdispindex, 
                            t[[i]]@specdisppar, parmufunc = m[[i]]@parmufunc)
              else 
                specpar <- t[[i]]@specpar 
      
      XList[[i]] <- doClpConstr(specModel(specpar, m[[i]]),
                                clp_ind = 1, 
                                clpCon = m[[i]]@clpCon, clpequ = t[[i]]@clpequ, 
                                num_clpequ = length(m[[i]]@clpequspec), 
                                usecompnames0 = m[[i]]@usecompnames0, 
                                usecompnamesequ = m[[i]]@usecompnamesequ)
      
    }
  }
  if(modtype=="mass") {
    for (i in 1:length(m)) {
     XList[[i]] <- compModelMass(theta = t[[i]], model = m[[i]])
   }
  }
  for(i in 1:length(XList)) {
    xdim <- dim(XList[[i]])
    attributes(XList[[i]])<-NULL
    dim(XList[[i]])<- xdim
  }
  
  XList
}
