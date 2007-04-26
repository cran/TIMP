"getClpList" <- 
function (multimodel, t) 
{
    m <- multimodel@modellist
    clpList <- list()
    resultlist <- multimodel@fit@resultlist 
    for(i in 1:length(m)) {

	  cptemp <- resultlist[[i]]@cp
	  if(m[[i]]@mod_type != "spec"){ 
		numcol <- max(m[[i]]@ncolc)
		clpvar <- m[[i]]@nl
	  }
	  else {
	       	numcol <- max(m[[i]]@ncole)
		clpvar <- m[[i]]@nt
	  }
	  
	  X <- matrix(nrow = clpvar, ncol = numcol) 
	  
	  if(dim(m[[i]]@clpCon$clpRem)[2] != 0) {
	      for(j in 1:dim(m[[i]]@clpCon$clpRem)[2]) {
		    for(k in 1:clpvar) {
			  cptemp[[k]][m[[i]]@clpCon$clpRem[k,j]] <- 
			  resultlist[[ 
			  m[[i]]@clpCon$clpDataset[k,j]]]@cp[[k]][
			  m[[i]]@clpCon$clpMod[k,j]] * t[[i]]@clpequ[j]
	            }
	       }
          }
	  for(j in 1:clpvar) 
		X[j,] <- cptemp[[j]]
          if(dim(m[[i]]@clpCon$clp0mat)[2] != 0) {
	      for(j in 1:dim(m[[i]]@clpCon$clp0mat)[2]) {
		    for(k in 1:clpvar) {
			if( m[[i]]@clpCon$clp0mat[k,j] != 0)
			    X[k,m[[i]]@clpCon$clp0mat[k,j]] <- 0 
	       }
	    }   
          }
	  clpList[[i]] <- X
    }
   
    clpList
}
