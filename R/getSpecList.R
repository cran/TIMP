"getSpecList" <- 
function (multimodel, t, getclperr = FALSE) 
{   
    m <- multimodel@modellist
    clpList <- list()
    resultlist <- multimodel@fit@resultlist
    for (i in 1:length(m)) {
       if(m[[i]]@clpType == "x2") {
	nx <- m[[i]]@nl
	colc <- max(m[[i]]@ncolc)
       }	
       else {
	    nx <- m[[i]]@nt
	    colc <- max(m[[i]]@ncole)
       }
	X <- matrix(nrow = nx, ncol = colc)
	X[which(m[[i]]@clpCon$clp0mat!=0)] <- 0
	## mark those clp determined by equality with 0 
	if(m[[i]]@lclpequ) 
		X[, m[[i]]@clpCon$clpRem] <- 0 
	## fill in the clp 
	if(getclperr) 
	   cptemp <- resultlist[[i]]@std_err_clp
	else
	   cptemp <- resultlist[[i]]@cp
	for(j in 1:nx) 
		X[j,is.na(X[j,])] <- cptemp[[j]]
	## go back and enforce the equ constraints
	for(j in 1:nx) 
	  X[j, m[[i]]@clpCon$clpRem[j,] ] <- X[j, m[[i]]@clpCon$clpMod[j,] ] *
			t[[i]]@clpequ
        clpList[[i]] <- X
    }  
    clpList
}
