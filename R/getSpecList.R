"getSpecList" <- 
function (multimodel, t, getclperr = FALSE) 
{   
    m <- multimodel@modellist
    clpList <- list()
    resultlist <- multimodel@fit@resultlist
    for (i in 1:length(m)) {
	X <- matrix(nrow = m[[i]]@nl, ncol = max(m[[i]]@ncolc))
	if(getclperr) {
	   cptemp <- resultlist[[i]]@std_err_clp
	   if (dim(m[[i]]@clpCon$clpRem)[2] != 0) {
	    for (j in 1:dim(m[[i]]@clpCon$clpRem)[2]) {
                for (k in 1:m[[i]]@nl) {
		  if(m[[i]]@clpCon$clpRem[k, j] != 0) 
		    if(m[[i]]@clpCon$clpDataset[k, j] == i) 
		      cptemp[[k]] <- append(cptemp[[k]],
		      resultlist[[m[[i]]@clpCon$clpDataset[k, j]]]@std_err_clp[[k]][m[[i]]@clpCon$clpMod[k, j]] * 
                    t[[i]]@clpequ[j], after = m[[i]]@clpCon$clpRem[k, j] - 1)
		    else	   
		    cptemp[[k]] <- resultlist[[m[[i]]@clpCon$clpDataset[k, j]]]@std_err_clp[[k]][m[[i]]@clpCon$clpMod[k, j]] * t[[i]]@clpequ[j]

		}

            }
         }
        }
	else  {
	   cptemp <- resultlist[[i]]@cp
	   if (dim(m[[i]]@clpCon$clpRem)[2] != 0) {
	    for (j in 1:dim(m[[i]]@clpCon$clpRem)[2]) {
                for (k in 1:m[[i]]@nl) {
		  if(m[[i]]@clpCon$clpRem[k, j] != 0) 
                    if(m[[i]]@clpCon$clpDataset[k, j] == i) 
		      cptemp[[k]] <- append(cptemp[[k]],
		      resultlist[[m[[i]]@clpCon$clpDataset[k, j]]]@cp[[k]][m[[i]]@clpCon$clpMod[k, j]] * 
                    t[[i]]@clpequ[j], after = m[[i]]@clpCon$clpRem[k, j] - 1)
		    else	   
		    cptemp[[k]] <- resultlist[[m[[i]]@clpCon$clpDataset[k, j]]]@cp[[k]][m[[i]]@clpCon$clpMod[k, j]] * t[[i]]@clpequ[j]


		}
            }
         }

	}
        if (dim(m[[i]]@clpCon$clp0mat)[2] != 0) {
            for (j in 1:dim(m[[i]]@clpCon$clp0mat)[2]) {
                for (k in 1:m[[i]]@nl) {
                  if (m[[i]]@clpCon$clp0mat[k, j] != 0) {
                    cptemp[[k]] <- 
		    append(cptemp[[k]],
		    0, after=m[[i]]@clpCon$clp0mat[k, j]-1)
		    }
                }
            }
        }
	for (j in 1:m[[i]]@nl) 
	    X[j, ] <- cptemp[[j]]
        clpList[[i]] <- X
    }
    clpList
}
