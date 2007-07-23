"doClpConstr" <-
function (X, clp_ind, clpCon, clpequ, dataset)  
{   
    colx <- dim(X)[2]
    ct <- rep(0, colx)
    
    if(dim(clpCon$clpRem)[2]!=0){ 
	ctparam <- 1			 
	for(i in 1:(dim(clpCon$clpRem)[2])){
	      if(clpCon$clpRem[clp_ind,i] !=0)  
	         if(clpCon$clpDataset[clp_ind,i] != dataset) {
			X[,clpCon$clpRem[clp_ind,i] - 
			ct[clpCon$clpRem[clp_ind,i]]  ] <-
			               X[,clpCon$clpRem[clp_ind,i] - 
			               ct[clpCon$clpRem[clp_ind,i]]  ] *
			               clpequ[ctparam]
			ctparam <- ctparam + 1
		  }
		  else {
		      cM <- ct[ clpCon$clpMod[clp_ind,i]]
		      cR <- ct[ clpCon$clpRem[clp_ind,i]]
	
	              X[,clpCon$clpMod[clp_ind,i] - cM ] <- X[,clpCon$clpMod[clp_ind,i] -cM ] + X[,clpCon$clpRem[clp_ind,i] -cR ] * clpequ[ctparam]
		      
		      X <-as.matrix(as.matrix(X)[,-(clpCon$clpRem[clp_ind,i] -cR)])
		      ct[ (clpCon$clpRem[clp_ind,i] +1):colx ] <- ct[
		       (clpCon$clpRem[clp_ind,i] +1):colx ] + 1
		       ctparam <- ctparam + 1
		  }
	}
	
   }
   if (dim(clpCon$clp0mat)[2] != 0) {
        for (i in 1:(dim(clpCon$clp0mat)[2])) {
            if (clpCon$clp0mat[clp_ind, i] != 0) {
                X <- as.matrix(as.matrix(X)[, -(clpCon$clp0mat[clp_ind, 
                  i] - ct[clpCon$clp0mat[clp_ind, i]] )])
		ct[ (clpCon$clp0mat[clp_ind,i] +1):colx ] <- ct[
		       (clpCon$clp0mat[clp_ind,i] +1):colx ] + 1
            }
        }
   }
   X
}

