"fillK" <-
function (theta, kinscal, kmat) 
#fillK
{
     dimk <- dim(kmat)[1]  # kmat is square 
     reskmat<-matrix(0, nrow=dimk, ncol=dimk)
     for(i in 1:dimk) {
	    for(j in 1:dimk) {
		 if(i==j)
			pl<- -1
		 else	pl<- 1
		 if(kmat[i,j,1] != 0 && kmat[i,j,2] != 0)
			# use scaling and theta
			reskmat[i,j]<-pl*theta[kmat[i,j,1]]*
                                      kinscal[kmat[i,j,2]]
		 if(kmat[i,j,1] != 0 && kmat[i,j,2] == 0)	
	                # use theta 
			reskmat[i,j]<-pl*theta[kmat[i,j,1]]
		 if(kmat[i,j,1] == 0 && kmat[i,j,2] != 0)	
	                # use scaling 
			reskmat[i,j]<-pl*kinscal[kmat[i,j,2]]
		
	   }
    }
     for(i in 1:dimk) {
	    for(j in 1:dimk) {
    	       if(i!=j)
    			reskmat[j,j]<-reskmat[j,j]-reskmat[i,j]
    	}
    }
    #kmat is 3d array with elements kmat[i,j,]
    #<- c(thetaindex, theta scal index)
    reskmat
}

