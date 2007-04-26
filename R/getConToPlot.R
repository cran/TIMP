"getConToPlot" <- function (C, cohspec, cohcol) 
{

	  cohmax <- vector()
	  if(!identical(cohcol, 0)) {	
		  for (i in 1:length(cohcol)) { 

		        cohmax <- append(max(abs( C[,dim(C)[2]-i+1])), cohmax)
		  	cvec <- C[, dim(C)[2]-i+1 ] / 
					 max(abs( C[, dim(C)[2]-i+1 ] ))
			cvec[is.nan(cvec)]<-0 
			C[, dim(C)[2]-i+1 ] <- cvec
			
	          }       
	  }
	  attr(C, "max") <- cohmax

	  C
}

