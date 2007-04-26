"addPrelCl" <-
function (thetaClass, model, th, po) 
{
     	## prelspec has structure 
	## list(list(what1, ind1, 
	##           what2, ind2, 
	##           rel, start), ...) 
     
     prelspec <- model@prelspec
     parvec <- getPar(model, po, th, thetaClass) 
     cnt <- 1
     thetaClass@prel <- parvec
     for(diffs in prelspec){
           if(length(diffs$rel) == 0 || diffs$rel == "lin"){
	      if(length(diffs$ind1)==1 && length(diffs$ind2)==1){
		    slot(thetaClass, diffs$what1)[diffs$ind1] <-slot(thetaClass, 
                    diffs$what2)[diffs$ind2] * parvec[cnt] + parvec[cnt+1]
	      } 
	      if(length(diffs$ind1)==1 && length(diffs$ind2)==2)
		    slot(thetaClass, diffs$what1)[diffs$ind1] <-
slot(thetaClass, diffs$what2)[[diffs$ind2[1]]][diffs$ind2[2]] * parvec[cnt] + parvec[cnt+1]
	      if(length(diffs$ind1)==2 && length(diffs$ind2)==1)
		    slot(thetaClass, 
	            diffs$what1)[[diffs$ind1[1]]][diffs$ind1[2]] <- 
	            slot(thetaClass, diffs$what2)[diffs$ind2]* parvec[cnt] + parvec[cnt+1]
	      if(length(diffs$ind1)==2 && length(diffs$ind2)==2)
	            slot(thetaClass, 
                    diffs$what1)[[diffs$ind1[1]]][diffs$ind1[2]] <- 
	            slot(thetaClass, diffs$what2)[[diffs$ind2[1]]][diffs$ind2[2]] * parvec[cnt] + parvec[cnt+1]
	  }
	  cnt <- cnt + 2       
	}
      thetaClass
}

