"addPrelCl" <- function (thetaClass, model, th, po, addM=TRUE) 
{
     	## prelspec has structure 
	## list(list(what1, ind1, 
	##           what2, ind2, 
	##           rel, start), ...) 
     prelspec <- model@prelspec
     prel <- thetaClass@prel
     parvec <- getPar(model, po, th, thetaClass, addM) 
     if(!addM) 
	       parvec[model@mvecind[["prel"]]] <- prel[model@mvecind[["prel"]]]
     cnt <- 1
     thetaClass@prel <- parvec
     for(diffs in prelspec){
           if(length(diffs$rel) == 0 || diffs$rel == "lin"){
              
	      newpar <- multiLin(thetaClass, diffs, parvec[cnt]) + parvec[cnt+1]
	      if(length(diffs$ind1)==1)
		    slot(thetaClass, diffs$what1)[diffs$ind1] <- newpar 
	      if(length(diffs$ind1)==2) {
		    slot(thetaClass, diffs$what1)[[diffs$ind1[1]]][diffs$ind1[2]] <- newpar 

                  }
	      cnt <- cnt + 2       
            }
	   else { 
             if(diffs$rel == "multilin"){
               newpar <- parvec[cnt] + multiLin(thetaClass, diffs,parvec[(cnt+1):(cnt+length(diffs$start))] )
               if(length(diffs$ind1)==1)
                 slot(thetaClass, diffs$what1)[diffs$ind1] <- newpar 
               if(length(diffs$ind1)==2) 
                 slot(thetaClass, diffs$what1)[[diffs$ind1[1]]][diffs$ind1[2]] <- newpar 
	       cnt <- cnt + length(diffs$start) 
               
             }
           }
         }
     thetaClass
}

