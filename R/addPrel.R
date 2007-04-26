"addPrel" <-
function (model) 
{
	## prel has structure 
	## list(list(what1, ind1, 
	##           what2, ind2, 
	##           rel, start), ...) 
	
	prelspec <- model@prelspec
	model@prel <- vector()
	for(diffs in prelspec){
           model@prel <- append(model@prel, diffs$start)  
	   if(length(diffs$rel) == 0 || diffs$rel == "lin"){
	      if(length(diffs$ind1)==1 && length(diffs$ind2)==1){
		    slot(model, diffs$what1)[diffs$ind1] <- 
		    slot(model, diffs$what2)[diffs$ind2] * 
			      diffs$start[1] + diffs$start[2]
	      } 
	      if(length(diffs$ind1)==1 && length(diffs$ind2)==2){
		    slot(model, diffs$what1)[diffs$ind1] <- slot(model, 
			      diffs$what2)[[diffs$ind2[1]]][diffs$ind2[2]] * 
			      diffs$start[1] + diffs$start[2]
	      }
	      if(length(diffs$ind1)==2 && length(diffs$ind2)==1){
		    slot(model, 
	            diffs$what1)[[diffs$ind1[1]]][diffs$ind1[2]] <- 
	            slot(model, 
	            diffs$what2)[diffs$ind2] * 
		    diffs$start[1] + diffs$start[2]
	      }
	      if(length(diffs$ind1)==2 && length(diffs$ind2)==2){
	            slot(model, 
	            diffs$what1)[[diffs$ind1[1]]][diffs$ind1[2]] <- 
	            slot(model, 
	            diffs$what2)[[diffs$ind2[1]]][diffs$ind2[2]] * 
		    diffs$start[1] + diffs$start[2]
              }
           }       
	}
	model
}

