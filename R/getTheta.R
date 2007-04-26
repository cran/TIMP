"getTheta" <- function (mod) 
{
   modelspec <- mod@modelspec
   modellist <- mod@modellist
   datasetind <- mod@datasetind 
   th <- vector()
   parorder <- list()
   for(i in 1:length(modelspec)) {
	 ds <- which(datasetind == i)
	 model <- modellist[[ds[1] ]]
	 fixed <- model@fvecind
	 prel <- model@pvecind
	 ppars <- append(model@parnames, "prel")
	 for (p in ppars) {
	     if(length(slot(modelspec[[i]], p)) != 0) {  
		removepar <- sort(unique(append(fixed[[p]], prel[[p]])))
		parapp <- if (length(removepar) != 0) 
			  unlist(slot(model, p))[-removepar]
			  else unlist(slot(model, p))
	     if(length(parapp) != 0) 
		ind <- (length(th) + 1):(length(th) + length(parapp))
	     else ind <- vector()
	     
	     parorder[[length(parorder)+1]] <- list(name=p, ind=ind, 
	     dataset = ds, rm=removepar)
	
	     if(p %in% model@positivepar && length(parapp) != 0) 
		  th <- append(th, log(parapp))
	     else 
		  th <- append(th, parapp)
             }
	}  
   }
    .currModel@parorder <<- parorder 
    getDiffTheta(th, mod)
}
    