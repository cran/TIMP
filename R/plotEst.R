"plotEst" <- function (mod, plotoptions, tr=FALSE)  
{
	options(scipen=-2) 
	m <- mod@modellist
	model <- m[[1]]
	sumonls <- mod@fit@nlsres$sumonls
	parorder <- mod@parorder
	x <- xret<- list()
	cnt <- 1
	for(i in 1:length(parorder)){
	      removepar <- parorder[[i]]$rm 
	      name <- parorder[[i]]$name
	      len <- length(unlist(slot(model, name)))
	      for(j in 1:len) { 
		    if(j %in% removepar) {
			  pest <- unlist(slot(model, name))[j]
			  t <- "NA"
			  xret[[length(xret)+1]] <- list(name=name, pest=pest, 
			  t=t, ind=j)
		    }
		    else{
			pest <- sumonls$parameter[cnt, 1] 
			t <- sumonls$parameters[cnt, 3]
			if(name %in% model@positivepar) {
				  std <- sumonls$parameters[cnt, 2]
				  std <- max( exp(pest + std)  -exp(pest),
				  exp(pest) -  exp(pest - std) ) 	
				  pest <- exp(pest)
				  t <- pest/std
			}
			cnt <- cnt + 1
			xret[[length(xret)+1]] <- list(name=name, 
			pest=signif(pest), t=signif(t), ind=j)
			pest <- signif(pest, digits=2)
		        t <- signif(t, digits=2)
		   }
		   
		 if(name %in% plotoptions@addest) 
		   x[[length(x)+1]] <- list(name=name, pest=pest, t=t,
		   ind = j)
		  
	      }
       }
       parorder <- mod@parorderdiff
       y <- yret <- list()
       if(length(parorder) > 0) {
       for(i in 1:length(parorder)){
	      name <- parorder[[i]]$name 
	      removepar <- parorder[[i]]$rm 
	      dataset <- parorder[[i]]$dataset[1] 
	      ind <- parorder[[i]]$ind
	      indm <- parorder[[i]]$indm
	      for(j in ind) { 
		    pest <- sumonls$parameters[cnt, 1] 
		    t <- sumonls$parameters[cnt, 3]
		    if(name %in% m[[dataset]]@positivepar) {
				  std <- sumonls$parameters[cnt, 2]
				  std <- max( exp(pest + std)  -exp(pest),
				  exp(pest) -  exp(pest - std) ) 	
				  pest <- exp(pest)
				  t <- pest/std
		     }
		     yret[[length(yret)+1]] <- list(name=name, 
		     pest=signif(pest), 
		     t=signif(t), dataset=dataset, ind=indm)
		     pest <- signif(pest, digits=2)
		     t <- signif(t, digits=2)
		     cnt <- cnt + 1
              }
	      for(j in removepar) {
		    pest <- unlist(slot(m[[dataset]], name))[j]
		    t <- "NA"
		    yret[[length(yret)+1]] <- list(name=name, pest=pest, 
			t=t, dataset=dataset, ind=indm)
	      }	   
	      if(name %in% plotoptions@addest)
		  y[[length(y)+1]] <- list(name=name, pest=pest, t=t,
		  ind=indm, dataset=dataset) 
				      
        }
       }
       parorder <- mod@parorderchange
       z <- zret <- list()
       if(length(parorder) > 0) {
        for(i in 1:length(parorder)){
	      name <- parorder[[i]]$name 
	      removepar <- parorder[[i]]$rm 
	      dataset <- parorder[[i]]$dataset[1]
	      len <- length(unlist(slot(m[[dataset]], name)))
	      for(j in 1:len) { 
		    if(j %in% removepar) {
			  pest <- unlist(slot(m[[dataset]], name))[j]
			  t <- "NA"
			  zret[[length(zret)+1]] <- list(name=name, 
			  pest=pest, t=t, dataset=dataset, ind=j)
		    }
		    else{
			pest <- sumonls$parameter[cnt, 1]
			t <- sumonls$parameters[cnt, 3]
			if(name %in% m[[dataset]]@positivepar) {
				  std <- sumonls$parameters[cnt, 2]
				  std <- max( exp(pest + std)  -exp(pest),
				  exp(pest) -  exp(pest - std) ) 	
				  pest <- exp(pest)
				  t <- pest/std
			}
			cnt <- cnt + 1
			zret[[length(zret)+1]] <- list(name=name, 
			pest=signif(pest), t=signif(t), dataset=dataset, 
			ind=j)
			pest <- signif(pest, digits=2)
			t <- signif(t, digits=2)
		   }
		 if(name %in% plotoptions@addest)
		  z[[length(z)+1]] <- list(name=name, pest=pest, t=t, ind=j, 
		  dataset=dataset)
	      }
        }
       }
       lall <- length(x)+length(y) + length(z)
       xmat <- matrix(nrow=lall+1, ncol=4, dimnames = list(rep("", lall+1), 
       c("PAR", "EST", "T VAL", "")))
       if(length(x) > 0)
        for(i in 1:length(x)) {
	     xmat[i,] <- c(paste(x[[i]]$name,x[[i]]$ind, x[[i]]$dataset), 
	     x[[i]]$pest, x[[i]]$t, NA)
	     if(x[[i]]$name == "kinpar") 
		       xmat[i, 4]<-paste("tau:",signif(1/x[[i]]$pest,digits=2))
       }
       if(length(y) > 0 || length(z) > 0) {
	 x2 <- xmat[(1+length(x)):(lall+1),]
	 xmat <- rbind(xmat[1:length(x), ], 
	 c(NA, "PER-D:",NA, NA), x2)
	 xmat[(lall+2),] <- rep(NA, 4)
       }
       else xmat[(lall+1),] <- rep(NA, 4)
       if(length(y) > 0)
         for(i in 1:length(y)) {
	     xmat[i+length(x)+1,] <- c(paste(y[[i]]$name,y[[i]]$ind, y[[i]]$dataset),
	      y[[i]]$pest, y[[i]]$t, NA)
	      if(y[[i]]$name == "kinpar") 
		       xmat[i+length(x)+1, 4]<-paste("tau:",signif(1/y[[i]]$pest,digits=2))
       }
       if(length(z) > 0)
         for(i in 1:length(z)) {
	     xmat[i+length(x)+length(y)+1,] <- c(paste(z[[i]]$name,z[[i]]$ind, 
	     z[[i]]$dataset), z[[i]]$pest,z[[i]]$t, NA)
	     if(z[[i]]$name == "kinpar") 
		       xmat[i+length(x)+length(y)+1, 4]<-paste("tau:",signif(1/z[[i]]$pest,digits=2))
        }
      if(!tr) 
	      textplot(xmat, mar=c(1,1,1,1))
      else 
	      textplot(t(xmat), mar=c(1,1,1,1))
      title(main=paste("RMSE:", signif(sumonls$sigma, digits=3)))
      options(scipen=0)

      list(x=xret, y=yret, z=zret)
    
}