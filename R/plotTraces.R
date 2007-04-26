"plotTraces" <- function(multimodel, multitheta, plotoptions)
{   
    get(getOption("device"))()
    m <- multimodel@modellist   
    t <- multitheta   
    res <- multimodel@fit@resultlist
    for(i in 1:length(m)) {
		x2 <- m[[i]]@x2
		x <- m[[i]]@x
		x2toplot <- if(length(plotoptions@selectedtraces) == 0) 
			    x2toplot <- 1:length(x2)
			    else plotoptions@selectedtraces
		fitted <- matrix(nrow = m[[i]]@nt, 
				 ncol = m[[i]]@nl)
		irfmu <- vector()
		for(j in 1:length(x2)){
		     fitted[,j] <- res[[i]]@fitted[[j]]
		     if(m[[i]]@weight) 
		          fitted[, j] <- fitted[,j]/m[[i]]@weightM[, j]
		     
		     irfmu[j] <- res[[i]]@irfvec[[j]][1]

		}
		if (m[[i]]@mirf) {
		   irfstart <- m[[i]]@x[which(m[[i]]@measured_irf == 
		   max(m[[i]]@measured_irf))]
		   irfmu <- rep(irfstart, m[[i]]@nl)
		}
		 po <- plotoptions@paropt
		 po$oma <- c(0,0,4,0)
		 po$mfrow <- n2mfrow(length(x2toplot))
		 par(po)
		 for (j in 1:length(x2)) {
                  if(j %in% x2toplot) {
		    linlogplot(x, m[[i]]@psi.df[, j], irfmu[j], 
		    plotoptions@linrange, type = "l", xlab = plotoptions@xlab, 
		    ylab = "amplitude",  main = signif(m[[i]]@x2[j]),  
		    col = i, xlim = c(min(x), max(x)) )
		    lines(linloglines(x, irfmu[j], plotoptions@linrange), 
		    fitted[, j], col = i, lty = 2, type = "l")
                   }
		}
		if(length(plotoptions@title) != 0){
			mtext(plotoptions@title, side=3,outer=TRUE,line=1)
			par(las=2)
	        }
		else 
    		    mtext(paste("Traces for dataset", i), side=3,outer=TRUE,
		    line=1)
	      			    
           # MAKE PS
           if(dev.interactive() && length(plotoptions@makeps) != 0) {
		dev.print(device=postscript, 
		file=paste(plotoptions@makeps, "_", i, "_traces.ps", 
		sep=""))
           }
      } 	       	 
 }

