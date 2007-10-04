"plotTracesSuper" <-
function(multimodel, multitheta, plotoptions)
{
  get(getOption("device"))() 
  m <- multimodel@modellist   
    t <- multitheta   
    res <- multimodel@fit@resultlist
    groups <- multimodel@groups 
    if(length(plotoptions@superimpose) == 0) 
	superimpose <- 1:length(m)			      
    else 
	 superimpose <- plotoptions@superimpose
    divdrel <- plotoptions@divdrel
    allx2 <- allx <- vector() 
    for(i in superimpose) {
	  allx2 <- append(allx2, m[[i]]@x2) 
	  allx <- append(allx, m[[i]]@x)
    }
    xmax <- max(allx)
    xmin <- min(allx)
    allx2 <- sort(unique(allx2))
    ## set up plot layout 
    par(plotoptions@paropt)
    par(oma = c(0,0,4,0))
    if(length(plotoptions@selectedtraces) > 0 ) {
	seltraces <- plotoptions@selectedtraces 
	xx <- vector()
	for(i in superimpose) {
	      xx <- append(m[[i]]@x2[seltraces],xx)
	}
	lensel <- length(unique(xx))

    }	
    else {
	 seltraces <- 1:length(allx2) 
	 lensel <- length(seltraces)
    }
    par(mfrow = n2mfrow(lensel))
    for (j in 1:length(allx2)) {
	 plotted <- FALSE 
	 for(i in 1:length(m)) {
	      k <- which(m[[i]]@x2  %in% allx2[j])[1]
	      if(i %in% superimpose && k %in% seltraces && 
	      k <= m[[i]]@nl ) {	
		
	        irfmu <- res[[i]]@irfvec[[k]][1]
		
		data <- m[[i]]@psi.df[,k] 
		fitted <- res[[i]]@fitted[[k]] 
		if(divdrel && length(t[[i]]@drel)!=0) 
			if(length(m[[i]]@dscalspec$perclp)!=0) 
				if(m[[i]]@dscalspec$perclp)	{	
				      data <- data/t[[i]]@drel[k]
				      fitted <- fitted/t[[i]]@drel[k]
				}
				else {
				      data <- data/t[[i]]@drel
				      fitted <- fitted/t[[i]]@drel
				}
		if(m[[i]]@weight) 
		          fitted <- fitted/m[[i]]@weightM[, k]
		if(!plotted){ 
		       linlogplot(m[[i]]@x, data, irfmu, 
		       plotoptions@linrange, type = "l", 
		       xlim=c(xmin,xmax), 
		       xlab = plotoptions@xlab,  
		       main = signif(m[[i]]@x2[k]), 
		       ylab ="amplitude", col = i)
		       plotted <- TRUE
		}
		else 
		       lines(linloglines(m[[i]]@x, irfmu, 
		       plotoptions@linrange), 
		       data, col = i, type = "l")
	
		lines(linloglines(m[[i]]@x, irfmu, plotoptions@linrange), 
		fitted, col = i, lty=3, type = "l")
            }
           }
	 }
if(length(plotoptions@title) != 0){
			tit <- plotoptions@title
			if(plotoptions@addfilename) tit <- paste(tit,m[[i]]@datafile)
    }
    else {
                        tit <- ""
		        if(plotoptions@addfilename) tit <- paste(tit, m[[i]]@datafile)
    }
    mtext(tit, side = 3, outer = TRUE, line = 1)
    par(las = 2)
     
     # MAKE PS
        if(dev.interactive() && length(plotoptions@makeps) != 0) {
		if(plotoptions@output == "pdf")
				      pdev <- pdf 
		else  pdev <- postscript
		dev.print(device=pdev, 
		file=paste(plotoptions@makeps, "_selectedtraces.", 
		plotoptions@output,
		sep=""))
        }
}

