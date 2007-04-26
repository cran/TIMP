"plotKinSpec" <-
function(multimodel, t, plotoptions, newplot=TRUE, max_x2=NA, min_x2=NA, 
ylim=vector(), kinspecerr=FALSE)
{
	m <- multimodel@modellist
	resultlist <- multimodel@fit@resultlist 
	if(newplot) {
	   get(getOption("device"))()
	   par(oma = c(0,0,4,0),cex=1.5)
	   kinspecerr <- plotoptions@kinspecerr
	}
	if(is.na(max_x2) || is.na(max_x2))
			 withlim <- FALSE 
	else		 withlim <- TRUE
	## is x2 decreasing? assume answer same for all datasets 
	x2_decr <- if(m[[1]]@x2[1] < m[[1]]@x2[m[[1]]@nl]) FALSE
	       else TRUE
        allx2 <- allx <- vector() 
	for(i in 1:length(m)) {
	  allx2 <- append(allx2, m[[i]]@x2) 
	  allx <- append(allx, m[[i]]@x)
	}
	specList <- list() 
	maxs <- mins <- maxspecdim <- 0
	specList <- getSpecList(multimodel, t)
	for(i in 1:length(m)) {
		      cohcol <- m[[i]]@cohcol 
		      spec <- getSpecToPlot(specList[[i]], 1, 
		      cohcol)
		       if(!identical(m[[i]]@cohcol, 0))	      
			   spec <- spec[,-cohcol] 
		      specList[[i]] <- spec
		      maxs <- max(maxs, max(spec))
		      mins <- min(mins, min(spec))
		      maxspecdim <- max(maxspecdim, dim(spec)[2])
	}	      			      
	if(!withlim) 
		xlim <- c(min(allx2),max(allx2))
	else xlim <- c(min_x2, max_x2)
	if(length(plotoptions@xlimspec) == 2) 
		xlim <- plotoptions@xlimspec
	if(length(plotoptions@ylimspec) == 2) 
		ylim <- plotoptions@ylimspec
	if(length(ylim) == 0) 
		ylim <- c(mins, maxs)
	if (plotoptions@normspec)  ylim <- c(-1,1)
	   plotted<-FALSE
	   if(kinspecerr)   
		errtList <- getSpecList(multimodel, t,  getclperr=TRUE) 
		
	   for(i in 1:length(m)) {
		if(kinspecerr) {
			if(plotoptions@writeclperr) 
		          write.table(errtList[[i]], 
			  file=paste(plotoptions@makeps,
		          "_std_err_clp_", i, ".txt", sep=""), 
		          quote = FALSE, row.names = m[[i]]@x2)
	   
			 if(!identical(m[[i]]@cohcol, 0))
			   errtList[[i]] <- errtList[[i]][,-m[[i]]@cohcol] 
	        }
		if (plotoptions@normspec) 
				    sp <-  normdat(specList[[i]])
		      else 
				    sp <- specList[[i]]
	      
	      for(j in 1:dim(sp)[2]) {
		    if(plotoptions@specinterpol) { 
		       xx <- predict(interpSpline(m[[i]]@x2, 
			sp[,j], bSpline=plotoptions@specinterpolbspline),
			nseg = plotoptions@specinterpolseg)
			
		       if(!plotted) {
			plot(xx, lty = j, main = "", xlab = plotoptions@xlab,
			ylab="amplitude", xlim =xlim,ylim=ylim, col = j,
			type="l")
                        plotted<-TRUE 
			}
			else lines(xx, col = j, lty = j)
			if(kinspecerr)
			 plotCI(m[[i]]@x2, sp[,j], uiw=errtList[[i]][,j], pch
			 = if(plotoptions@specinterpolpoints) 26-j else NA,
			 col = j, sfrac = 0, type="p", gap = 0, add =TRUE,
			 labels = "")
		     
		      if(plotoptions@writespecinterpol) 
		          write.table(xx$y, file=paste(plotoptions@makeps,
		          "_smoothedspectracomponent_", j, ".txt", sep=""), 
		          quote = FALSE, row.names = xx$x)
		     }
		     else 
		      if(kinspecerr)
		       plotCI(m[[i]]@x2, sp[,j], 
		       uiw=errtList[[i]][,j], 
		       main = "", xlab = plotoptions@xlab,
		       ylab="amplitude", lty = i, xlim =xlim,ylim=ylim,
		       col = j, sfrac = 0,  type="l", gap = 0,
		       add = !(i == 1 &&	j == 1), labels = "")
		      else {
		       if(!plotted) {
			plot(m[[i]]@x2, sp[, j], lty = j, main = "", 
			xlab = plotoptions@xlab,
			ylab="amplitude", xlim =xlim,ylim=ylim, col = j,
			type="l")
                        plotted<-TRUE 
		       }
			else lines(m[[i]]@x2, sp[, j], col = j, lty = j)

		     }
		}
	    }
	 if (newplot && length(plotoptions@title) != 0) {
            mtext(plotoptions@title, side = 3, outer = TRUE, 
                line = 1)
            par(las = 2)
        }
	abline(0,0)
	if (dev.interactive() && length(plotoptions@makeps) != 0) {
            dev.print(device = postscript, file = paste(plotoptions@makeps, 
                "_kinspec.ps", sep = ""))
        }
}

