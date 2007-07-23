"set_plotter_kin" <- function () 
{
    setMethod("plotter", signature(model = "kin"), function(model, 
        multimodel, multitheta, plotoptions) {
	if(length(plotoptions@paropt) == 0)
	          plotoptions@paropt <- par(mgp = c(1.5, 1, 0), 
			      mai = c(0.5, 0.6, .5, 0.5),mar=c(3,3,4,1))
	plotoptions@addest <- c("kinpar")
	sumonls <- multimodel@fit@nlsres$sumonls
	onls <- multimodel@fit@nlsres$onls
	resultlist <- multimodel@fit@resultlist
	if(length(plotoptions@superimpose) > 0) {
	    if (!plotoptions@notraces) 
	       plotTracesSuper(multimodel, multitheta, plotoptions)
        }
        else {
            if (!plotoptions@notraces) 
	       plotTraces(multimodel, multitheta, plotoptions)
        }
	if(plotoptions@residplot) {
		if(!plotoptions@FLIM) 
		   plotResids(multimodel, multitheta, plotoptions) 
	}
	if(plotoptions@writefit || plotoptions@writefitivo)
		writeFit(multimodel, multitheta, plotoptions)
        if (length(plotoptions@breakdown)>0) 
            plotKinBreakDown(multimodel, multitheta, 
                plotoptions)  
        if (plotoptions@FLIM) {
            plotFLIM(multimodel, multitheta, plotoptions)
            return()
        }
	get(getOption("device"))()
	par(mgp = c(2, 1, 0), mar=c(3,3,3,2), oma = c(1,0,4,0), 
	mfrow=c(plotoptions@summaryplotrow, plotoptions@summaryplotcol))
        m <- multimodel@modellist
        t <- multitheta
        allx2 <- allx <- vector() 
	for(i in 1:length(m)) {
	  allx2 <- append(allx2, m[[i]]@x2) 
	  allx <- append(allx, m[[i]]@x)
	}
	xmax <- max(allx)
        xmin <- min(allx)
	x2max <- max(allx2)
        x2min <- min(allx2)
        conmax <- list()
	tauList <- list() 
	muList <- list()       
	contoplotList <- list() 
	minc <- maxc <- 0 
	if(m[[i]]@anispec$useparperp) {
		calcAniSignal(m, plotoptions)
	}
	for (i in 1:length(m)) {
            irfmu <- irftau <- vector()
            if (m[[i]]@dispmu) {
                for (j in 1:length(m[[i]]@x2)) 
                       irfmu[j] <- resultlist[[i]]@irfvec[[j]][1]
            }
            else {
                if (m[[i]]@mirf) 
                  irfstart <- m[[i]]@x[which(m[[i]]@measured_irf == 
                    max(m[[i]]@measured_irf))]
                else irfstart <- 0
                irfmu <- rep(irfstart, m[[i]]@nl)
            }
	    muList[[i]] <- irfmu 
	    
	    if (m[[i]]@disptau) {
                for (j in 1:length(m[[i]]@x2)) {
                  if (m[[i]]@cohspec$type == "freeirfdisp") 
                    irftau[j] <- resultlist[[i]]@irfvec[[1]][[j]][2]
		  else 
		       irftau[j] <- resultlist[[i]]@irfvec[[j]][2]
		}
	    }
	    tauList[[i]] <- irftau 
            if (m[[i]]@cohspec$type == "freeirfdisp") {
                cohirf <- irfparF(resultlist[[i]]@cohirf[[1]], 
                  m[[i]]@lambdac, m[[i]]@x2[1], 1, m[[i]]@dispmu, 
                  t[[i]]@parmu[[2]], m[[i]]@disptau, t[[i]]@partau, 
                  m[[i]]@dispmufun, m[[i]]@disptaufun, m[[i]]@irffun)
            }
            else 
                  cohirf <- vector()
 	    irfvec <- resultlist[[i]]@irfvec[[1]]
	    C <- compModel(k = t[[i]]@kinpar, 
                kinscal = m[[i]]@kinscal, x = m[[i]]@x, irfpar = irfvec, 
                irf = m[[i]]@irf, seqmod = m[[i]]@seqmod, 
		fullk = m[[i]]@fullk, 
                kmat = m[[i]]@kmat, jvec = t[[i]]@jvec, 
		dscalspec = m[[i]]@dscalspec, 
                drel = t[[i]]@drel, cohspec = m[[i]]@cohspec, 
                coh = t[[i]]@coh, lamb = 1, 
                dataset = i, cohirf = cohirf, mirf = m[[i]]@mirf, 
                convalg = m[[i]]@convalg, measured_irf = m[[i]]@measured_irf,
		speckin2 = m[[i]]@speckin2, 
		usekin2 = m[[i]]@usekin2, kinpar2 = t[[i]]@kinpar2, 
		kin2scal = t[[i]]@kin2scal, reftau = m[[i]]@reftau, 
		anispec = m[[i]]@anispec, 
		anipar = t[[i]]@anipar, cohcol = m[[i]]@cohcol)
	      if(plotoptions@writerawcon) 
		 write.table(C, file=paste(plotoptions@makeps,
		 "_rawconcen_dataset_", i, ".txt", sep=""), quote = FALSE,
		  row.names = m[[i]]@x)
	      if(length(plotoptions@writeplaincon) > 0){
		xplot <- plotoptions@writeplaincon$x 

		CWRITE <- compModel(k = t[[i]]@kinpar, 
                kinscal = m[[i]]@kinscal, x = xplot, irfpar = irfvec, 
                irf = m[[i]]@irf, seqmod = m[[i]]@seqmod, 
		fullk = m[[i]]@fullk, 
                kmat = m[[i]]@kmat, jvec = m[[i]]@jvec, 
		dscalspec = m[[i]]@dscalspec, 
                drel = t[[i]]@drel, cohspec = m[[i]]@cohspec, 
                coh = t[[i]]@coh, lamb = 1, 
                dataset = i, cohirf = cohirf, mirf = m[[i]]@mirf, 
                convalg = m[[i]]@convalg, measured_irf = m[[i]]@measured_irf,
		speckin2 = m[[i]]@speckin2, 
		usekin2 = m[[i]]@usekin2, kinpar2 = t[[i]]@kinpar2, 
		kin2scal = t[[i]]@kin2scal, reftau = m[[i]]@reftau, 
		anispec = m[[i]]@anispec,  anipar = t[[i]]@anipar, 
		cohcol = m[[i]]@cohcol)
	       write.table(CWRITE, file=paste(plotoptions@makeps,
		 "_plaincon_dataset_", i, ".txt", sep=""), quote = FALSE,
		  row.names = linloglines(xplot, irfvec[1], 0  ))
	      } 
            contoplotList[[length(contoplotList)+1]] <- getConToPlot(
	    doClpConstr(C, 1, m[[i]]@clpCon, t[[i]]@clpequ, dataset = i), 
                m[[i]]@cohspec, m[[i]]@cohcol)
           minc <- min(minc, min(contoplotList[[length(contoplotList)]]))
	   maxc <- max(maxc, max(contoplotList[[length(contoplotList)]]))
	   
           conmax[[i]] <- attributes(
	   contoplotList[[length(contoplotList)]])$max
	   }
	   for(i in 1:length(m)) {
           matlinlogplot(m[[i]]@x, contoplotList[[i]], muList[[i]][1], 
	   plotoptions@linrange, 
           type = "l", add = !(i == 1), lty = i, ylab = "concentration", 
           xlab = plotoptions@xlab, main = "Concentrations", 
	   xlim = c(xmin, xmax), ylim = c(minc, maxc) )
            if(plotoptions@writecon) 
		 write.table(contoplotList[[i]], file=paste(plotoptions@makeps,
		 "_concen_dataset_", i, ".txt", sep=""), quote = FALSE,
		 row.names = m[[i]]@x)
	}
	if(length(model@kin2scal)!=0) {
		perA<-vector()
		for(i in 1:length(m)) 
		      perA <- append(perA, t[[i]]@kin2scal[2])
		
		matplot(1:length(m), perA, type = "l", 
		main = "% Concentration Photoconverted", 
		xlab = "Dataset number", ylab = "percent")
		
	}
        spectralist <- getSpecList(multimodel, t)

	mins <- maxs <- 0 
	specList <- list() 
        for (i in 1:length(m)) {
            if (length(conmax) > 0) 
                spec <- getSpecToPlot(spectralist[[i]], conmax[[i]], 
		m[[i]]@cohcol,  plotoptions@plotcohcolspec)
            else spec <- spectralist[[i]]
	    mins <- min(mins, min(spec))
	    maxs <- max(maxs, max(spec))
	    specList[[length(specList)+1]] <- spec
	  }
	  for (i in 1:length(m)) {
	    matplot(m[[i]]@x2, specList[[i]], type = "l", main = "Spectra", 
                xlab = plotoptions@ylab, ylab = "amplitude", lty = i, 
		add = !(i == 1), xlim=c(x2min,x2max), ylim=c(mins, maxs))
	     if(plotoptions@writespec)
	        write.table(specList[[i]], file=paste(plotoptions@makeps,
		 "_spec_dataset_", i, ".txt", sep=""),
		 row.names = m[[i]]@x2, quote=FALSE) 
	}
	
	abline(0,0)
        for (i in 1:length(m)) {
            matplot(m[[i]]@x2, normdat(specList[[i]]), type = "l", 
	    main = "Normalized spectra",  xlim=c(x2min,x2max),
                xlab = plotoptions@ylab, ylab = "amplitude", lty = i, 
		add = !(i == 1))
	    abline(0,0)
	    if(plotoptions@writenormspec)
	         write.table( normdat(specList[[i]]),	
		 file=paste(plotoptions@makeps,
		 "_normspec_dataset_", i, ".txt", sep=""),
		 row.names = m[[i]]@x2, quote=FALSE)  
	}
	abline(0,0)
	minmu <- min(muList[[1]])
	maxmu <- max(muList[[1]])
	for (i in 1:length(m)) {
	   	minmu <- min(minmu, min(muList[[i]]))
		maxmu <- max(maxmu, max(muList[[i]]))
	}
	notplotted <- TRUE
	for (i in 1:length(m)) {
	    if(m[[i]]@dispmu) {
	      matplot(m[[i]]@x2, muList[[i]], type = "l", 
	      main = "IRF location",  xlim=c(x2min,x2max),
	      add = !notplotted, ylim = c(minmu, maxmu), col=i,
	      xlab = plotoptions@ylab, ylab = "IRF location")
	      notplotted <- FALSE  
	  }
       }
        if(m[[1]]@disptau) {
		mintau <- min(tauList[[1]])
		maxtau <- max(tauList[[1]])
	}
	for (i in 1:length(m)) {
	    if(m[[i]]@disptau) {
	   	mintau <- min(mintau, min(tauList[[i]]))
		maxtau <- max(maxtau, max(tauList[[i]]))
	    }
	}
	notplotted <- TRUE
	for (i in 1:length(m)) {
	    if(m[[i]]@disptau) {
	      matplot(m[[i]]@x2, tauList[[i]], type = "l", 
	      main = "IRF width",   add = !notplotted, col=i,
	       xlim=c(x2min,x2max),ylim = c(mintau, maxtau),
	      xlab = plotoptions@ylab, ylab = "IRF width")
	      notplotted <- FALSE
	    }
	}
	  
      	svddatalist <- list()
        residlist <- svdresidlist <- list()
        for (i in 1:length(m)) {
            residuals <- matrix(nrow = m[[i]]@nt, ncol = m[[i]]@nl)
            for (j in 1:length(resultlist[[i]]@resid)) {
                residuals[, j] <- resultlist[[i]]@resid[[j]]
            }
            svdresidlist[[length(svdresidlist) + 1]] <- doSVD(residuals, 2, 2)
            residlist[[length(residlist) + 1]] <- residuals
	    svddatalist[[length(svddatalist) + 1]] <- doSVD(multimodel@data[[i]]@psi.df, 2, 2)
        }
	maxleftr <- minleftr <- maxrightr <- minrightr <- maxvalr <- minvalr <- 0
	maxleftd <- minleftd <- maxrightd <- minrightd <-  maxvald <- minvald <- 0
	for(i in 1:length(m)) {
	      maxleftr <- max(svdresidlist[[i]]$left[,1], maxleftr)
	      maxrightr <- max(svdresidlist[[i]]$right[1,], maxrightr)
	      minleftr <- min(svdresidlist[[i]]$left[,1], minleftr)
	      minrightr <- min(svdresidlist[[i]]$right[1,], minrightr)
	      maxvalr <- max(svdresidlist[[i]]$values, maxvalr)
	      minvalr <- min(svdresidlist[[i]]$values, minvalr)

	      maxleftd <- max(svddatalist[[i]]$left[,1], maxleftd)
	      maxrightd <- max(svddatalist[[i]]$right[1,], maxrightd)
	      minleftd <- min(svddatalist[[i]]$left[,1], minleftd)
	      minrightd <- min(svddatalist[[i]]$right[1,], minrightd)
	      maxvald <- max(svddatalist[[i]]$values, maxvald)
	      minvald <- min(svddatalist[[i]]$values, minvald)
	 
	}
	##START RESID PLOTTING
	for (i in 1:length(m)) {          
	    limd<- max(  max(residlist[[i]]), abs(min(residlist[[i]]))) 
	    image.plot(m[[i]]@x, m[[i]]@x2, 
	    residlist[[i]], xlab = plotoptions@xlab, 
	    ylab = plotoptions@ylab, 
            main = paste("Residuals Dataset", i),  
		zlim=c(-limd,limd),
		col = diverge_hcl(40, h = c(0, 120), c = 60, 
		l = c(45, 90), power = 1.2))
        }
	for (i in 1:length(m)) {     
	   if (m[[i]]@nt > 1 && m[[i]]@nl > 1) {
	       xpos <- m[[i]]@x 
	       xpos[which(xpos <= 0)] <- NA  
	       matplot(xpos, svdresidlist[[i]]$left[,1], 
	       type = "l", ylim = c(minleftr, maxleftr), 
               main = "Left sing. vectors residuals ",  add = !(i == 1),
               log = "x", xlab = plotoptions@xlab, col=i, 
	       ylab = plotoptions@ylab)
           
        }
       }
       for (i in 1:length(m)) {     
	   if (m[[i]]@nt > 1 && m[[i]]@nl > 1) {
	    
            matplot(m[[i]]@x2, svdresidlist[[i]]$right[1,],
	    type = "l", xlim=c(x2min,x2max),
	    ylim = c(minrightr, maxrightr), 
            main = "Right sing. vectors residuals ", 
	    xlab = plotoptions@xlab, add = !(i == 1),
            col=i, ylab = plotoptions@ylab)
           }
       }
       for (i in 1:length(m)) {     
	    if(i == 1)
	      plot(1:length(svdresidlist[[i]]$values), 
	      log10(svdresidlist[[1]]$values), xlab="",
              ylab = plotoptions@ylab, col=i, 
              main = "Sing. values residuals", type = "b")
           else
	      lines(1:length(svdresidlist[[i]]$values), 
	      log10(svdresidlist[[i]]$values), type = "b", col=i)
       }
       ##START DATA PLOTTING
       	for (i in 1:length(m)) {     
	   if (m[[i]]@nt > 1 && m[[i]]@nl > 1) {
	    xpos <- m[[i]]@x 
	    xpos[which(xpos <= 0)] <- NA  
	    matplot(xpos, svddatalist[[i]]$left[,1], type = "l", 
            main = "Left sing. vectors data",  add = !(i == 1),
            log = "x", xlab = plotoptions@xlab, col=i, 
	    ylim = c(minleftd, maxleftd),
	    ylab = plotoptions@ylab)
           }
       }    
       for (i in 1:length(m)) {     
	   if (m[[i]]@nt > 1 && m[[i]]@nl > 1) {
	    matplot(m[[i]]@x2, svddatalist[[i]]$right[1,], type = "l", 
            main = "Right sing. vectors data", xlim=c(x2min,x2max),
	    ylim = c(minrightd, maxrightd),
	    xlab = plotoptions@xlab, add = !(i == 1),
            col=i, ylab = plotoptions@ylab)
           }
       }
       for (i in 1:length(m)) {     
	    if(i == 1)
	      plot(1:length(svddatalist[[i]]$values), 
	      log10(svddatalist[[1]]$values), xlab="",
              ylab = plotoptions@ylab, col=i,
              main = "Sing. values data", type = "b")
           else
	      lines(1:length(svddatalist[[i]]$values), 
	      log10(svddatalist[[i]]$values), type = "b", col=i)
       }
       ## OTHER PLOTS
        for (i in 1:length(m)) {
            pl <- FALSE
            if (length(m[[i]]@dscalspec$perclp) != 0) 
                if (m[[i]]@dscalspec$perclp) {
                  if (!pl) {
                    plot(m[[i]]@x2, t[[i]]@drel, 
		    main = "Dataset scaling per clp", 
                    xlab = plotoptions@ylab, ylab = "", type = "l")
                    pl <- TRUE
                  }
                  else lines(m[[i]]@x2, t[[i]]@drel, type = "l", 
                    col = i)
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

  
	par(mfrow=c(plotoptions@summaryplotrow,1), new=TRUE)
	plotEstout <- plotEst(multimodel, plotoptions, tr=TRUE)
	writeEst(multimodel, multitheta, plotoptions, plotEstout)
        displayEst(plotoptions)

        if (dev.interactive() && length(plotoptions@makeps) != 0) {
	   if(plotoptions@output == "pdf")
				      pdev <- pdf 
	   else  pdev <- postscript
	    dev.print(device = pdev, file = paste(plotoptions@makeps, 
                "_summary.",  
		plotoptions@output,
		sep = ""))
        }
        if (plotoptions@plotkinspec) {
            plotKinSpec(multimodel, t, plotoptions)
        }
	if (plotoptions@kinspecest) {
            plotKinSpecEst(t, plotoptions, multimodel)
        }
	
	

    })
}
