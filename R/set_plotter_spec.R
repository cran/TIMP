"set_plotter_spec" <- 
function(){

  setMethod("plotter", signature(model="spec"), 
    function (model, multimodel, multitheta, plotoptions) 
    {     
  	# PLOT SPECTRA IN NEW WINDOW
        #if(length(plotoptions$superimpose) > 0) {
	#	 if(length(plotoptions$notraces) == 1) {} 
	#	 else
	#		plotSpectraSuper(model, multimodel, multitheta, resid, 
	#	        plotoptions)
	#	if(length(plotoptions$selectedtraces) > 0)
	#	      plotSelectedSpectraSuper(model, multimodel, multitheta, 
	#	      resid, plotoptions)
	#}
	#else {
	        if(!plotoptions@nospectra) 
		   plotSpectra(model, multimodel, multitheta, 
		       plotoptions)
	#}
	if(length(plotoptions@residplot) == 1) {
		plotResids(multimodel, multitheta, plotoptions) 
	}
	plotoptions@addest <- c("specpar")
	plotoptions@paropt <-  par(mgp = c(2, 1, 0), mar=c(4,4,.5,.5))
	get(getOption("device"))()
	if(!identical(model@title,""))
		tit <- c(0,0,1,0)
	else 
		tit <- c(0,0,0,0)
	plotrow<-4
	plotcol<-4
	par(plotoptions@paropt)
	par(mfrow = c(plotrow, plotcol))
	nt <- model@nt
	nl <- model@nl
	x <- model@x
	x2 <- model@x2
	increasing_x2 <- model@x2[2] > model@x2[1]
	groups <- multimodel@modeldiffs$groups	

	m <- multimodel@modellist
	t <- multitheta 
	resultlist <- multimodel@fit@resultlist
	conmax <- list()       

	conlist <- getClpList(multimodel, t) 

        for(i in 1:length(m)) {
			contoplot <- conlist[[i]]

			matplot(m[[i]]@x, contoplot,
			type = "l", 
                        add=!(i==1), lty=i, ylab = "concentration", 
                        xlab=plotoptions@xlab,main = "Concentrations")
       }

	# SPECTRA 

	for(i in 1:length(m)) {
		   if(m[[i]]@timedep)
		     specpar <- specparF(t[[i]]@specpar, m[[i]]@x[1], 
			1, m[[i]]@specref, m[[i]]@specdispindex, 
			t[[i]]@specdisppar, parmufunc = m[[i]]@parmufunc)
		   else 
		     specpar <- t[[i]]@specpar 
		   
		   spectra <- doClpConstr(specModel(specpar, m[[i]]),
			  clp_ind = 1, clpCon = m[[i]]@clpCon, 
			  clpequ = t[[i]]@clpequ, dataset = i)

		      matplot(m[[i]]@x2, spectra, type = "l", 
		      main = "Spectra", 
		      xlab = plotoptions@ylab, ylab="amplitude", lty = i, 
		      add = !(i ==1)) 
        }
	for(i in 1:length(m)) {
		   if(m[[i]]@timedep)
		     specpar <- specparF(t[[i]]@specpar, m[[i]]@x[1], 
			1, m[[i]]@specref, m[[i]]@specdispindex, 
			t[[i]]@specdisppar, parmufunc = m[[i]]@parmufunc)
		   else 
		     specpar <- t[[i]]@specpar 
		   spectra <- doClpConstr(specModel(specpar, m[[i]]),
			  clp_ind = 1, clpCon = m[[i]]@clpCon, 
			  clpequ = t[[i]]@clpequ, dataset = i)
		      matplot(m[[i]]@x2, normdat(spectra), type = "l", 
		      main = "Normalized spectra", xlab = plotoptions@ylab, 
		      ylab="amplitude", lty = i, add = !(i ==1)) 
	}

	# RESIDUALS 
	# make a list of resid matrix
	residlist <- svdresidlist <- list() 
	for(i in 1:length(m)) {
		residuals <- matrix(nrow = m[[i]]@nt, 
				    ncol = m[[i]]@nl)
		for(j in 1:length(resultlist[[i]]@resid)){ 
		    
		    if(m[[i]]@mod_type != "spec")
			 residuals[,j] <- resultlist[[i]]@resid[[j]]
		    else
			residuals[j,] <- resultlist[[i]]@resid[[j]]
		    
		}
		
		svdresidlist[[length(svdresidlist)+1]] <- doSVD(residuals,2,2) 
		residlist[[length(residlist)+1]] <- residuals 
	}

	if(increasing_x2) {
	   limd<- max(  max(residlist[[1]]), abs(min(residlist[[1]]))) 
	   image(x, x2, residlist[[1]], xlab = plotoptions@xlab, 
	   ylab = plotoptions@ylab, main = "Residuals Dataset 1", 
	   zlim=c(-limd,limd), col=diverge_hcl(40, h = c(0, 120), c = 60, 
	   l = c(45, 90), power = 1.2))
	}
	if(nt > 1 && nl > 1){ 
	      matplot(x, svdresidlist[[1]]$left, type = "l",
	      main = "Left sing. vectors residuals ", log = "x", xlab =
		plotoptions@xlab, col = 1, ylab=plotoptions@ylab)
	      if(length(m) > 1){
		 for(i in 2:length(m)) {
			 matlines(m[[i]]@x, 
			 svdresidlist[[i]]$left, log = "x", 
			 type ="l", col = i)
                 }
	      }
	      matplot(x2, t(svdresidlist[[1]]$right), type = "l",  
	      main = "Right sing. vectors residuals ", xlab = plotoptions@xlab, 
	      col = 1, ylab=plotoptions@ylab)
	      if(length(m) > 1){
		 for(i in 2:length(m)) {
			 matlines(m[[i]]@x2, 
			 t(svdresidlist[[i]]$right), type ="l", col = i)
                 }
	      }
	      plot(1:length(svdresidlist[[1]]$values),
	      log10(svdresidlist[[1]]$values), ylab=plotoptions@ylab, 
              main = "Sing. values residuals", type= "b", xlab="")
	      if(length(m) > 1){
		 for(i in 2:length(m)) {
			 lines(1:length(svdresidlist[[i]]$values),
			 log10(svdresidlist[[i]]$values), 
			 type = "b", col = i)
                 }
	      }
	}
	# DATA
	# make a list of svd data	

	svddatalist <- list() 
	for(i in 1:length(m)) {
		svddatalist[[length(svddatalist)+1]] <- doSVD(
		multimodel@data[[i]]@psi.df,2,2) 
	}
	#if(increasing_x2) 
	#  image(x, x2, multimodel@data[[1]]@psi.df, xlab = plotoptions@xlab, 
	#	ylab = ylab, main = "Dataset 1", 
	#	col = rainbow(16, s = 1, v = 1, start = 0, 
	#	end = .5, gamma = 1))
	if(nt > 1 && nl > 1){ 
	      matplot(x, svddatalist[[1]]$left, type = "l",
	      main = "Left sing. vectors data", log = "x", xlab = plotoptions@xlab, 
	      col = 1, ylab=plotoptions@ylab)
	      if(length(m) > 1){
		 for(i in 2:length(m)) {
			 matlines(m[[i]]@x, 
			 svddatalist[[i]]$left, log = "x", 
			 type ="l", col = i )
                 }
	      }
	      matplot(x2, t(svddatalist[[1]]$right), type = "l", ylab="", 
	      main = "Right sing. vectors data", xlab = plotoptions@ylab, col = 1)
	      if(length(m) > 1){
		 for(i in 2:length(m)) {
			 matlines(m[[i]]@x2, 
			 t(svddatalist[[i]]$right), type ="l", col = i)
                 }
	      }
	      plot(1:length(svddatalist[[1]]$values),
	      log10(svddatalist[[1]]$values), ylab ="",
              main = "Sing. values data", type= "b", xlab="")
	      if(length(m) > 1){
		 for(i in 2:length(m)) {
			 lines(1:length(svddatalist[[i]]$values),
			 log10(svddatalist[[i]]$values), 
			 type = "b", col = i)
                 }
	      }
	}
	# PLOT DSCALING PER CLP
	
        for(i in 1:length(m)) {
	      pl <- FALSE
    	      if(length(m[[i]]@dscalspec$perclp) != 0 )
                if(m[[i]]@dscalspec$perclp){
		  if(!pl) {
		    plot(m[[i]]@x2, t[[i]]@drel, 
		    main = "Dataset scaling per clp", xlab= plotoptions@ylab, 
		    ylab="", type = "l")  
		    pl <- TRUE 
                  } 
	          else 
		    lines(m[[i]]@x2, t[[i]]@drel, type = "l", col = i) 
                }	        
        }
	
	if(length(plotoptions@title) != 0){
			mtext(plotoptions@title, side=3,outer=TRUE,line=1)
			par(las=2)
	}
       # MAKE PS
       if(dev.interactive() && length(plotoptions@makeps) != 0) {
		dev.print(device=postscript, 
		file=paste(plotoptions@makeps, "_summary.ps", sep=""))
       }
       	par(mfrow=c(plotoptions@summaryplotrow,1), new=TRUE)
	plotEstout <- plotEst(multimodel, plotoptions, tr=TRUE)
	writeEst(multimodel, multitheta, plotoptions, plotEstout)
        displayEst(plotoptions)
       if(length(plotoptions@plotkinspec) == 0) 
		plotoptions@plotkinspec <- FALSE
       if(plotoptions@plotkinspec) {
		      plotKinSpec(multimodel, m, t, plotoptions) 
	}
     })	
}
