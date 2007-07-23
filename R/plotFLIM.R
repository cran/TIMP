"plotFLIM" <- function (multimodel, multitheta, plotoptions) 
{
    ## this function results in plots associated with modeling FLIM data

    m <- multimodel@modellist
    t <- multitheta
    if(plotoptions@residplot)
         plotFLIMresid(multimodel, multitheta, plotoptions) 
    if(plotoptions@noFLIMsummary) return()
 for(i in 1:length(m)) {
    k <- t[[i]]@kinpar
    model <- m[[i]]
    get(getOption("device"))()   
    par(plotoptions@paropt)
    par(mgp = c(2, 1, 0), mar=c(3,3,3,2), oma = c(1,0,4,0), cex.main=.95,
	mfrow=c(plotoptions@summaryplotrow, plotoptions@summaryplotcol))
    nt <- model@nt
    nl <- model@nl
    x <- model@x
    x2 <- model@x2
    resultlist <- multimodel@fit@resultlist
    irfmu <- vector()
    cohirfmu <- vector()
    irftau <- vector()
   
    conmax <- list()
    spectralist <- getSpecList(multimodel, t)
	spec <- spectralist[[i]]
	if( (m[[i]]@cohcol != 0)[1])
		spec <- as.matrix(spectralist[[i]][,-m[[i]]@cohcol])

	for (j in 1:dim(spec)[2]) {
            hist(spec[, j], xlab = paste("tau=", signif(1/k[j], 5)), 
	    main = paste("Comp.", j, "amplitude"))
        }
        
        if (dim(spec)[2] > 1) {
            sumspec <- rep(0, dim(spec)[1])
            sumAv <- matrix(0, length(k), model@nl)
            for (j in 1:dim(spec)[2]) {
                sumspec <- sumspec + spec[, j]
            }
            for (j in dim(spec)[2]:1) {
                sumAv[j, ] <- spec[,j]/sumspec
                if (j != 1) 
                  hist(sumAv[j, ], xlab = paste("mean", signif(mean(spec[, 
                    j])/mean(sumspec), 5)), main = paste("Norm. Component", 
                    j))
            }
        }

    colvec <- gray(seq(from = 0, to = 1, length = 100))
    image.plot(model@inten, col = colvec, xlab = "", axes = FALSE, 
        ylab = "", main = "Intensity image")
    require(gclus)
    tt <- (model@inten - min(model@inten))/max(model@inten - 
        min(model@inten))
    colmat <- gray(tt)
    colmat[model@x2] <- "#0000FF"
    dim(colmat) <- dim(model@inten)
    csave <- colmat
    colmat <- t(colmat)
    for (j in 1:(dim(colmat)[2])) 
	colmat[, j] <- rev(colmat[, j])
    plotcolors(colmat, main = "Region of interest")
    tracemat <- matrix(0, dim(model@inten)[1], dim(model@inten)[2])

   
    ## find out the rows where the selected pixels begin and end
   c1 <- c2 <- TRUE 
   for(j in 1:dim(csave)[2]) { 
	 if(c1)
	    if("#0000FF" %in% csave[,j]) {	
	        colstart <- j
		c1 <- FALSE 
            }
	 if(c2)
	    if("#0000FF" %in% csave[,dim(csave)[2]-(j-1)]) {	
		colend <- dim(csave)[2]-(j-1) 
		c2 <- FALSE 
	    }
   }
   c1 <- c2 <- TRUE 
   for(j in 1:dim(csave)[1]) { 
	  if(c1) 
	      if("#0000FF" %in% csave[j,]) {	
	          rowstart <- j
		  c1 <- FALSE
	      }
	   if(c2)     
	      if("#0000FF" %in% csave[dim(csave)[1]-(j-1),] ) {	
	        rowend <- dim(csave)[1]-(j-1)
	        c2 <- FALSE
	      }
   }

   ## <tau> 
    if (dim(spec)[2] > 1) {
      
	xmat <- matrix(nrow = length(k), ncol = model@nl)
        vecres <- vector()
        for (j in length(k):1) xmat[j, ] <- sumAv[j, ] * (1/k[j])
        vecR <- colSums(xmat)
        resmat <- as.vector(tracemat)
	zmin <- min(vecR) - (.10* min(vecR))
	zmax <- max(vecR) 
        resmat[model@x2] <- vecR
        dim(resmat) <- dim(tracemat)
	 if(length( plotoptions@ylimspec ) == 2)
		  zlim <- plotoptions@ylimspec
        else	  zlim <- range(resmat[rowstart:rowend,colstart:colend]) 

        image.plot(resmat[rowstart:rowend,colstart:colend],
	xlab = "", axes = FALSE, ylab = "", 
        main = " < tau > ", zlim = zlim)
    }

    if (dim(spec)[2] > 1) {
        for (j in 1:length(k)) {
            resmat <- as.vector(tracemat)
            resmat[model@x2] <- sumAv[j, ]
            dim(resmat) <- dim(tracemat)
            image.plot(resmat[rowstart:rowend,colstart:colend], 
	    ylab = "", axes = FALSE, 
	    xlab = paste("tau=", signif(1/k[j], 5)), 
                main = paste("Comp.", j, "norm. amp."),  zlim=c(0,1))
        }
    }
    if (length(model@parmu) > 1) {
        plot(x2, t[[i]]@parmu, type = "l", main = "Shift parameter", 
            xlab = plotoptions@ylab, ylab = "")
    }
    
    residlist <- svdresidlist <- list()
    
        residuals <- matrix(nrow = m[[i]]@nt, ncol = m[[i]]@nl)
        for (j in 1:length(resultlist[[i]]@resid)) {
            residuals[, j] <- resultlist[[i]]@resid[[j]]
        }
        svdresidlist[[length(svdresidlist) + 1]] <- doSVD(residuals, 
            2, 2)
        residlist[[length(residlist) + 1]] <- residuals
    
	limd<- max(  max(residlist[[1]]), abs(min(residlist[[1]]))) 
	if(plotoptions@FLIMresidimag) {
	image.plot(x, x2, residlist[[1]], 
	xlab = plotoptions@xlab, ylab = plotoptions@ylab, 
            main = paste("Residuals Dataset",i), zlim=c(-limd,limd),
		col = diverge_hcl(40, h = c(0, 120), c = 60, l = c(45, 90), 
		power = 1.2))

        } 
    ## matplot function with "log" option is not compatible with 
    ## neg. x values; do the below to avoid warning
        xpos<- x
	xpos[which(x<=0)]<-NA
    if (nt > 1 && nl > 1) {
        matplot(xpos, svdresidlist[[1]]$left[,1], type = "l", 
	main = "Left sing. vec. residuals ", 
            ylab = "", log = "x", xlab = plotoptions@xlab, col = 1)
    }
    ## plot 1 right singular vector as image        
            resmat <- as.vector(tracemat)
            resmat[model@x2] <- as.vector(svdresidlist[[1]]$right[1,]) 
            dim(resmat) <- dim(tracemat)
            image.plot(resmat[rowstart:rowend,colstart:colend],
	    xlab = "", 
	    col = diverge_hcl(40, h = c(0, 120), c = 60, l = c(45, 90), 
	    power = 1.2),
	    axes = FALSE, ylab = "", 
            main = "Right sing. vec. residuals")
        
    
    svddatalist <- list()
    svddatalist[[length(svddatalist) + 1]] <- doSVD(multimodel@data[[i]]@psi.df, 
            2, 2)
    if (nt > 1 && nl > 1) {
        plot(1:length(svddatalist[[1]]$values), log10(svddatalist[[1]]$values), 
            ylab = "", xlab = "", main = "Sing. values data", 
            type = "b")
    }
    if (length(plotoptions@title) != 0) {
       if(length(m) > 1) tit <- paste(plotoptions@title, ", dataset ", i,sep="")
       else tit <- plotoptions@title
       if(plotoptions@addfilename) tit <- paste(tit, m[[i]]@datafile)
        mtext(tit, side = 3, outer = TRUE, line = 1)
    }
    if (dev.interactive() && length(plotoptions@makeps) != 0) {
      if(plotoptions@output == "pdf")
        pdev <- pdf 
      else  pdev <- postscript
        dev.print(device = pdev, file = paste(plotoptions@makeps,
	"dataset_", i, "_summary.", plotoptions@output, 
	sep = ""))
    }
  }

}
