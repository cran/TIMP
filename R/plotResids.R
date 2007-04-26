"plotResids" <- function (multimodel,  multitheta,  plotoptions)
{
	get(getOption("device"))()
	plotrow<-2
	plotcol<-2
	par(plotoptions@paropt)
	par(mfrow = c(plotrow, plotcol))
	par(mar=c(5.1,7.1,4.1,2.),cex.lab=1.6, cex.axis=1.6,cex.main=1.4, 
	mgp=c(4,1, 0))

	# RESIDUALS 
	# make a list of resid matrix
	m <- multimodel@modellist
	res <- multimodel@fit@resultlist
	model <- multimodel@modellist[[1]]
	increasing_x2 <- model@x2[2] > model@x2[1]
	residlist <- svdresidlist <- list() 
	for(i in 1:length(m)) {
		residuals <- matrix(nrow = m[[i]]@nt, 
				    ncol = m[[i]]@nl)
		for(j in 1:length(res[[i]]@resid)){ 
		    
		    if(m[[i]]@mod_type != "spec")
			 residuals[,j] <- res[[i]]@resid[[j]]
		    else
			residuals[j,] <- res[[i]]@resid[[j]]
		    
		}
		
		svdresidlist[[length(svdresidlist)+1]] <- doSVD(residuals,2,2) 
		residlist[[length(residlist)+1]] <- residuals 
	}
	## matplot function with "log" option is not compatible with 
	## neg. x values; do the below to avoid warning
        xpos <- model@x
	xpos[which(model@x<=0)]<-NA	
	if(increasing_x2) {
	   
	   limd<- max(  max(residlist[[1]]), abs(min(residlist[[1]]))) 
	   image(model@x, model@x2, residlist[[1]], xlab = plotoptions@xlab, ylab = plotoptions@ylab,
		main = "Residuals Dataset 1", zlim=c(-limd,limd),
		col = diverge_hcl(40, h = c(0, 120), c = 60, l = c(45, 90), power = 1.2))
	}
	if(model@nt > 1 && model@nl > 1){ 
	      matplot(xpos, as.matrix(svdresidlist[[1]]$left[,i]), type = "l",
	      main = "1st left sing. vec. residuals ", log = "x", xlab =
		plotoptions@xlab, col = 1, ylab="")
	      if(length(m) > 1){
		 for(i in 2:length(m)) {
			 matlines(m[[i]]@x, 
			 as.matrix(svdresidlist[[i]]$left[,i]), log="x", 
			 type ="l", col = i)
                 }

              }
	      matplot(model@x2, as.matrix(svdresidlist[[1]]$right[i,]), 
	      type = "l",  
	      main = "1st right sing. vec. residuals ", xlab = plotoptions@ylab, 
	      col = 1, ylab="")
	      if(length(m) > 1){
		 for(i in 2:length(m)) {
			 matlines(m[[i]]@x2, 
			 as.matrix(svdresidlist[[i]]$right[i,]), type ="l", col = i)
                 }
	      }
	      plot(1:length(svdresidlist[[1]]$values),
	      log10(svdresidlist[[1]]$values), 
              main = "Sing. values residuals", type= "b",xlab="",ylab="")
	      if(length(m) > 1){
		 for(i in 2:length(m)) {
			 lines(1:length(svdresidlist[[i]]$values),
			 log10(svdresidlist[[i]]$values), xlab="",ylab="", 
			 type = "b", col = i)
                 }
	      }
      }
       # MAKE PS
       if(dev.interactive() && length(plotoptions@makeps) != 0) {
		dev.print(device=postscript, 
		file=paste(plotoptions@makeps, "_resids.ps", sep=""))
       }

}
