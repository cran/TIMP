"writeEst" <- function (multimodel, multitheta, plotoptions, plotEstout) 
{
  if(!plotoptions@algorithm=="optim" && !plotoptions@noplotest) {
    onls <- multimodel@fit@nlsres$onls
    if (length(plotoptions@makeps) != 0) 
            filename <- paste(plotoptions@makeps, "_paramEst.txt", sep = "")
    else
	   filename <- "currParamEst"	
    fileparam <- file(filename, "w")
    if(length(plotoptions@title) != 0)
	cat(plotoptions@title, "\n\n", file = fileparam)
    cat("Sum square error:", onls$m$deviance(), "\n\n", 
        file = fileparam)
    s <- summary(onls, multimodel)
    cat("Residual standard error:", s$sigma, "on", s$df[2], "degrees
    of freedom\n\n\n", file = fileparam)

    x<-plotEstout$x
    y<-plotEstout$y
    z<-plotEstout$z

    lall <- length(x) + length(y) + length(z)
    xmat <- matrix(nrow=lall+1, ncol=4, dimnames = list(rep("", lall+1), 
            c("PAR", "EST", "T VAL", "")))
    if(length(x) > 0)
        for(i in 1:length(x)) {
	     xmat[i,] <- c(paste( x[[i]]$name, "index:", x[[i]]$ind, 
	     "dataset:", x[[i]]$dataset), 
	     paste("est:", x[[i]]$pest), paste("tvalue:", 
	     x[[i]]$t), NA)
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
	     xmat[i+length(x)+1,] <- c(paste( y[[i]]$name, "index:", toString(y[[i]]$ind), 
	     "dataset:", y[[i]]$dataset), paste("est:", y[[i]]$pest), paste("tvalue:", 
	     y[[i]]$t), NA)

	      if(y[[i]]$name == "kinpar") 
		       xmat[i+length(x)+1, 4]<-paste("tau:",signif(1/y[[i]]$pest,digits=2))
       }
       if(length(z) > 0)
         for(i in 1:length(z)) {
	     xmat[i+length(x)+length(y)+1,] <- c(paste( z[[i]]$name, "index:", toString(z[[i]]$ind), 
	     "dataset:", z[[i]]$dataset), paste("est:", z[[i]]$pest), paste("tvalue:", 
	     z[[i]]$t), NA)
	     if(z[[i]]$name == "kinpar") 
		       xmat[i+length(x)+length(y)+1, 4]<-paste("tau:",signif(1/z[[i]]$pest,digits=2))
        }
      
      options(scipen=0)
      write.table(xmat[-nrow(xmat),], file=filename, quote=FALSE, append =
      TRUE, col.names = FALSE, na="")
    close(fileparam)
  }
}

