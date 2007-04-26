"writeEst" <- function (multimodel, multitheta, plotoptions, plotEstout) 
{
    onls <- multimodel@fit@nlsres$onls
    if (length(plotoptions@makeps) != 0) 
            filename <- paste(plotoptions@makeps, "_paramEst.txt", sep = "")
    else
	   filename <- ".currParamEst"	
     
    fileparam <- file(filename, "w")
    if(length(plotoptions@title) != 0)
	cat(plotoptions@title, "\n\n", file = fileparam)
    cat("Sum square error:", onls$m$deviance(), "\n\n", 
        file = fileparam)
    cat("Residual standard error:", summary(onls)$sigma, "\n\n", 
        file = fileparam)

    x<-plotEstout$x
    y<-plotEstout$y
    z<-plotEstout$z

    m <- multimodel@modellist
    if(length(m) > 1) 
	cat("Estimates for parameters associated with all datasets:\n\n", 
        file = fileparam)
    else 
	cat("Estimates for parameters:\n\n", file = fileparam)
    for(i in 1:length(x)) {
	  cat(x[[i]]$name, " ", x[[i]]$ind, ":  ", 
	  x[[i]]$pest,"\t(t value: ", x[[i]]$t, ")\n", sep="", 
	  file = fileparam)
    }
    cat("\n", file = fileparam)
    if(length(m) > 1 && (length(y) > 0 || length(z) > 0)) {
	cat("Estimates for parameters associated with per-dataset model differences:\n\n", file = fileparam)
	if(length(y) > 0) 
	  for(i in 1:length(y)) {
	       cat(y[[i]]$name, " ", y[[i]]$ind, ", dataset ", y[[i]]$dataset, ": ",
	       y[[i]]$pest,"\t(t value: ", 
	       y[[i]]$t, ")\n",  sep="", file = fileparam)
          }
	if(length(z) > 0) 
	 for(i in 1:length(z)) {
	       cat(z[[i]]$name, " ", z[[i]]$ind, ", dataset ", z[[i]]$dataset, ": ",
	       z[[i]]$pest,"\t(t value: ", 
	       z[[i]]$t, ") \n",  sep="", file = fileparam)
         }
    } 	
    close(fileparam)
}
