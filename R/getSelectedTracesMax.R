"getSelectedTracesMax" <- function (multimodel, t, plotoptions) 
{
  specList <- getSpecList(multimodel, t)
  sp <- specList[[1]]
  ## this assumes the spectra are same for all datasets 
  
  selectedtraces <- c()
  sorted <- apply(sp, 2, sort, index.return = TRUE, decreasing = TRUE)
  cnt1 <- 1
  cnt <- plotoptions@nummaxtraces 
  col <- ncol(sp)
  while(cnt > 0) {
    for(i in 1:col) {
      if(cnt > 0  && (! (sorted[[i]]$ix[cnt1] %in% selectedtraces))){
        selectedtraces <- append(selectedtraces, sorted[[i]]$ix[cnt1])   	
        cnt <- cnt - 1	
      }
      cnt1 <- cnt1 + 1
    }
  }
  cat("The following traces will be plotted:\n", toString(selectedtraces), "\n")
  selectedtraces 
}
