"getClpConstr" <-
function (clp, clp0, clpequspec, dataset = 0) {
    if(length(clp) > 1){ 
		  increasing_clp <- clp[1] < clp[length(clp)] 
		  single_clp <- FALSE
    }
    else single_clp <- TRUE
    clp0mat <- matrix(0, length(clp), length(clp0))
    clpRem <- matrix(0, length(clp), length(clpequspec))
    clpMod <- matrix(0, length(clp), length(clpequspec))
    clpDataset <- matrix(0, length(clp), length(clpequspec))
    if(!single_clp) {
      if (length(clp0) != 0) 
        for (i in 1:length(clp0)) {
            to0 <- if (increasing_clp) 
                intersect(which(clp >= clp0[[i]]$low), which(clp <= 
                  clp0[[i]]$high))
            else intersect(which(clp <= clp0[[i]]$low), which(clp >= 
                clp0[[i]]$high))
            clp0mat[to0, i] <- clp0[[i]]$comp
        }
    if (length(clpequspec) != 0) 
        for (i in 1:length(clpequspec)) {
            clptoequ <- if (increasing_clp) 
                intersect(which(clp >= clpequspec[[i]]$low), 
                  which(clp <= clpequspec[[i]]$high))
            else intersect(which(clp <= clpequspec[[i]]$low), 
                which(clp >= clpequspec[[i]]$high))
            clpRem[clptoequ, i] <- clpequspec[[i]]$to
            clpMod[clptoequ, i] <- clpequspec[[i]]$from
            clpDataset[clptoequ, i] <- if (length(clpequspec[[i]]$dataset) == 
                0) 
                dataset
            else clpequspec[[i]]$dataset
        }
    }
    list(clp0mat = clp0mat, clpRem = clpRem, clpMod = clpMod, 
        clpDataset = clpDataset)
}
