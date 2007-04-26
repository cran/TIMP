"writeFit" <- function (multimodel, multitheta, plotoptions) 
{
    m <- multimodel@modellist
    t <- multitheta
    res <- multimodel@fit@resultlist
    for (i in 1:length(m)) {
        x2 <- m[[i]]@x2
        x <- m[[i]]@x
        fitted <- matrix(nrow = m[[i]]@nt, ncol = m[[i]]@nl)
        irfmu <- vector()
        for (j in 1:length(x2)) {
            fitted[, j] <- res[[i]]@fitted[[j]]
            if (m[[i]]@weight) 
                fitted[, j] <- fitted[, j]/m[[i]]@weightM[, j]
        }
	
	write.table(fitted, file=paste(plotoptions$makeps, 
	"_fit_dataset_", i, ".txt", sep=""),  quote=FALSE,
	row.names = x, col.names = x2)
     }
}
