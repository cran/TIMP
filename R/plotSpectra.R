"plotSpectra" <-
function (model, multimodel, multitheta, plotoptions) 
{
    get(getOption("device"))()
    m <- multimodel@modellist
    t <- multitheta
    res <- multimodel@fit@resultlist
    for (i in 1:length(m)) {
        x2 <- m[[i]]@x2
        x <- m[[i]]@x
	xtoplot <- if(length(plotoptions@selectedspectra) > 0)
	       plotoptions@selectedspectra
	       else x2
        fitted <- matrix(nrow = m[[i]]@nt, ncol = m[[i]]@nl)
        irfmu <- vector()
        for (j in 1:length(x)) {
            fitted[j, ] <- res[[i]]@fitted[[j]]
            if (m[[i]]@weight) 
                fitted[j, ] <- fitted[j, ]/m[[i]]@weightM[j, ]
        }
    par(plotoptions@paropt)
    par(mfrow = n2mfrow(length(xtoplot)))
        for (j in 1:length(x)) {
            if (j %in% xtoplot) {
                plot(x2, m[[i]]@psi.df[j, ],  
                  type = "l", xlab = plotoptions@ylab, 
		  ylab = signif(x[j], 3), 
                  col = i)
                lines(x2, fitted[j, ], col = i, lty = 2, type = "l")
            }
        }
        if (length(plotoptions@title) != 0) {
            mtext(plotoptions@title, side = 3, outer = TRUE, 
                line = 1)
           }
        else mtext(paste("Spectra for dataset", i), side = 3, 
            outer = TRUE, line = 1)
        if (dev.interactive() && length(plotoptions@makeps) != 0) {
            dev.print(device = postscript, file = paste(plotoptions@makeps, 
                "_", i, "_spectra.ps", sep = ""))
        }
    }
}
