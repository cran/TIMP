"plotKinSpecEst" <-
function(t, plotoptions, multimodel)
{
	get(getOption("device"))()
	resultlist <- multimodel@fit@resultlist
	sumonls <-  multimodel@fit@nlsres$sumonls
	m <- multimodel@modellist 
	par(mfrow=c(1,2))
	par(oma = c(0,0,4,0))
	plotKinSpec(multimodel, t, plotoptions, newplot=FALSE, 
	kinspecerr = plotoptions@kinspecerr)
	plotEst(multimodel, plotoptions) 
	if (length(plotoptions@title) != 0) {
            mtext(plotoptions@title, side = 3, outer = TRUE, 
                line = 1)
            par(las = 2)
        }
	if (dev.interactive() && length(plotoptions@makeps) != 0) {
            dev.print(device = postscript, file = paste(plotoptions@makeps, 
                "_kinspecrates.ps", sep = ""))
        }

}

