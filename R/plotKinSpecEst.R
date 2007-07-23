"plotKinSpecEst" <-
function(t, plotoptions, multimodel)
{
	get(getOption("device"))()
	resultlist <- multimodel@fit@resultlist
	sumonls <-  multimodel@fit@nlsres$sumonls
	par(mfrow=c(1,2), mar=c(5,2,2,2))
	plotKinSpec(multimodel, t, plotoptions, newplot=FALSE, 
	kinspecerr = plotoptions@kinspecerr)
	plotEst(multimodel, plotoptions) 
	if (length(plotoptions@title) != 0) {
            mtext(plotoptions@title, side = 3, outer = TRUE, 
                line = 1)
            par(las = 2)
        }
	if (dev.interactive() && length(plotoptions@makeps) != 0) {
	   if(plotoptions@output == "pdf")
				 pdev <- pdf 
	   else  pdev <- postscript

            dev.print(device = pdev, file = paste(plotoptions@makeps, 
                "_kinspecrates.",plotoptions@output, sep = ""))
        }

}

