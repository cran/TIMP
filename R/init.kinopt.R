"init.kinopt" <-
function () 
{
    setClass("kinopt", representation("opt",
			   notraces = "logical",
			   selectedtraces = "vector",
			   breakdown = "list",
			   FLIM = "logical", 
			   FLIMresidimag = "logical",
			   noFLIMsummary = "logical",
			   writerawcon = "logical",
			   plotcohcolspec = "logical", 
			   kinspecest = "logical",
			   writeplaincon = "list"),
        prototype = list( notraces = FALSE,
			   selectedtraces = vector(),
			   breakdown = list(),
			   writerawcon = FALSE,
			   plotcohcolspec = TRUE,
			   FLIM = FALSE,	
			   FLIMresidimag = TRUE, 
			   noFLIMsummary = FALSE,
			   xlab = "time",
			   ylab = "wavelength",
			   kinspecest = FALSE,
			   writeplaincon = list()))
}

