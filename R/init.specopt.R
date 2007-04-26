"init.specopt" <-
function () 
{
    setClass("specopt", representation("opt",
			   nospectra = "logical",
			   selectedspectra = "vector"),
        prototype = list( nospectra = FALSE,
			   selectedspectra = vector(),
			   xlab = "wavelength",
			   ylab = "time"))
}

