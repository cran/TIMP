"initModel" <-
function (...) 
{
    dots <- list(...)
    if ("mod_type" %in% names(dots)) {
        if (dots$mod_type == "kin") 
            model <- kin(...)
        if (dots$mod_type == "spec") 
            model <- spec(...)
	if (dots$mod_type == "mass") 
            model <- mass(...)
        if (dots$mod_type == "amp") 
            model <- amp(...)
    }
    if(model@mod_type == "spec")
	model@clpType <- "x"
    else model@clpType <- "x2"
    model@datCall <- append(model@datCall, match.call())   
    model <- initOneModel(model) 
    model
}
