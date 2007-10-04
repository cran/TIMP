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
    }
    if(model@mod_type == "kin" || model@mod_type == "mass")
	model@clpType <- "x2"
    else model@clpType <- "x"
    model@datCall <- append(model@datCall, match.call())   
    model <- initOneModel(model) 
    model
}
