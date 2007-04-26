"weightNL" <-
function (temp, model, n) 
{
    if (model@mod_type == "kin") 
        temp <- temp * model@weightM[, n]
    if (model@mod_type == "spec") 
        temp <- temp * model@weightM[n, ]
    temp
}
