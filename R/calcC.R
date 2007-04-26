"calcC" <- 
function (theta, t) 
{

        conc <- matrix(nrow = length(t), ncol = length(theta))
        for(i in 1:length(theta)) {
                conc[, i] <- exp(-theta[i]*t)
        }

        conc

}
