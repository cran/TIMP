"normdat" <-
function (mat) 
{
    for (i in 1:ncol(mat)) mat[, i] <- mat[, i]/max(abs(mat[, i]))
    mat
}

