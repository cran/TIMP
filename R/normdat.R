"normdat" <-
function (mat) 
{
    for (i in 1:dim(mat)[2]) mat[, i] <- mat[, i]/max(abs(mat[, i]))
    mat
}

