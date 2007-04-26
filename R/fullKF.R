"fullKF" <-
function(theta, kinscal, kmat, jvec)
{
#fullKF

        A <- matrix(nrow = length(theta), ncol = length(theta))
        K <- fillK(theta, kinscal, kmat)

        eigenlijk <- eigen(K, only.values = F)
        V <- eigenlijk$vectors
        gamma <- solve(V) %*% jvec
        for(j in 1:length(theta)) 
                A[j, ] <- V[ ,j] * gamma[j]
	list(A=A, values = eigenlijk$values)
}
