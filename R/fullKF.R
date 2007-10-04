"fullKF" <-
function(theta, kinscal, kmat, jvec)
{
#fullKF
        dimk <- nrow(kmat)
	A <- matrix(nrow = dimk, ncol = dimk)
        K <- fillK(theta, kinscal, kmat)
	
        eigenlijk <- eigen(K, only.values = F)
        V <- eigenlijk$vectors
        gamma <- solve(V) %*% jvec
        for(j in 1:dimk) 
                A[j, ] <- V[ ,j] * gamma[j]
	list(A=A, values = eigenlijk$values)
}
