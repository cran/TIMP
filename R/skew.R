"skew" <- 
function(numax, deltanu, b, nu, nupower=1)
{
	arg <- 1 + (2 * b * (nu - numax))/deltanu
	res<-ifelse(arg>0, exp( - log(2) * (log(arg)/b)^2), 0)
	if(nupower!=1)
		res<- res * nu^nupower 

	res
}
