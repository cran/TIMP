"doSVD" <-
function (x, numleft, numright)
#doSVD
{
	svdx<-La.svd(as.matrix(x),nu=numleft,nv=numright)
	list(values=svdx$d, left=svdx$u, right=svdx$vt)

}

