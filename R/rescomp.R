"rescomp" <-
function (theta, d = vector(), currModel) 
{
	currTheta <- getThetaCl(theta, currModel)
	groups <- currModel@groups 
	m <- currModel@modellist
	resid <- clpindepX <-list() 
	for(i in 1:length(m)) 
	      clpindepX[[i]] <- if(!m[[i]]@clpdep || m[[i]]@getX) 
		               getClpindepX(model = m[[i]], theta =
				currTheta[[i]], multimodel = currModel,
				returnX = FALSE, rawtheta= theta, dind=0)
				 else 
				   matrix() 
	for(i in 1:length(groups)) {
	      resid[[i]] <-  residPart(model = m[[1]], 
	      group = groups[[i]], multimodel = currModel, 
	      thetalist = currTheta, clpindepX = clpindepX, 
	      finished = currModel@finished, returnX = FALSE,
	      rawtheta = theta) 
	      if(currModel@finished){
		currModel <- fillResult(group = groups[[i]],
                multimodel = currModel, thetalist = currTheta,
                clpindepX = clpindepX, rlist = resid[[i]], rawtheta =
                theta)
              
              }
            }
        if(currModel@finished) return(list(currModel=currModel,
                                           currTheta=currTheta))
	unlist(resid)

}

