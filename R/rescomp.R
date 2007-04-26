"rescomp" <-
function (theta, d=vector()) 
{
	assign(".currTheta", getThetaCl(theta, .currModel), envir = .GlobalEnv)
	groups <- .currModel@modeldiffs$groups 
	m <- .currModel@modellist
	resid <- clpindepX <-list() 
	for(i in 1:length(m)) 
	      clpindepX[[i]] <- if(!m[[i]]@clpdep) 
		               getClpindepX(model = m[[i]], theta =
				.currTheta[[i]], multimodel = .currModel,
				returnX = FALSE, rawtheta= theta, dind=0)
				 else 
				   matrix() 
	for(i in 1:length(groups)) {
	      resid[[i]] <-  residPart(model = m[[1]], 
	      group = groups[[i]], multimodel = .currModel, 
	      thetalist = .currTheta, clpindepX = clpindepX, 
	      finished = .currModel@finished, returnX = FALSE,
	      rawtheta = theta) 
	      if(.currModel@finished)
		fillResult(group = groups[[i]], multimodel = .currModel, 
	        thetalist = .currTheta, clpindepX = clpindepX, 
		rlist = resid[[i]], rawtheta = theta)

	}      
	unlist(resid)

}

