"fitModel" <-
function (data, modspec = list(), datasetind = vector(), modeldiffs = list(), 
opt = opt()) 
{
    currModel <- getModel(data, modspec, modeldiffs, datasetind, opt)

    tr <- getTheta(currModel)
    theta <- tr$theta
    currModel <- tr$mod 
    
    currTheta <- getThetaCl(theta, currModel)

    iter <- opt@iter
    d <- vector()
    dummy <- as.data.frame(d)
    
    currModel@fit@nlsres$onls <- nls(~rescomp(t=t, d=d,
                                      currModel=currModel),
                                      data=dummy, control =
                                      nls.control(maxiter = iter,
                                      warnOnly = TRUE, printEval =
                                      TRUE), start = list(t = theta),
                                      trace = TRUE)

    currModel@fit@nlsres$sumonls <- summary(currModel@fit@nlsres$onls)

    currModel@finished <- TRUE

    resFinal <- rescomp(t=currModel@fit@nlsres$onls$m$getPars(),
                          currModel=currModel)

    currModel <- resFinal$currModel
    currTheta <- resFinal$currTheta
    
    if (opt@plot) 
        plotter(currModel@modellist[[1]], currModel, currTheta, opt)

    ret <- list( toPlotter = list(model = currModel@modellist[[1]],
                multimodel = currModel, multitheta = currTheta,
                plotoptions = opt), currModel = currModel, currTheta =
                currTheta)
    return(ret)
  }
