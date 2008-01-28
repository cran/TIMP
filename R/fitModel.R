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
    assign(".currTheta", currTheta, envir = .GlobalEnv)
    assign(".currModel", currModel, envir = .GlobalEnv)
    
    
    if(opt@nls) {
      d <- vector()
      dummy <- as.data.frame(d)
      currModel@fit@nlsres$onls <- nls(~rescomp(t=t, d=d,
                                              currModel=currModel),
                                              data=dummy, control =
                                              nls.control(maxiter =
                                              iter, minFactor =
                                              opt@minFactor, warnOnly
                                              = TRUE, printEval =
                                              TRUE), start = list(t =
                                              theta),
                                              algorithm = opt@nlsalgorithm,
                                              trace = TRUE)
      currModel@fit@nlsres$sumonls <- summary(currModel@fit@nlsres$onls)
    }
    else {
      currModel@fit@nlsres$onls <- nls.lm(par=theta,
                                          fn = rescomp, 
                                          currModel=currModel,
                                          control=list(nprint=1))
    } 
    currModel@finished <- TRUE
    if(opt@nls)
      resFinal <- rescomp(t=currModel@fit@nlsres$onls$m$getPars(),
                        currModel=currModel)
    else 
       resFinal <- rescomp(t=currModel@fit@nlsres$onls$par,
                        currModel=currModel)
    
    currModel <- resFinal$currModel
    currTheta <- resFinal$currTheta

    assign(".currTheta", currTheta, envir = .GlobalEnv)
    assign(".currModel", currModel, envir = .GlobalEnv)
    
    if (opt@plot) 
        plotter(currModel@modellist[[1]], currModel, currTheta, opt)

    ret <- list( toPlotter = list(model = currModel@modellist[[1]],
                multimodel = currModel, multitheta = currTheta,
                plotoptions = opt), currModel = currModel, currTheta =
                currTheta)
    return(ret)
  }
