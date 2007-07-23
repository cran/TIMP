"fitModel" <-
function (data, modspec = list(), datasetind = vector(), modeldiffs = list(), 
opt = opt()) 
{
    assign(".currModel", getModel(data, modspec, modeldiffs, datasetind, opt),
    envir = .GlobalEnv)

    theta <- getTheta(.currModel)

    assign(".currTheta", getThetaCl(theta, .currModel), envir = .GlobalEnv)

    iter <- opt@iter
    d <- vector()
    dummy <- as.data.frame(d)
    
    .currModel@fit@nlsres$onls <<- nls(~rescomp(t, d), data = dummy, 
    control = nls.control(maxiter = iter, warnOnly = TRUE, printEval = TRUE), 
    start = list(t = theta), trace = TRUE)

    .currModel@fit@nlsres$sumonls <<- summary(.currModel@fit@nlsres$onls)

    .currModel@finished <<- TRUE

    rescomp(.currModel@fit@nlsres$onls$m$getPars())

    if (opt@plot) 
        plotter(.currModel@modellist[[1]], .currModel, .currTheta, opt)

    return(list(toPlotter = list(model = .currModel@modellist[[1]], 
    multimodel = .currModel, multitheta = .currTheta, 
    plotoptions = opt)))
}