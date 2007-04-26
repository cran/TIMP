"getGroups" <-
function (m, modeldiffs) 
{
    thresh <- if (length(modeldiffs$thresh) == 0) 
        0
    else modeldiffs$thresh
    linkclp <- if (length(modeldiffs$linkclp) == 0) 
        rep(1, length(m))
    else modeldiffs$linkclp
    sl <- if (m[[1]]@mod_type != "spec") 
        "x2"
    else "x"
    allsl <- vector()
    datasetind <- vector()
    clplongind <- vector()
    for (i in 1:length(m)) {
        allsl <- append(allsl, slot(m[[i]], sl))
        datasetind <- append(datasetind, rep(i, length(slot(m[[i]], sl))))
        clplongind <- append(clplongind, 1:length(slot(m[[i]], sl)))
    }
    if (length(allsl) < 2) 
        return(list(groups = list(list(c(1, 1))), linkclp = linkclp))
    
    sl1 <- slot(m[[1]], sl)[1] 
    sl2 <- slot(m[[1]], sl)[length(slot(m[[1]], sl))]
    decr <- if(sl1 <= sl2) FALSE else TRUE
    sort_tmp <- sort(allsl, index.return = TRUE, decreasing = decr)
    sortclp <- sort_tmp$x
    sortindex <- sort_tmp$ix
    markclp <- sortclp[1]
    groups <- list(list(c(clplongind[sortindex[1]], datasetind[sortindex[1]])))
    refgroup <- linkclp[datasetind[sortindex[1]]]
    for (i in 2:length(sortclp)) {
        overlimit  <- abs(markclp - sortclp[i]) > thresh 
        if (refgroup != linkclp[datasetind[sortindex[i]]] || overlimit) {
            groups[[length(groups) + 1]] <- list(c(clplongind[sortindex[i]], 
                datasetind[sortindex[i]]))
            refgroup <- linkclp[datasetind[sortindex[i]]]
            markclp <- sortclp[i]
        }
        else groups[[length(groups)]][[length(groups[[length(groups)]]) + 
            1]] <- c(clplongind[sortindex[i]], datasetind[sortindex[i]])
    }
    list(groups = groups, linkclp = linkclp)
}
