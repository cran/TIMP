"sample_sel" <-
function (data, sample = c(), sample_time = c(), sample_lambda = c(), 
    sel_time = c(), sel_lambda = c(), sel_time_ab = c(), 
    sel_lambda_ab = c()) 
{
    if (length(sel_time) == 2) {
        if (sel_time[2] > data@nt) 
            sel_time[2] <- data@nt
        data@psi.df <- data@psi.df[sel_time[1]:sel_time[2], ]
        data@x <- data@x[sel_time[1]:sel_time[2]]
        if (data@simdata) 
            data@C2 <- data@C2[sel_time[1]:sel_time[2], ]
    }
    if (length(sel_time_ab) == 2) {
        tmin <- which(data@x >= sel_time_ab[1])[1]
	tmax <- which(data@x <= sel_time_ab[2])[length(
                            which(data@x <= sel_time_ab[2]))]
        data@psi.df <- data@psi.df[tmin:tmax, ]
        data@x <- data@x[tmin:tmax]
        if (data@simdata) 
            data@C2 <- data@C2[tmin:tmax, ]
    }
    if (length(sel_lambda) == 2) {
        lmin <- sel_lambda[1]
        lmax <- sel_lambda[2]
        data@psi.df <- if (is.matrix(data@psi.df)) 
            data@psi.df[, lmin:lmax]
        else data@psi.df[lmin:lmax]
        data@psi.df <- as.matrix(data@psi.df)
        data@nl <- if (is.matrix(data@psi.df)) 
            ncol(data@psi.df)
        else as.integer(1)
        data@x2 <- data@x2[lmin:lmax]
        if (data@simdata) 
            data@E2 <- data@E2[lmin:lmax, ]
    }
    if (length(sel_lambda_ab) == 2) {
        increasing_x2 <- if(data@x2[1] < data@x2[2]) TRUE else FALSE 
	if(increasing_x2) {
		lmin <- which(data@x2 >= sel_lambda_ab[1])[1]
		lmax <- which(data@x2 <= sel_lambda_ab[2])[length(
                            which(data@x2 <= sel_lambda_ab[2]))]
        }
	else {
	     lmin <- which(data@x2 <= sel_lambda_ab[1])[1]
	     lmax <- which(data@x2 >= sel_lambda_ab[2])[length(
                            which(data@x2 >= sel_lambda_ab[2]))]
	}
	data@psi.df <- data@psi.df[,lmin:lmax]
        data@nt <- ifelse(is.matrix(data@psi.df), nrow(data@psi.df), 
            as.integer(1))
        data@x2 <- data@x2[lmin:lmax]
        if (data@simdata) 
            data@E2 <- data@E2[,lmin:lmax ]
	
    }
    data@nl <- length(data@x2)
    data@nt <- length(data@x)
    if (sample != 1) {
	data@psi.df <- data@psi.df[seq(1, data@nt, by = sample), 
            seq(1, data@nl, by = sample)]
	data@x <- data@x[seq(1, data@nt, by = sample)]
	data@x2 <- data@x2[seq(1, data@nl, by = sample)]
	data@nl <- length(data@x2)
	data@nt <- length(data@x)
    }
    else {
        if (data@nt == 1) 
            data@psi.df <- data@psi.df[seq(1, data@nl, by = sample_lambda)]
        if (data@nl == 1) 
            data@psi.df <- data@psi.df[seq(1, data@nt, by = sample_time)]
        if (is.matrix(data@psi.df)) 
            data@psi.df <- data@psi.df[seq(1, data@nt, by = sample_time), 
                seq(1, data@nl, by = sample_lambda)]
        data@x <- data@x[seq(1, data@nt, by = sample_time)]
        data@x2 <- data@x2[seq(1, data@nl, by = sample_lambda)]
	data@nl <- length(data@x2)
	data@nt <- length(data@x)
    }
    data
}
