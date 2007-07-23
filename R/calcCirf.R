"calcCirf" <-
function (k, x, irfpar, mirf = FALSE, measured_irf = vector(), 
    convalg = 1, shiftmea = vector(), lamb = 1, reftau = 0) 
{
    if (!mirf) {
        mu <- irfpar[1]
        tau <- irfpar[2]
        m <- rep(0, length(x) * length(k))
        m <- as.matrix(.C("calcCirf", m = as.double(m), as.double(k), 
            as.double(x), as.double(tau), as.double(mu), as.integer(length(k)), 
            as.integer(length(x)), PACKAGE="TIMP")$m)
        dim(m) <- c(length(x), length(k))
    }
    else {
        m <- matrix(0, nrow = length(x), ncol = length(k))
        xspace <- x[2] - x[1]
	if (length(shiftmea) != 0) {
            if (length(shiftmea) == 1) 
                lamb <- 1
            measured_irf <- .C("ShiftCurve", source = as.double(measured_irf), 
                as.double(measured_irf), as.double(shiftmea[lamb]/xspace), 
                as.integer(length(x)), PACKAGE="TIMP")$source
        }
        for (i in 1:length(k)) {
            m[, i] <- switch(convalg, .C("Conv1", result = as.double(m[, 
                i]), as.double(measured_irf), as.integer(length(x)), 
                as.double(k[i]), as.double(xspace), 
		PACKAGE="TIMP")$result, 
                .C("Conv2", result = as.double(m[, i]), as.double(measured_irf), 
                  as.integer(length(x)), as.double(k[i]), as.double(xspace), 
		  PACKAGE="TIMP")$result, 
		  .C("Conv3", result = as.double(m[, 
                  i]), as.double(measured_irf), as.integer(length(x)), 
                  as.double(k[i]), as.double(xspace),  
		  PACKAGE="TIMP")$result, 
                .C("Conv4", result = as.double(m[, i]), as.double(measured_irf), 
                  as.integer(length(x)), as.double(k[i]), as.double(xspace), 
                  PACKAGE="TIMP")$result,
		.C("Conv5", result = as.double(m[, i]), as.double(measured_irf), 
                  as.integer(length(x)), as.double(k[i]), as.double(xspace), 
                  PACKAGE="TIMP")$result, 
		.C("Conv6", result = as.double(m[, i]), as.double(measured_irf), 
                  as.integer(length(x)), as.double(k[i]), as.double(xspace),
		  as.double(reftau), PACKAGE="TIMP")$result, 
		  .C("Conv7", result = as.double(m[, i]), as.double(measured_irf), 
                  as.integer(length(x)), as.double(k[i]), as.double(xspace),
		  as.double(reftau), PACKAGE="TIMP")$result,
		  .C("Conv8", result = as.double(m[, i]), as.double(measured_irf), 
                  as.integer(length(x)), as.double(k[i]), as.double(xspace),
		  as.double(reftau), PACKAGE="TIMP")$result,
		    .C("Conv9", result = as.double(m[, i]), as.double(measured_irf), 
                  as.integer(length(x)), as.double(k[i]), as.double(xspace),
		  as.double(reftau), PACKAGE="TIMP")$result,
		  .C("Conv10", result = as.double(m[, i]), as.double(measured_irf), 
                  as.integer(length(x)), as.double(k[i]), as.double(xspace),
		  as.double(reftau), PACKAGE="TIMP")$result,
		   .C("Conv11", result = as.double(m[, i]), as.double(measured_irf), 
                  as.integer(length(x)), as.double(k[i]), as.double(xspace),
		  as.double(reftau), PACKAGE="TIMP")$result)
        }
    }
    m
}
