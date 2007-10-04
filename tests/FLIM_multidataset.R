#===============================================================
# Example provided by Sergey Laptenok
#===============================================================

require(TIMP)

mea_IRF <- scan("xtetoh_256_060822-bg_int_shift")[25:230]

cfp_data <- readData("c-001")
cfp_data_sel1 <- preProcess(cfp_data, sel_time=c(25,230))
cfp_data <- readData("c-002")
cfp_data_sel2 <- preProcess(cfp_data, sel_time=c(25,230))
cfp_data <- readData("c-003")
cfp_data_sel3 <- preProcess(cfp_data, sel_time=c(25,230))
cfp_data <- readData("c-004")
cfp_data_sel4 <- preProcess(cfp_data, sel_time=c(25,230))


#===============================================================
#Global Fit
#===============================================================

modelC<- initModel(mod_type = "kin",
kinpar=c(1.5431384, 0.3571556), convalg = 2, parmu = list(-0.013),
measured_irf = mea_IRF,
seqmod=FALSE, positivepar=c("kinpar"),
cohspec = list( type = "irf"),
title="Global CFP bi-exp model with pulse-follower")

fixRes<-fitModel(list(cfp_data_sel1, cfp_data_sel2, cfp_data_sel3,
                      cfp_data_sel4),
list(modelC),
modeldiffs = list(
linkclp=list(1,2,3,4),
free = list(
list(what = "parmu", ind = c(1,1), dataset = 2, start=0),
list(what = "parmu", ind = c(1,1), dataset = 3, start=-.01),
list(what = "parmu", ind = c(1,1), dataset = 4, start=-.01)
)),
opt=kinopt(iter=2, linrange = 10,
makeps = "global_CFP_pulsefol",
addfilename = TRUE,
notraces = FALSE,
selectedtraces = seq(1, length(cfp_data@x2), by=11),
summaryplotcol = 4, summaryplotrow = 3,
ylimspec = c(1, 2.6),
xlab = "time (ns)", ylab = "pixel number",
FLIM=TRUE))

 
