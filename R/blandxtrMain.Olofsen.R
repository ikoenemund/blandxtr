# Modified Bland Altman-analysis on data from Olofsen et al. (2015).
# source (web): https://sec.lumc.nl/method_agreement_analysis/sim.txt

start_time <- Sys.time()

source("R/blandxtr.readData.R")
path <- "data/dataOlofsen.csv"
input_dt <- blandxtr_readData(path)

source("R/blandxtrMain.R")
biasMod <- TRUE
bt <- 10

olofsen_result <- blandxtrMain (input_dt, bt, biasMod)
biasMod <- olofsen_result$res$biasMod

end_time <- Sys.time()
time_total <- end_time - start_time
time_total
