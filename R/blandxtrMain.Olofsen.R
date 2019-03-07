# Modified Bland Altman-analysis on data from Olofsen et al. (2015).
# source (web): https://sec.lumc.nl/method_agreement_analysis/sim.txt

path <- "data/dataOlofsen.csv"
biasMod <- FALSE
source("R/blandxtr.readData.R")
olofsen_dt <- blandxtr_readData(path)

source("R/blandxtrMain.R")
bt <- 10
olofsen_result <- blandxtrMain (bt, olofsen_dt, biasMod)
