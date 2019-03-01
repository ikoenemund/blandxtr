# Modified Bland Altman-analysis on data from Olofsen et al. (2015).
# source (web): https://sec.lumc.nl/method_agreement_analysis/sim.txt

library(data.table)

path <- "data/dataOlofsen.csv"
biasMod <- FALSE
source("R/blandxtrMain.R")
olofsen <- blandxtrMain (path, biasMod)
