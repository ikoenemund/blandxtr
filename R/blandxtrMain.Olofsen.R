# Modified Bland Altman-analysis on data from Olofsen et al. (2015).
# source (web): https://sec.lumc.nl/method_agreement_analysis/sim.txt

library(profvis)
profvis({

  source("R/blandxtr.readData.R")
  path <- "data/dataOlofsen.csv"
  input_dt <- blandxtr_readData(path)

  source("R/blandxtrMain.R")
  biasMod <- TRUE
  bt <- 10

  olofsen_result <- blandxtrMain (input_dt, bt, biasMod)
  biasMod <- olofsen_result$res$biasMod

})
