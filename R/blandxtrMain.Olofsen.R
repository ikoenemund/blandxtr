# Modified Bland Altman-analysis on data from Olofsen et al. (2015).
# source (web): https://sec.lumc.nl/method_agreement_analysis/sim.txt

# library(profvis)
# profvis({

  # get data.table from RData
  source("R/loadRData.R")
  input_dt <- loadRData("data/dataOlofsen.RData")

  alpha <- 0.05
  beta <- 0.05

  source("R/blandxtrMain.R")
  biasMod <- TRUE
  bt <- 10

  olofsen_result <- blandxtrMain (input_dt, bt, biasMod, alpha, beta)
  biasMod <- olofsen_result$res$biasMod

# })
