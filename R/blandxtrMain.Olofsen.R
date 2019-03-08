# Modified Bland Altman-analysis on data from Olofsen et al. (2015).
# source (web): https://sec.lumc.nl/method_agreement_analysis/sim.txt


# ### TO DO: ideas for getting right wd automatically
# # idea: opts_knit$set(root.dir = '../..')
# setwd("C:/Users/IK/Documents/blandxtr")
# ###

source("R/blandxtrMain.R")
path <- "data/dataOlofsen.csv"
biasMod <- FALSE
bt <- 1

olofsen_result <- blandxtrMain (bt, path, biasMod)
