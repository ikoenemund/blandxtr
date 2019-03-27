# Modified Bland Altman-analysis on data from
# https://www-users.york.ac.uk/~mb55/datasets/datasets.htm

# ----------------------------------
# CardiacOutput
# source (web): https://www-users.york.ac.uk/~mb55/datasets/cardiac.dct

start_time <- Sys.time()

source("R/blandxtr.readData.R")
path <- "D:/EigeneDateien/Studium/MedizinischeInformatik/Bachelorarbeit/IMBS/BA_Bland_Altman/R_BA_BlandAltman/Daten_BA/CardiacOutput.csv"
input_dt <- blandxtr_readData(path)

source("R/blandxtrMain.R")
biasMod <- FALSE
bt <- 10

cardiacOutput_result <- blandxtrMain (input_dt, bt, biasMod)
biasMod <- cardiacOutput_result$res$biasMod

end_time <- Sys.time()
time_total <- end_time - start_time
time_total

# ----------------------------------
# Saturation
# source (web): https://www-users.york.ac.uk/~mb55/datasets/sealey.dct

start_time <- Sys.time()

source("R/blandxtr.readData.R")
path <- "D:/EigeneDateien/Studium/MedizinischeInformatik/Bachelorarbeit/IMBS/BA_Bland_Altman/R_BA_BlandAltman/Daten_BA/Saturation.csv"
input_dt <- blandxtr_readData(path)

source("R/blandxtrMain.R")
biasMod <- TRUE
bt <- 0

saturation_result <- blandxtrMain (input_dt, bt, biasMod)
biasMod <- saturation_result$res$biasMod

end_time <- Sys.time()
time_total <- end_time - start_time
time_total
