# main blandxtr function for data from Olofsen et al. 2015

#####
# Fallunterscheidungen
# Var_loa/ B <--> B_a: biasMod = FALSE --> B; biasMod = TRUE --> B_a
# std. + mod. Berechnung von LoA (MSSI/ MSSIa) durchgehend berücksichtigt
#####

#####
# Choose biasMod
####
# biasMod = FALSE for standard (small bsv/ B)
# biasMod = TRUE for modified (small wsv/ B_a)
# automatically for unbalanced case: biasMod = TRUE (see below)
biasMod <- FALSE;
#####

# # function for reading csv-data from given path
# readData <- function (path){
#   # read data (csv)
#   library(data.table)
#   dt <- fread(path)
#   # data.frame to data.table
#   setDT(dt)
#   return(dt)
# }

# -----------------------------------------

# read Olofsen data (csv) from package (data)
path <- "data/dataOlofsen.csv"
# readData(path)
library(data.table)
dt <- fread(path)
setDT(dt)
rm(path)

# -----------------------------------------

# calculate basic variables
source("R/basicVariables.R")
bv <- basicVariables(dt)


# unbalanced case: biasMod = TRUE
for (i in 1:bv$n){
  if (bv$outputSubjects$m_i[i]!=(bv$n_obs/bv$n)){
    biasMod <- TRUE
  }
}
rm(i)

if (biasMod){
  bv$d <- bv$d_a
}

# TEST: with function "readData"
# bv <- basicVariables(readData(path))

# # calculate variances (tvv: standard version)
# source("R/var_tvv.R")
# var_tvv_std <- calc_var_tvv_std(bv$n, bv$n_obs, bv$d, bv$outputSubjects,
# bv$outputMeasurements)
#
# # calculate variances (tvv: modified version)
# source("R/var_tvv_mod.R")
# var_tvv_mod <- calc_var_tvv_mod(bv$n, bv$n_obs, bv$d_a, bv$outputSubjects,
# bv$outputMeasurements)

# analysis of variances
source("R/var.tvv.R")
var_tvv <- calc_var_tvv(bv$n, bv$n_obs, bv$d, bv$d_a, bv$outputSubjects,
  bv$outputMeasurements)

# -----------------------------------------
# calculate limits of agreement (loa) (standard and modified)
source("R/loa.R")

# limits of agreement (based on standard tvv)
loa <- calc_loa(bv$d, var_tvv$sd_d)

# limits of agreement (based on modified tvv)
loa_mod <- calc_loa(bv$d_a, var_tvv$sd_d_mod)

# -----------------------------------------
# calculate variance of limits of agreement (loa)
source("R/var.loa.R")

# variance of loa (based on standard tvv)
var_loa <- calc_var_loa (bv$n, bv$n_obs, var_tvv$bsv, var_tvv$wsv,
  bv$outputSubjects, var_tvv$var_var_d, biasMod)

# variance of loa (based on modified tvv)
var_loa_mod <- calc_var_loa (bv$n, bv$n_obs, var_tvv$bsv_mod, var_tvv$wsv_mod,
  bv$outputSubjects, var_tvv$var_var_d_mod, biasMod)

# -----------------------------------------

# CI Bland Altman
source("R/ci.loa.ba.R")
# CI Bland Altman (based on standard tvv)
loa_ba <- calc_ci_loa_ba (loa$loa_l, loa$loa_u, var_loa)

# CI Bland Altman (based on modified tvv)
# mod: uses modified versions of loa (modified tvv)
loa_ba_mod <- calc_ci_loa_ba (loa_mod$loa_l, loa_mod$loa_u, var_loa_mod)

# -----------------------------------------

# CI mover
# mod: uses modified versions of loa (modified tvv)
source("R/ci.loa.mover.R")

# CI mover (based on standard tvv)
loa_mover <- calc_ci_loa_mover (bv$n, bv$n_obs, bv$outputSubjects,
  var_tvv$bsv_mod, var_tvv$wsv, loa$loa_l, loa$loa_u)

# CI mover (based on modified tvv)
loa_mover_mod <- calc_ci_loa_mover (bv$n, bv$n_obs, bv$outputSubjects,
  var_tvv$bsv_mod, var_tvv$wsv_mod, loa_mod$loa_l, loa_mod$loa_u)
