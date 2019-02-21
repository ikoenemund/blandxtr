# main blandxtr function for data of Olofsen et al. 2015

#####
# TODO: Fallunterscheidungen automatisieren?!
#####

  # set working directory
  # setwd("~/blandxtr")

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
# data.frame to data.table
setDT(dt)
rm(path)

# calculate basic variables
source("R/basicVariables.R")
# basicVariables <- basicVariables(readData(path))

# calculate variances (tvv: standard version)
source("R/var_tvv.R")

# calculate variances (tvv: modified version)
source("R/var_tvv_mod.R")

# -----------------------------------------
# calculate limits of agreement (loa) (modified and standard)
source("R/loa.R")

#####
# TODO: case discrimination (standard or modified loa?)?
# both cases calculated at the moment
#####

# limits of agreement (based on standard tvv)
loa_l <- calc_loa_l(d, sd_d)
loa_u <- calc_loa_u (d, sd_d)

# limits of agreement (based on modified tvv)
loa_l_a <- calc_loa_l(d_a, sd_d_a)
loa_u_a <- calc_loa_u (d_a, sd_d_a)

# -----------------------------------------
# calculate variance of limits of agreement (loa)
source("R/var.loa.R")

#####
# case discrimination for Var(LoA) depending on bsv/ wsv
# meth = 0 for standard (small bsv)
# meth = 1 for modified (small wsv)
meth <- 0;
#####

var_loa <- calc_var_loa (n, n_obs, bsv, wsv, outputSubjects, var_var_d, meth)

# # variance of loa (for loa and loa_a)
# ev_var_d <- calc_ev_var_d (n, n_obs, bsv, wsv, outputSubjects)
# var_sd_d <- calc_var_sd_d (var_var_d, ev_var_d)
#
# if(meth == 0){
#   # standard variance of loa (standard: for small bsv)
#   var_d <- calc_var_d (n, n_obs, bsv, wsv, outputSubjects)
#   var_loa <- calc_var_loa (var_d, var_sd_d)
# } else {
# # modfied variance of loa (modified: for small wsv)
# var_d <- calc_var_d_a (n, bsv, wsv, outputSubjects)
# var_loa <- calc_var_loa (var_d_a, var_sd_d)
# }

# -----------------------------------------

# CI Bland Altman
# mod: uses modified versions of loa
source("R/ci.loa.ba.R")
loa_ba <- calc_ci_loa_l_ba (loa_l, loa_u, var_loa)
loa_ba_mod <- calc_ci_loa_l_ba (loa_l_a, loa_u_a, var_loa)

# -----------------------------------------

# CI mover
# mod: uses modified versions of loa
source("R/ci.loa.mover.R")
loa_mover <- ci_loa_mover (n, n_obs, outputSubjects, bsv, wsv, loa_l, loa_u)
loa_mover_mod <- ci_loa_mover (n, n_obs, outputSubjects, bsv, wsv, loa_l_a, loa_u_a)
