#' @title Helper function for main method for blandxtr
#'
#' @description \code{blandxtrMain_pre} performs modified Bland Altman-analysis
#' as proposed by Olofsen et al. (2015) without calculation of
#' confidence intervals. Helper function for \code{blandxtr.Main} which performs
#' the whole analysis.
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param bt number of bootstrap samples
#' @param input_dt data.table with input dataset
#' @param biasMod set TRUE for modified calculation of bias (small wsv),
#' set FALSE for standard calculation of bias (small bsv)
#'
#' @note \code{biasMod} is automatically set TRUE for
#' different number of measurements in each subject (unbalanced case)
#' @note "_mod" labels results based on modified true value varies-method
#'
#' @return A list containing the return values of all used functions.
#'

blandxtrMain_pre <- function (input_dt, bt, biasMod) {
  # -----------------------------------------
  start_time <- Sys.time()
  # calculate basic variables
  source("R/basicVariables.R")
  bv <- basicVariables(input_dt)

  end_time <- Sys.time()
  time_bv <- end_time - start_time

  # unbalanced case: biasMod = TRUE
  for (i in 1:bv$n){
    if (bv$outputSubjects$m_i[i]!=(bv$n_obs/bv$n)){
      biasMod <- TRUE
      break
    }
  }
  rm(i)

  if (biasMod){
    bv$d <- bv$d_a
  }

  # -----------------------------------------
  start_time <- Sys.time()
  # analysis of variances
  source("R/var.tvv.R")
  var_tvv <- calc_var_tvv(bv$n, bv$n_obs, bv$d, bv$d_a, bv$outputSubjects,
    bv$outputMeasurements)
  end_time <- Sys.time()
  time_var <- end_time - start_time

  # -----------------------------------------
  start_time <- Sys.time()
  # calculate limits of agreement (loa) (standard and modified)
  source("R/loa.R")

  # limits of agreement (based on standard tvv)
  loa <- calc_loa(bv$d, var_tvv$sd_d)

  # limits of agreement (based on modified tvv)
  loa_mod <- calc_loa(bv$d_a, var_tvv$sd_d_mod)
  end_time <- Sys.time()
  time_loa <- end_time - start_time

  # -----------------------------------------
  start_time <- Sys.time()
  # calculate variance of limits of agreement (loa)
  source("R/var.loa.R")

  # variance of loa (based on standard tvv)
  var_loa <- calc_var_loa (bv$n, bv$n_obs, var_tvv$bsv, var_tvv$wsv,
    bv$outputSubjects, var_tvv$var_var_d, biasMod)

  # variance of loa (based on modified tvv)
  var_loa_mod <- calc_var_loa (bv$n, bv$n_obs, var_tvv$bsv_mod, var_tvv$wsv_mod,
    bv$outputSubjects, var_tvv$var_var_d_mod, biasMod)
  end_time <- Sys.time()
  time_varloa <- end_time - start_time
  # -----------------------------------------
  return(
    list(
      bv = bv,
      var_tvv = var_tvv,
      loa = loa,
      loa_mod = loa_mod,
      var_loa = var_loa,
      var_loa_mod = var_loa_mod,
      time_bv = time_bv,
      time_var = time_var,
      time_loa = time_loa,
      time_varloa = time_varloa,
      biasMod = biasMod
    )
  )
}
