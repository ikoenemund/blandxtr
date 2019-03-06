#' @title Main method for blandxtr
#'
#' @description \code{blandxtrMain} performs modified Bland Altman-analysis as
#' proposed by Olofsen et al. (2015). Uses the following functions from
#' blandxtr-package: \code{basicVariables}, \code{var.tvv}, \code{loa},
#' \code{var.loa}, \code{ci.loa.ba} and \code{ci.loa.mover}.
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param path path where input dataset is stored (as csv)
#' @param biasMod set TRUE for modified calculation of bias (small wsv),
#' set FALSE for standard calculation of bias (small bsv)
#'
#' @note \code{biasMod} is automatically set TRUE for
#' different number of measurements in each subject (unbalanced case)
#' @note "_mod" labels results based on modified true value varies-method
#'
#' @return A list containing the return values of all used functions.

blandxtrMain <- function(path, biasMod){

  # -----------------------------------------
  # read data from given path
  dt <- data.table::fread(path)
  # data.frame to data.table
  data.table::setDT(dt)

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

  # -----------------------------------------
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

  # -----------------------------------------
  return(
    list(
      bv = bv,
      var_tvv = var_tvv,
      loa = loa,
      loa_mod = loa_mod,
      var_loa = var_loa,
      var_loa_mod = var_loa_mod,
      loa_ba = loa_ba,
      loa_ba_mod = loa_ba_mod,
      loa_mover = loa_mover,
      loa_mover_mod = loa_mover_mod
    )
  )
}
