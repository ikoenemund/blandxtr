#' @title Confidence intervals for LoA (parametric bootsrap-t)
#'
#' @description \code{calc_ci_loa_bt} returns confidence intervals
#' (CI) for limits of agreement (LoA). Calculation is based on
#' parametric bootstrap-t.
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param bt number of bootstrap samples
#' @param input_dt data.table with input dataset
#' @param biasMod set TRUE for modified calculation of bias (small wsv),
#' set FALSE for standard calculation of bias (small bsv)
#' @param loa_l lower limit of agreement
#' @param loa_u upper limit of agreement
#' @param var_loa variance of limits of agreement
#' @param alpha for 100*(1-alpha)%-confidence interval around LoA
#' @param beta for 100*(1-beta)%-confidence interval around bias
#'
#' @note \code{biasMod} is automatically set TRUE for
#' different number of measurements in each subject (unbalanced case)
#' @note "_mod" labels results based on modified true value varies-method
#'
#' @return A list with the following elements is returned
#' \itemize{
#'  \item{\code{ci_l_loa_l_bt}} {lower limit of 95\%-CI for lower LoA}
#'  \item{\code{ci_u_loa_l_bt}} {upper limit of 95\%-CI for lower LoA}
#'  \item{\code{ci_l_loa_u_bt}} {lower limit of 95\%-CI for upper LoA}
#'  \item{\code{ci_u_loa_u_bt}} {upper limit of 95\%-CI for upper LoA}
#' }

calc_ci_loa_bt <- function(bt, input_dt, biasMod, loa_l, loa_u, var_loa,
  alpha, beta) {
  source("R/blandxtrMain.pre.R")

  #  sampling
  boot_samp <- vector("list", bt)
  i <- 1:bt
  sample_dt <- function(i, input_dt){
    boot_dt <- input_dt[sample(nrow(input_dt), nrow(input_dt), replace = TRUE), ]
    boot_samp[[i]] <- boot_dt
  }
  boot_samp <- lapply(i, sample_dt, input_dt=input_dt)
  rm(i)

  # bland altman analysis per sample
  boot <- lapply(boot_samp, blandxtrMain_pre, bt=bt, biasMod=biasMod,
    beta)

  # initialize and fill matrix
  s <- matrix(NA, nrow = bt, ncol = 5)
  colnames(s) = (c("lower LoA", "upper LoA", "SD LoA", "z_l", "z_u"))

  for(r in 1:bt){
    s[r,1] <- boot[[r]]$loa$loa_l
    s[r,2] <- boot[[r]]$loa$loa_u
    s[r,3] <- sqrt(boot[[r]]$var_loa)
    s[r,4] <- (boot[[r]]$loa$loa_l-loa_l)/var_loa
    s[r,5] <- (boot[[r]]$loa$loa_u-loa_u)/var_loa
  }
  rm(r)

  ci_l_loa_l_bt <- unname(loa_l-(sqrt(var_loa)*quantile(s[,4],1-alpha/2)))
  ci_u_loa_l_bt <- unname(loa_l-(sqrt(var_loa)*quantile(s[,4],alpha/2)))
  ci_l_loa_u_bt <- unname(loa_u-(sqrt(var_loa)*quantile(s[,5],1-alpha/2)))
  ci_u_loa_u_bt <- unname(loa_u-(sqrt(var_loa)*quantile(s[,5],alpha/2)))
  rm(s)

  return(
    list(
      ci_l_loa_l_bt = ci_l_loa_l_bt,
      ci_u_loa_l_bt = ci_u_loa_l_bt,
      ci_l_loa_u_bt = ci_l_loa_u_bt,
      ci_u_loa_u_bt = ci_u_loa_u_bt
    )
  )
}
