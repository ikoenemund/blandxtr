#' @title 95\%-confidence intervals for LoA (parametric bootsrap-t)
#'
#' @description \code{calc_ci_loa_bt} returns 95\%-confidence intervals
#' (95\%-CI) for limits of agreement (LoA). Calculation is based on
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

calc_ci_loa_bt <- function(bt, input_dt, biasMod, loa_l, loa_u, var_loa) {
  source("R/blandxtrMain.pre.R")
  # prepare matrix for storing results of bootstrapping
  s <- matrix(NA, nrow = bt, ncol = 5)
  colnames(s) = (c("lower LoA", "upper LoA", "SD LoA", "z_l", "z_u"))

  # generate B bootstrap samples of input_dt
  for (i in 1:bt){
    # boot <- sample(nrow(input_dt), replace = TRUE)
    # boot_dt <- as.data.table(input_dt[boot])
    ### TEST
    # boot_dt <- as.data.table(sample(input_dt, 300, replace = TRUE))
    # start_time <- Sys.time()
    boot_dt <- input_dt[sample(nrow(input_dt), 300, replace = TRUE), ]
    # end_time <- Sys.time()
    # time_sample <- end_time - start_time
    ###
    # start_time <- Sys.time()
    boot <- blandxtrMain_pre(bt, boot_dt, biasMod)
    # end_time <- Sys.time()
    # time_ba <- end_time - start_time
    #
    # start_time <- Sys.time()
    s[i,1] <- boot$loa$loa_l
    s[i,2] <- boot$loa$loa_u
    s[i,3] <- sqrt(boot$var_loa)
    s[i,4] <- (boot$loa$loa_l-loa_l)/var_loa
    s[i,5] <- (boot$loa$loa_u-loa_u)/var_loa
    # end_time <- Sys.time()
    # time_matrix <- end_time - start_time
  }

  # start_time <- Sys.time()
  ci_l_loa_l_bt <- loa_l-(sqrt(var_loa)*quantile(s[,4],0.975))
  ci_u_loa_l_bt <- loa_l-(sqrt(var_loa)*quantile(s[,4],0.025))
  ci_l_loa_u_bt <- loa_u-(sqrt(var_loa)*quantile(s[,5],0.975))
  ci_u_loa_u_bt <- loa_u-(sqrt(var_loa)*quantile(s[,5],0.025))
  # end_time <- Sys.time()
  # time_quantiles <- end_time - start_time
  return(
    list(
      ci_l_loa_l_bt = ci_l_loa_l_bt,
      ci_u_loa_l_bt = ci_u_loa_l_bt,
      ci_l_loa_u_bt = ci_l_loa_u_bt,
      ci_u_loa_u_bt = ci_u_loa_u_bt
      # time_sample = time_sample,
      # time_matrix = time_matrix,
      # time_ba = time_ba,
      # time_quantiles = time_quantiles
    )
  )
}
