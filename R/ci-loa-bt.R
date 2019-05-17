#' @title Confidence intervals for limits of agreement (using parametric bootsrap-t)
#'
#' @description \code{ci_loa_bt} returns confidence intervals
#' (CI) for limits of agreement (LoA). Calculation is based on
#' parametric bootstrap-t.
#'
#' @note For further information please see:
#' \itemize{
#'  \item Efron, B. and Tibshirani, R. J., An introduction to the bootstrap.
#'  Monographs on statistics and applied probability, 57, 1993.
#'  \item Carpenter, J. and Bithell, J., Bootstrap confidence intervals:
#'  when, which, what? A practical guide for medical statisticians.
#'  Statistics in medicine, 19 (9), 2000.
#'  }
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param bt number of bootstrap samples
#' @param input_dt data.table with input dataset
#' @param bias_alt set TRUE for alternative calculation of bias (small
#' within-subject variance) and its variance, set FALSE for standard calculation
#' of bias (small between-subjects variance) and its variance
#' @param loa_l lower limit of agreement
#' @param loa_u upper limit of agreement
#' @param var_loa variance of limits of agreement
#' @param alpha for 100*(1-alpha)\%-confidence interval around LoA
#' @param beta for 100*(1-beta)\%-confidence interval around bias
#' @param n number of subjects
#' @param n_obs number of observations
#' @param d mean of all differences
#' @param d_a modified mean of all differences
#' @param wsv within-subject variance
#' @param bsv between-subjects variance
#'
#' @return A list with the following elements is returned
#' \itemize{
#'  \item{\code{ci_l_loa_l_bt}} {lower limit of 95\%-CI for lower LoA}
#'  \item{\code{ci_u_loa_l_bt}} {upper limit of 95\%-CI for lower LoA}
#'  \item{\code{ci_l_loa_u_bt}} {lower limit of 95\%-CI for upper LoA}
#'  \item{\code{ci_u_loa_u_bt}} {upper limit of 95\%-CI for upper LoA}
#' }
#' @export

ci_loa_bt <- function(bt, input_dt, bias_alt, loa_l, loa_u, var_loa, alpha,
  beta, n, n_obs, d, d_a, bsv, wsv) {

  # -----------------------------------------
  # check input

  coll <- checkmate::makeAssertCollection()
  checkmate::assert_int(bt, add = coll)
  checkmate::assert_data_table(input_dt, add = coll)
  checkmate::assert_logical(bias_alt, add = coll)
  checkmate::assert_numeric(loa_l, add = coll)
  checkmate::assert_numeric(loa_u, add = coll)
  checkmate::assert_numeric(var_loa, add = coll)
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, add = coll)
  checkmate::assert_numeric(beta, lower = 0, upper = 1, add = coll)
  checkmate::assert_int(n, add = coll)
  checkmate::assert_int(n_obs, add = coll)
  checkmate::assert_numeric(d, add = coll)
  checkmate::assert_numeric(d_a, add = coll)
  checkmate::assert_numeric(bsv, add = coll)
  checkmate::assert_numeric(wsv, add = coll)
  checkmate::reportAssertions(coll)

  # -----------------------------------------
  #  generate bootstrapping-samples (parametric sampling)
  boot_samp <- vector("list", bt)
  i <- 1:bt

  generate_boot_dt <- function(i){
    # create random deviation from bias for each subject
    # (considering between-subjects-standarddeviation)
    j <- 1:n
    df_I <- data.frame(j=1:n, value_i = d + stats::rnorm(n, 0, sqrt(bsv)))

    # original input data
    df_w <- as.data.frame(input_dt)

    # create random numbers for every repeated measurement of each subject
    # (considering within-subject-standarddeviation)
    # results in random deviation between repeated measurements
    df_w$value_w = stats::rnorm(n_obs, 0, sqrt(wsv))

    # combination of created deviations
    df_simu <- merge(df_I, df_w, by.x = 'j', by.y = 'subject')

    # adding the deviations provides (simulated) differences between methods
    df_simu$diff <- df_simu$value_i + df_simu$value_w

    # create a data.table which can be used as input for 'blandxtr'-function
    # 'measurement_x' corresponds to the simulated differences calculated above,
    # 'measurement_y' is set to 0 which results in d_ij=simulated difference
    dt_simu <- as.data.table(df_simu)
    # rename columns of dt_simu
    setnames(dt_simu,"j", "subject")
    dt_simu$measurement_x <- NULL
    dt_simu$measurement_y <- NULL
    setnames(dt_simu,"diff", "measurement_x")
    setnames(dt_simu,"value_i", "measurement_y")
    dt_simu$measurement_y <- 0
    dt_simu$value_w <- NULL

    boot_samp[[i]] <- dt_simu
  }

  boot_samp <- lapply(i, generate_boot_dt)
  rm(i)

  # -----------------------------------------
  # Bland Altman analysis per bootstrapping-sample

  boot <- lapply(boot_samp, main_pre, bt=bt, bias_alt=bias_alt,
    beta)

  # -----------------------------------------
  # initialize and fill matrix containing main results from
  # Bland Altman-analysis

  s <- matrix(NA, nrow = bt, ncol = 5)
  colnames(s) = (c("lower LoA", "upper LoA", "SD LoA", "z_l", "z_u"))

  for(r in 1:bt){
    s[r,1] <- boot[[r]]$loa$loa_l
    s[r,2] <- boot[[r]]$loa$loa_u
    s[r,3] <- sqrt(boot[[r]]$var_loa$var_loa)
    s[r,4] <- ((boot[[r]]$loa$loa_l-loa_l))/sqrt(boot[[r]]$var_loa$var_loa)
    s[r,5] <- ((boot[[r]]$loa$loa_u-loa_u))/sqrt(boot[[r]]$var_loa$var_loa)
  }
  rm(r)

  # -----------------------------------------
  # calculate confidence intervals

  ci_l_loa_l_bt <- unname(loa_l-(sqrt(var_loa)*
      stats::quantile(s[,4],probs=1-(alpha/2))))
  ci_u_loa_l_bt <- unname(loa_l-(sqrt(var_loa)*
      stats::quantile(s[,4],probs=(alpha/2))))
  ci_l_loa_u_bt <- unname(loa_u-(sqrt(var_loa)*
      stats::quantile(s[,5],probs=1-(alpha/2))))
  ci_u_loa_u_bt <- unname(loa_u-(sqrt(var_loa)*
      stats::quantile(s[,5],probs=(alpha/2))))

  rm(s)

  # -----------------------------------------
  return(
    list(
      ci_l_loa_l_bt = ci_l_loa_l_bt,
      ci_u_loa_l_bt = ci_u_loa_l_bt,
      ci_l_loa_u_bt = ci_l_loa_u_bt,
      ci_u_loa_u_bt = ci_u_loa_u_bt
    )
  )
}
