#' @title Helper function for main method for blandxtr
#'
#' @description \code{main_pre} performs modified Bland Altman-analysis
#' as proposed by Olofsen et al. (2015) without calculation of
#' confidence intervals. Helper function for \code{blandxtr} which performs
#' the whole analysis.
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param bt number of bootstrap samples
#' @param input_dt data.table with input dataset
#' @param bias_mod set TRUE for modified calculation of bias (small wsv),
#' set FALSE for standard calculation of bias (small bsv)
#' @param beta for 100*(1-beta)\%-confidence interval around bias
#'
#' @note \code{bias_mod} is automatically set TRUE for
#' different number of measurements in each subject (unbalanced case)
#' @note "_mod" labels results based on modified true value varies-method
#'
#' @return A list containing the return values of all used functions.
#'
#' @export
#'

main_pre <- function (input_dt, bt, bias_mod, beta) {

  # -----------------------------------------
  # check input
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_data_table(input_dt, add = coll)
  checkmate::assert_integer(bt, add = coll)
  checkmate::assert_logical(bias_mod, add = coll)
  checkmate::assert_numeric(beta, lower = 0, upper = 1, add = coll)
  checkmate::reportAssertions(coll)
  # -----------------------------------------

  # -----------------------------------------
  # calculate basic variables
  bv <- basic_variables(input_dt)

  # unbalanced case: bias_mod = TRUE
  for (i in 1:bv$n){
    if (bv$output_subjects$m_i[i]!=(bv$n_obs/bv$n)){
      bias_mod <- TRUE
      break
    }
  }
  rm(i)

  if (bias_mod){
    bv$d <- bv$d_a
  }

  # -----------------------------------------
  # analysis of variances
  var_tvv <- var_tvv(bv$n, bv$n_obs, bv$d, bv$d_a, bv$output_subjects,
    bv$output_measurements)

  # -----------------------------------------
  # calculate limits of agreement (loa) (standard and modified)

  # limits of agreement (based on standard tvv)
  loa <- loa(bv$d, var_tvv$sd_d, beta)

  # limits of agreement (based on modified tvv)
  loa_mod <- loa(bv$d_a, var_tvv$sd_d_mod, beta)

  # -----------------------------------------
  # calculate variance of limits of agreement (loa)

  # variance of loa (based on standard tvv)
  var_loa <- var_loa (bv$n, bv$n_obs, var_tvv$bsv, var_tvv$wsv,
    bv$output_subjects, var_tvv$var_var_d, bias_mod, beta)

  # variance of loa (based on modified tvv)
  var_loa_mod <- var_loa (bv$n, bv$n_obs, var_tvv$bsv_mod, var_tvv$wsv_mod,
    bv$output_subjects, var_tvv$var_var_d_mod, bias_mod, beta)

  # -----------------------------------------

  return(
    list(
      bv = bv,
      var_tvv = var_tvv,
      loa = loa,
      loa_mod = loa_mod,
      var_loa = var_loa,
      var_loa_mod = var_loa_mod,
      bias_mod = bias_mod
    )
  )
}
