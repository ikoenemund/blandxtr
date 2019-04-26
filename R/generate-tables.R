#' @title Tables with results of modified Bland Altman-analysis
#'
#' @description \code{blandxtr.results.table} summarizes the results of
#' modified Bland Altman-analysis performed with \code{blandxtrMain} as
#' LaTeX-tables.
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param res list with results from \code{blandxtr}
#' @param bt number of bootstrap samples (no bootstrapping if bt <= 0)
#' @param bias_mod set TRUE for modified calculation of bias (small wsv) and
#' its variance, set FALSE for standard calculation of bias (small bsv) and
#' its variance
#' @param alpha for 100*(1-alpha)\%-confidence interval around LoA
#' @param beta for 100*(1-beta)\%-confidence interval around bias
#'
#' @return Table with analysis results
#' @return Table with modified analysis results
#' @return Table with repeatability coefficients
#' @return Table with individual residuals
#' @return Table with residuals
#'
#' @export

generate_tables <- function (res, bt, bias_mod, alpha, beta) {

  # -----------------------------------------
  # check input
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_list(res, add = coll)
  checkmate::assert_integer(bt, add = coll)
  checkmate::assert_logical(bias_mod, add = coll)
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, add = coll)
  checkmate::assert_numeric(beta, lower = 0, upper = 1, add = coll)
  checkmate::reportAssertions(coll)
  # -----------------------------------------

  if (bt < 1){
    # analysis results: using matrix
    analysis_results_m <- matrix(NA, nrow=10, ncol=3)
    rownames(analysis_results_m)=(c("Bias", "SD of the differences",
      "lower limit of agreement", "upper limit of agreement",
      "MOVER CI lower LoA", "MOVER CI upper LoA",
      "BA CI lower LoA", "BA CI upper LoA",
      "Within-subject variance (WSV)", "Between-subject variance (BSV)"))
    colnames(analysis_results_m) = (c("value", " ", "+/- SE"))
    analysis_results_m["Bias",1]=res$bv$d
    analysis_results_m[,1] = c(res$bv$d, res$var_tvv$sd_d,
      res$loa$loa_l, res$loa$loa_u, res$loa_mover$ci_l_loa_l_mover,
      res$loa_mover$ci_l_loa_u_mover, res$loa_ba$ci_l_loa_l_ba,
      res$loa_ba$ci_l_loa_u_ba, res$var_tvv$wsv, res$var_tvv$bsv)

    analysis_results_m[,2]=c(NA,NA,NA,NA,res$loa_mover$ci_u_loa_l_mover,
      res$loa_mover$ci_u_loa_u_mover, res$loa_bt$ci_u_loa_l_bt,
      res$loa_bt$ci_u_loa_u_bt, res$loa_ba$ci_u_loa_l_ba,
      res$loa_ba$ci_u_loa_u_ba, NA, NA)

    analysis_results_m[2,3]=c(res$var_tvv$se_sd_d)
    analysis_results_m[11,3]=c(res$var_tvv$se_wsv)
    analysis_results_m[12,3]=c(res$var_tvv$se_bsv)

    # -----------------------------------
    # modified analysis results: using matrix
    analysis_results_mod_m <- matrix(NA, nrow=10, ncol=3)
    rownames(analysis_results_mod_m)=(c("Bias", "SD of the differences",
      "lower limit of agreement", "upper limit of agreement",
      "MOVER CI lower LoA", "MOVER CI upper LoA",
      "BA CI lower LoA", "BA CI upper LoA",
      "Within-subject variance (WSV)", "Between-subject variance (BSV)"))
    colnames(analysis_results_mod_m) = (c("value", " ", "+/- SE"))
    analysis_results_mod_m["Bias",1]=res$bv$d
    analysis_results_mod_m[,1] = c(res$bv$d, res$var_tvv$sd_d_mod,
      res$loa_mod$loa_l, res$loa_mod$loa_u, res$loa_mover_mod$ci_l_loa_l_mover,
      res$loa_mover_mod$ci_l_loa_u_mover, res$loa_ba_mod$ci_l_loa_l_ba,
      res$loa_ba_mod$ci_l_loa_u_ba, res$var_tvv$wsv_mod, res$var_tvv$bsv_mod)

    analysis_results_mod_m[,2]=c(NA,NA,NA,NA,res$loa_mover_mod$ci_u_loa_l_mover,
      res$loa_mover_mod$ci_u_loa_u_mover, res$loa_bt_mod$ci_u_loa_l_bt,
      res$loa_bt_mod$ci_u_loa_u_bt, res$loa_ba_mod$ci_u_loa_l_ba,
      res$loa_ba_mod$ci_u_loa_u_ba, NA, NA)

    analysis_results_m[2,3]=c(res$var_tvv$se_sd_d_mod)
    analysis_results_m[11,3]=c(res$var_tvv$se_wsv_mod)
    analysis_results_m[12,3]=c(res$var_tvv$se_bsv_mod)

  } else {
    # analysis results: using matrix
    analysis_results_m <- matrix(NA, nrow=12, ncol=3)
    rownames(analysis_results_m)=(c("Bias", "SD of the differences",
      "lower limit of agreement", "upper limit of agreement",
      "MOVER CI lower LoA", "MOVER CI upper LoA",
      "BT CI lower LoA", "BT CI upper LoA",
      "BA CI lower LoA", "BA CI upper LoA",
      "Within-subject variance (WSV)", "Between-subject variance (BSV)"))
    colnames(analysis_results_m) = (c("value", " ", "+/- SE"))
    analysis_results_m["Bias",1]=res$bv$d
    analysis_results_m[,1] = c(res$bv$d, res$var_tvv$sd_d,
      res$loa$loa_l, res$loa$loa_u, res$loa_mover$ci_l_loa_l_mover,
      res$loa_mover$ci_l_loa_u_mover, res$loa_bt$ci_l_loa_l_bt,
      res$loa_bt$ci_l_loa_u_bt, res$loa_ba$ci_l_loa_l_ba,
      res$loa_ba$ci_l_loa_u_ba, res$var_tvv$wsv, res$var_tvv$bsv)

    analysis_results_m[,2]=c(NA,NA,NA,NA,res$loa_mover$ci_u_loa_l_mover,
      res$loa_mover$ci_u_loa_u_mover, res$loa_bt$ci_u_loa_l_bt,
      res$loa_bt$ci_u_loa_u_bt, res$loa_ba$ci_u_loa_l_ba,
      res$loa_ba$ci_u_loa_u_ba, NA, NA)

    analysis_results_m[2,3]=c(res$var_tvv$se_sd_d)
    analysis_results_m[11,3]=c(res$var_tvv$se_wsv)
    analysis_results_m[12,3]=c(res$var_tvv$se_bsv)

    # -----------------------------------
    # modified analysis results: using matrix
    analysis_results_mod_m <- matrix(NA, nrow=12, ncol=3)
    rownames(analysis_results_mod_m)=(c("Bias", "SD of the differences",
      "lower limit of agreement", "upper limit of agreement",
      "MOVER CI lower LoA", "MOVER CI upper LoA",
      "BT CI lower LoA", "BT CI upper LoA",
      "BA CI lower LoA", "BA CI upper LoA",
      "Within-subject variance (WSV)", "Between-subject variance (BSV)"))
    colnames(analysis_results_mod_m) = (c("value", " ", "+/- SE"))
    analysis_results_mod_m["Bias",1]=res$bv$d
    analysis_results_mod_m[,1] = c(res$bv$d, res$var_tvv$sd_d_mod,
      res$loa_mod$loa_l, res$loa_mod$loa_u, res$loa_mover_mod$ci_l_loa_l_mover,
      res$loa_mover_mod$ci_l_loa_u_mover, res$loa_bt_mod$ci_l_loa_l_bt,
      res$loa_bt_mod$ci_l_loa_u_bt, res$loa_ba_mod$ci_l_loa_l_ba,
      res$loa_ba_mod$ci_l_loa_u_ba, res$var_tvv$wsv_mod, res$var_tvv$bsv_mod)

    analysis_results_mod_m[,2]=c(NA,NA,NA,NA,res$loa_mover_mod$ci_u_loa_l_mover,
      res$loa_mover_mod$ci_u_loa_u_mover, res$loa_bt_mod$ci_u_loa_l_bt,
      res$loa_bt_mod$ci_u_loa_u_bt, res$loa_ba_mod$ci_u_loa_l_ba,
      res$loa_ba_mod$ci_u_loa_u_ba, NA, NA)

    analysis_results_m[2,3]=c(res$var_tvv$se_sd_d_mod)
    analysis_results_m[11,3]=c(res$var_tvv$se_wsv_mod)
    analysis_results_m[12,3]=c(res$var_tvv$se_bsv_mod)
  }
  # -----------------------------------
  # repCoeff_table
  # using data.table

  rep_coeff_m <- matrix(NA, nrow=5, ncol=1)
  rownames(rep_coeff_m)=(c("SX:", "SY:",
    "SX/SY:", "mean X:", "mean Y:"))
  colnames(rep_coeff_m) = (c("value"))
  rep_coeff_m["SX:",1]=res$bv$rep_coeff$s_x
  rep_coeff_m["SY:",1]=res$bv$rep_coeff$s_y
  rep_coeff_m["SX/SY:",1]=res$bv$rep_coeff$s_x_s_y
  rep_coeff_m["mean X:",1]=res$bv$mean_x
  rep_coeff_m["mean Y:",1]=res$bv$mean_y

  # -----------------------------------
  # individualMeans_table
  # using data.table

  ind_means <- copy(res$bv$output_subjects)
  setnames(ind_means,"d_i", "Mean")
  setnames(ind_means,"m_i", "M")

  # -----------------------------------
  # table with residuals
  # using data.table

  resid <- res$bv$output_measurements[, list(subject, measurement_id, r_ij)]
  setnames(resid,"r_ij", "Residual")
  setnames(resid,"measurement_id", "ID (Messung)")

  # -----------------------------------
  # table with input data

  input_data <- res$bv$output_measurements[, list(subject, measurement_id,
    measurement_x, measurement_y)]
  setnames(input_data,"subject", "Subject")
  setnames(input_data,"measurement_x", "Measurement X")
  setnames(input_data,"measurement_y", "Measurement Y")
  setnames(input_data,"measurement_id", "ID (Messung)")

  # -----------------------------------
  # table with input parameters (bias_mod, alpha, beta, bt, tau, tau_mod)
  input_param_m <- matrix(NA, nrow=6, ncol=1)
  rownames(input_param_m)=(c("bias_mod:", "alpha:",
    "beta:", "number of bootstrapping samples:", "tau:", "tau_mod:"))
  colnames(input_param_m) = (c("value"))
  if(bias_mod){
    input_param_m["bias_mod:",1]="TRUE"
  } else {
    input_param_m["bias_mod:",1]="FALSE"
  }
  input_param_m["alpha:",1]=alpha
  input_param_m["beta:",1]=beta
  if(bt>0){
    input_param_m["number of bootstrapping samples:",1]=bt
  } else {
    input_param_m["number of bootstrapping samples:",1]="no bootstrapping"
  }
  input_param_m["tau:",1]=res$var_tvv$tau
  input_param_m["tau_mod:",1]=res$var_tvv$tau_mod

  return(
    list(
      analysis_results_m = analysis_results_m,
      analysis_results_mod_m = analysis_results_mod_m,
      rep_coeff_m = rep_coeff_m,
      ind_means = ind_means,
      resid = resid,
      input_data = input_data,
      input_param_m = input_param_m
    )
  )

}
