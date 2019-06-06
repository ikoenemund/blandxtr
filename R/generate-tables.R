#' @title Tables with results of advanced Bland Altman-analysis
#'
#' @description \code{genrate_tables} summarizes the results of
#' advanced Bland Altman-analysis performed with \code{blandxtr} as
#' tables.
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param res list with results from \code{blandxtr}
#'
#' @return Table with analysis results
#' @return Table with modified analysis results
#' @return Table with parameters for calculation of repeatability coefficients
#' @return Table with individual residuals
#' @return Table with residuals
#' @return Table with input parameters
#' @return Table with input data
#'
#' @export

generate_tables <- function (res) {

  # -----------------------------------------
  # check input
  checkmate::assert_list(res)

  # -----------------------------------------
  # create tables containing analysis results

  # two cases: one displays bootstrapping results,
  # the other one does not (when bootstrapping is skipped)

  if (res$bt < 1){
    # create table containing unmodified analysis results (without
    # bootstrapping)
    analysis_results_m <- matrix(NA, nrow=10, ncol=3)
    rownames(analysis_results_m)=(c("Bias (mean of all differences)",
      "SD of the differences", "Lower limit of agreement",
      "Upper limit of agreement", "CI lower LoA (MOVER)",
      "CI upper LoA (MOVER)", "CI lower LoA (BA)", "CI upper LoA (BA)",
      "Within-subject variance", "Between-subjects variance"))
    colnames(analysis_results_m) = (c("Value", " ", "+/- SE"))
    analysis_results_m["Bias (mean of all differences)",1]=res$bv$d
    analysis_results_m[,1] = c(res$bv$d, res$var_tvv$sd_d,
      res$loa$loa_l, res$loa$loa_u, res$ci_loa_mover$ci_l_loa_l_mover,
      res$ci_loa_mover$ci_l_loa_u_mover, res$ci_loa_ba$ci_l_loa_l_ba,
      res$ci_loa_ba$ci_l_loa_u_ba, res$var_tvv$wsv, res$var_tvv$bsv)

    analysis_results_m[,2]=c(NA,NA,NA,NA,res$ci_loa_mover$ci_u_loa_l_mover,
      res$ci_loa_mover$ci_u_loa_u_mover, res$ci_loa_ba$ci_u_loa_l_ba,
      res$ci_loa_ba$ci_u_loa_u_ba, NA, NA)

    analysis_results_m[1,3]=c(res$var_loa$se_d)
    analysis_results_m[2,3]=c(res$var_tvv$se_sd_d)
    analysis_results_m[9,3]=c(res$var_tvv$se_wsv)
    analysis_results_m[10,3]=c(res$var_tvv$se_bsv)

    # -----------------------------------
    # create table containing modified analysis results  (without bootstrapping)

    analysis_results_mod_m <- matrix(NA, nrow=10, ncol=3)
    rownames(analysis_results_mod_m)=(c("Bias (mean of all differences)",
      "SD of the differences", "Lower limit of agreement",
      "Upper limit of agreement", "CI lower LoA (MOVER)",
      "CI upper LoA (MOVER)", "CI lower LoA (BA)", "CI upper LoA (BA)",
      "Within-subject variance", "Between-subjects variance"))
    colnames(analysis_results_mod_m) = (c("Value", " ", "+/- SE"))
    analysis_results_mod_m["Bias (mean of all differences)",1]=res$bv$d
    analysis_results_mod_m[,1] = c(res$bv$d, res$var_tvv$sd_d_mod,
      res$loa_mod$loa_l, res$loa_mod$loa_u,
      res$ci_loa_mover_mod$ci_l_loa_l_mover,
      res$ci_loa_mover_mod$ci_l_loa_u_mover,
      res$ci_loa_ba_mod$ci_l_loa_l_ba, res$ci_loa_ba_mod$ci_l_loa_u_ba,
      res$var_tvv$wsv_mod, res$var_tvv$bsv_mod)

    analysis_results_mod_m[,2]=c(NA,NA,NA,NA,
      res$ci_loa_mover_mod$ci_u_loa_l_mover,
      res$ci_loa_mover_mod$ci_u_loa_u_mover,
      res$ci_loa_ba_mod$ci_u_loa_l_ba, res$ci_loa_ba_mod$ci_u_loa_u_ba, NA, NA)

    analysis_results_mod_m[1,3]=c(res$var_loa_mod$se_d)
    analysis_results_mod_m[2,3]=c(res$var_tvv$se_sd_d_mod)
    analysis_results_mod_m[9,3]=c(res$var_tvv$se_wsv_mod)
    analysis_results_mod_m[10,3]=c(res$var_tvv$se_bsv_mod)

  } else {
    # create table containing unmodified analysis results (includes
    # bootstrapping results)

    analysis_results_m <- matrix(NA, nrow=12, ncol=3)
    rownames(analysis_results_m)=(c("Bias (mean of all differences)",
      "SD of the differences", "Lower limit of agreement",
      "Upper limit of agreement", "CI lower LoA (MOVER)",
      "CI upper LoA (MOVER)", "CI lower LoA (BT)", "CI upper LoA (BT)",
      "CI lower LoA (BA)", "CI upper LoA (BA)",
      "Within-subject variance", "Between-subjects variance"))
    colnames(analysis_results_m) = (c("Value", " ", "+/- SE"))
    analysis_results_m["Bias (mean of all differences)",1]=res$bv$d
    analysis_results_m[,1] = c(res$bv$d, res$var_tvv$sd_d,
      res$loa$loa_l, res$loa$loa_u, res$ci_loa_mover$ci_l_loa_l_mover,
      res$ci_loa_mover$ci_l_loa_u_mover, res$ci_loa_bt$ci_l_loa_l_bt,
      res$ci_loa_bt$ci_l_loa_u_bt, res$ci_loa_ba$ci_l_loa_l_ba,
      res$ci_loa_ba$ci_l_loa_u_ba, res$var_tvv$wsv, res$var_tvv$bsv)

    analysis_results_m[,2]=c(NA,NA,NA,NA,res$ci_loa_mover$ci_u_loa_l_mover,
      res$ci_loa_mover$ci_u_loa_u_mover, res$ci_loa_bt$ci_u_loa_l_bt,
      res$ci_loa_bt$ci_u_loa_u_bt, res$ci_loa_ba$ci_u_loa_l_ba,
      res$ci_loa_ba$ci_u_loa_u_ba, NA, NA)

    analysis_results_m[1,3]=c(res$var_loa$se_d)
    analysis_results_m[2,3]=c(res$var_tvv$se_sd_d)
    analysis_results_m[11,3]=c(res$var_tvv$se_wsv)
    analysis_results_m[12,3]=c(res$var_tvv$se_bsv)

    # -----------------------------------
    # create table containing modified analysis results (includes bootstrapping
    # results)

    analysis_results_mod_m <- matrix(NA, nrow=12, ncol=3)
    rownames(analysis_results_mod_m)=(c("Bias (mean of all differences)",
      "SD of the differences", "Lower limit of agreement",
      "Upper limit of agreement", "CI lower LoA (MOVER)",
      "CI upper LoA (MOVER)", "CI lower LoA (BT)", "CI upper LoA (BT)",
      "CI lower LoA (BA)", "CI upper LoA (BA)",
      "Within-subject variance", "Between-subjects variance"))
    colnames(analysis_results_mod_m) = (c("Value", " ", "+/- SE"))
    analysis_results_mod_m["Bias (mean of all differences)",1]=res$bv$d
    analysis_results_mod_m[,1] = c(res$bv$d, res$var_tvv$sd_d_mod,
      res$loa_mod$loa_l, res$loa_mod$loa_u,
      res$ci_loa_mover_mod$ci_l_loa_l_mover,
      res$ci_loa_mover_mod$ci_l_loa_u_mover,
      res$ci_loa_bt_mod$ci_l_loa_l_bt, res$ci_loa_bt_mod$ci_l_loa_u_bt,
      res$ci_loa_ba_mod$ci_l_loa_l_ba, res$ci_loa_ba_mod$ci_l_loa_u_ba,
      res$var_tvv$wsv_mod, res$var_tvv$bsv_mod)

    analysis_results_mod_m[,2]=c(NA,NA,NA,NA,
      res$ci_loa_mover_mod$ci_u_loa_l_mover,
      res$ci_loa_mover_mod$ci_u_loa_u_mover,
      res$ci_loa_bt_mod$ci_u_loa_l_bt, res$ci_loa_bt_mod$ci_u_loa_u_bt,
      res$ci_loa_ba_mod$ci_u_loa_l_ba, res$ci_loa_ba_mod$ci_u_loa_u_ba, NA, NA)

    analysis_results_mod_m[1,3]=c(res$var_loa_mod$se_d)
    analysis_results_mod_m[2,3]=c(res$var_tvv$se_sd_d_mod)
    analysis_results_mod_m[11,3]=c(res$var_tvv$se_wsv_mod)
    analysis_results_mod_m[12,3]=c(res$var_tvv$se_bsv_mod)
  }
  # -----------------------------------
  # create table containing parameters for calculation of
  # repeatability coefficients

  param_rep_coeff_m <- matrix(NA, nrow=7, ncol=1)
  rownames(param_rep_coeff_m)=(c("SX", "SY",
    "SX/SY", "Mean X", "Mean Y", "Mean X (alternative)",
    "Mean Y (alternative)"))
  colnames(param_rep_coeff_m) = (c("Value"))
  param_rep_coeff_m["SX",1]=res$bv$param_rep_coeff$s_x
  param_rep_coeff_m["SY",1]=res$bv$param_rep_coeff$s_y
  param_rep_coeff_m["SX/SY",1]=res$bv$param_rep_coeff$s_x_s_y
  param_rep_coeff_m["Mean X",1]=res$bv$mean_x
  param_rep_coeff_m["Mean Y",1]=res$bv$mean_y
  param_rep_coeff_m["Mean X (alternative)",1]=res$bv$mean_x_a
  param_rep_coeff_m["Mean Y (alternative)",1]=res$bv$mean_y_a

  # -----------------------------------
  # create table displaying individual means of the differences

  ind_means <- copy(res$bv$output_subjects)
  setnames(ind_means,"subject", "Subject")
  setnames(ind_means,"d_i", "Individual mean")
  setnames(ind_means,"m_i", "Number of measurements")

  # -----------------------------------
  # create table displaying residuals

  resid <- res$bv$output_measurements[, list(subject, measurement_id, r_ij)]
  setnames(resid,"subject", "Subject")
  setnames(resid,"r_ij", "Residual")
  setnames(resid,"measurement_id", "ID (measurement)")

  # -----------------------------------
  # create table displaying input data

  input_data <- res$bv$output_measurements[, list(subject, measurement_id,
    measurement_x, measurement_y)]
  setnames(input_data,"subject", "Subject")
  setnames(input_data,"measurement_x", "Measurement X")
  setnames(input_data,"measurement_y", "Measurement Y")
  setnames(input_data,"measurement_id", "ID (measurement)")

  # -----------------------------------
  # # create table displaying input parameters
  # (bias_alt, alpha, beta, bt, tau, tau_mod)

  input_param_m <- matrix(NA, nrow=1, ncol=6)
  colnames(input_param_m)=(c("Bias_alt", "Alpha",
    "Beta", "Number of bootstrapping samples (bt)", "Tau", "Tau_mod"))
  rownames(input_param_m) = (c("Value"))

  input_param <- as.data.frame(input_param_m)
  input_param[1,"Bias_alt"]=res$bias_alt
  input_param[1,"Alpha"]=res$alpha
  input_param[1,"Beta"]=res$beta
  if(res$bt>0){
    input_param[1,"Number of bootstrapping samples (bt)"]=res$bt
  } else {
    input_param[1,"Number of bootstrapping samples (bt)"]="no bootstrapping"
  }
  input_param[1,"Tau"]=res$var_tvv$tau
  input_param[1,"Tau_mod"]=res$var_tvv$tau_mod

  # -----------------------------------
  invisible(
    list(
      analysis_results_m = analysis_results_m,
      analysis_results_mod_m = analysis_results_mod_m,
      param_rep_coeff_m = param_rep_coeff_m,
      ind_means = ind_means,
      resid = resid,
      input_data = input_data,
      input_param = input_param
    )
  )
}
