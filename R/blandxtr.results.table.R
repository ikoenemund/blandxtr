#' @title Tables with results of modified Bland Altman-analysis
#'
#' @description \code{blandxtr.results.table} summarizes the results of
#' modified Bland Altman-analysis performed with \code{blandxtrMain} as
#' LaTeX-tables.
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param res list with results from \code{blandxtrMain}
#' @param bt number of bootstrap samples (no bootstrapping if bt <= 0)
#'
#' @return Table with analysis results
#' @return Table with modified analysis results
#' @return Table with repeatability coefficients
#' @return Table with individual residuals
#' @return Table with residuals

blandxtr_results_table <- function (res, bt) {
  if (bt < 1){
    # analysis results: using matrix and xtable
    library(xtable)
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
    test_m <- xtable(analysis_results_m, digits = 7, NA.string = "-")
    ana_res_tab <- print(test_m, type="latex", file = "report/ana_res_tab.tex")

    # -----------------------------------
    # modified analysis results: using matrix and xtable
    library(xtable)
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
    test_m <- xtable(analysis_results_mod_m, digits = 7, NA.string = "-")
    ana_res_mod_tab <- print(test_m, type="latex", file = "report/ana_res_mod_tab.tex")
  } else {
    # analysis results: using matrix and xtable
    library(xtable)
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
    test_m <- xtable(analysis_results_m, digits = 7, NA.string = "-")
    ana_res_tab <- print(test_m, type="latex", file = "report/ana_res_tab.tex")

    # -----------------------------------
    # modified analysis results: using matrix and xtable
    library(xtable)
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
    test_m <- xtable(analysis_results_mod_m, digits = 7, NA.string = "-")
    ana_res_mod_tab <- print(test_m, type="latex", file = "report/ana_res_mod_tab.tex")
  }
  # -----------------------------------
  # repCoeff_table
  # using data.table and xtable

  rep_coeff_m <- matrix(NA, nrow=5, ncol=1)
  rownames(rep_coeff_m)=(c("SX:", "SY:",
    "SX/SY:", "mean X:", "mean Y:"))
  colnames(rep_coeff_m) = (c("value"))
  rep_coeff_m["SX:",1]=res$bv$rep_coeff$s_x
  rep_coeff_m["SY:",1]=res$bv$rep_coeff$s_y
  rep_coeff_m["SX/SY:",1]=res$bv$rep_coeff$s_x_s_y
  rep_coeff_m["mean X:",1]=res$bv$mean_x
  rep_coeff_m["mean Y:",1]=res$bv$mean_y

  rep_coeff <- xtable(rep_coeff_m, digits = 7, NA.string = "-")
  rep_coeff_tab <- print(rep_coeff, type="latex",
    file = "report/rep_coeff_tab.tex")

  # -----------------------------------
  # individualMeans_table
  # using data.table and xtable

  ind_means <- copy(res$bv$outputSubjects)
  setnames(ind_means,"d_i", "Mean")
  setnames(ind_means,"m_i", "M")
  ind_means <- xtable(ind_means, digits = 3, NA.string = "-")
  ind_means_tab <- print(ind_means, type="latex", include.rownames=FALSE,
    file = "report/ind_means_tab.tex")

  # -----------------------------------
  # table with residuals
  # using data.table and xtable

  addtorow          <- list()
  addtorow$pos      <- list()
  addtorow$pos[[1]] <- c(0)
  addtorow$command  <- c(paste("\\hline \n",
    "\\endhead \n",
    "\\hline \n",
    "{\\footnotesize Continued on next page} \n",
    "\\endfoot \n",
    "\\endlastfoot \n",sep=""))

  resid <- res$bv$outputMeasurements[, list(subject, measurement_id, r_ij)]
  setnames(resid,"r_ij", "Residual")
  setnames(resid,"measurement_id", "ID (Messung)")
  resid <- xtable(resid, digits = 3, NA.string = "-", longtable = TRUE)
  resid_tab <- print(resid, tabular.environment = "longtable", floating = FALSE,
    include.rownames = FALSE,  # because addtorow will substitute the default row names
    add.to.row = addtorow,     # this is where you actually make the substitution
    hline.after=c(-1), # because addtorow will substitute the default hline for the first row
    file = "report/resid_tab.tex")

  # -----------------------------------
  # table with input data

  addtorow          <- list()
  addtorow$pos      <- list()
  addtorow$pos[[1]] <- c(0)
  addtorow$command  <- c(paste("\\hline \n",
    "\\endhead \n",
    "\\hline \n",
    "{\\footnotesize Continued on next page} \n",
    "\\endfoot \n",
    "\\endlastfoot \n",sep=""))

  input_data <- res$bv$outputMeasurements[, list(subject, measurement_id,
    measurementX, measurementY)]
  setnames(input_data,"subject", "Subject")
  setnames(input_data,"measurementX", "Measurement X")
  setnames(input_data,"measurementY", "Measurement Y")
  setnames(input_data,"measurement_id", "ID (Messung)")
  input_data <- xtable(input_data, digits = 3, NA.string = "-", longtable = TRUE)
  input_data_tab <- print(input_data, tabular.environment = "longtable", floating = FALSE,
    include.rownames = FALSE,  # because addtorow will substitute the default row names
    add.to.row = addtorow,     # this is where you actually make the substitution
    hline.after=c(-1), # because addtorow will substitute the default hline for the first row
    file = "report/input_data_tab.tex")

  # -----------------------------------
  # table with input parameters (biasMod, alpha, beta, bt, tau, tau_mod)
  input_param_m <- matrix(NA, nrow=6, ncol=1)
  rownames(input_param_m)=(c("biasMod:", "alpha:",
    "beta:", "number of bootstrapping samples:", "tau:", "tau_mod:"))
  colnames(input_param_m) = (c("value"))
  if(biasMod){
    input_param_m["biasMod:",1]="TRUE"
  } else {
    input_param_m["biasMod:",1]="FALSE"
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

  input_param <- xtable(input_param_m, digits = 2, NA.string = "-")
  input_param_tab <- print(input_param, type="latex",
    file = "report/input_param_tab.tex")

  return(
    list(
      ana_res_tab = ana_res_tab,
      ana_res_mod_tab = ana_res_mod_tab,
      rep_coeff_tab = rep_coeff_tab,
      ind_means_tab = ind_means_tab,
      resid_tab = resid_tab,
      input_data_tab = input_data_tab
    )
  )

}
