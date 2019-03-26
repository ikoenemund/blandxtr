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

  test_m <- xtable(rep_coeff_m, digits = 7, NA.string = "-")
  rep_coeff_tab <- print(test_m, type="latex",
    file = "report/rep_coeff_tab.tex")
  # -----------------------------------

  # individualMeans_table
  # using data.table and xtable

  ind_means <- copy(res$bv$outputSubjects)
  # remove column "var_d_i"
  ind_means[, var_d_i:=NULL]
  setnames(ind_means,"d_i", "Mean")
  setnames(ind_means,"m_i", "M")
  ind_means <- xtable(ind_means, digits = 7, NA.string = "-")
  ind_means_tab <- print(ind_means, type="latex", include.rownames=FALSE,
    file = "report/ind_means_tab.tex")

  # -----------------------------------

  # table with residuals
  # using data.table and xtable

  # resid <- res$bv$outputMeasurements[, list(subject, measurement_id, r_ij)]
  # setnames(resid,"r_ij", "Residual")
  # setnames(resid,"measurement_id", "ID (Messung)")
  # resid <- xtable(resid, digits = 7, NA.string = "-", longtable = TRUE)
  # resid_tab <- print(resid, type="latex", tabular.environment="longtable",
  #   include.rownames=FALSE, file = "report/resid_tab.tex")

  ### TEST
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
  resid <- xtable(resid, digits = 7, NA.string = "-", longtable = TRUE)
  resid_tab <- print(resid, tabular.environment = "longtable", floating = FALSE,
    include.rownames = FALSE,  # because addtorow will substitute the default row names
    add.to.row = addtorow,     # this is where you actually make the substitution
    hline.after=c(-1), # because addtorow will substitute the default hline for the first row
    file = "report/resid_tab.tex")
  ###

  return(
    list(
      ana_res_tab = ana_res_tab,
      ana_res_mod_tab = ana_res_mod_tab,
      ind_means_tab = ind_means_tab,
      resid_tab = resid_tab,
      rep_coeff_tab = rep_coeff_tab
    )
  )

}
