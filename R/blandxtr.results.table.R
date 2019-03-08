blandxtr_results_table <- function (res) {

  # using matrix and xtable
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
    res$loa_mover$ci_u_loa_u_mover, res$loa_ba$ci_u_loa_l_ba,
    res$loa_ba$ci_u_loa_u_ba, NA, NA)
  test_m <- xtable(analysis_results_m, digits = 7, NA.string = "-")
  ana_res_tab <- print(test_m, type="latex", file = "report/ana_res_tab.tex")

  # -----------------------------------

  # individualMeans_table
  # using data.table and xtable

  library(xtable)
  ind_means <- copy(res$bv$outputSubjects)
  # remove column "var_d_i"
  ind_means[, var_d_i:=NULL]
  setnames(ind_means,"d_i", "Mean")
  setnames(ind_means,"m_i", "M")
  ind_means <- xtable(ind_means, digits = 7, NA.string = "-")
  ind_means_tab <- print(ind_means, type="latex", include.rownames=FALSE,
    file = "report/ind_means_tab.tex")

  return(
    list(
      ana_res_tab = ana_res_tab,
      ind_means_tab = ind_means_tab
    )
  )


}
