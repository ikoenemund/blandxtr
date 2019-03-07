## ---- analysisResults

### TO DO: ideas for getting right wd automatically
# idea: opts_knit$set(root.dir = '../..')
setwd("C:/Users/IK/Documents/blandxtr")
###
source("R/blandxtrMain.Olofsen.R")

## ---- analysisResults_table

# table with Bland Altman results
# # using data.frame and xtable (not working: should transpose rows and columns)
# library(xtable)
# analysis_results_df <- data.frame("Bias" = olofsen_result$bv$d,
#   "SD of the differences" = olofsen_result$var_tvv$sd_d,
#   "lower limit of agreement" = olofsen_result$loa$loa_l,
#   "upper limit of agreement" = olofsen_result$loa$loa_u)
# test_df <- xtable(analysis_results_df)
# print(test_df, type="latex")

# using matrix and xtable
library(xtable)
analysis_results_m <- matrix(NA, nrow=10, ncol=3)
rownames(analysis_results_m)=(c("Bias", "SD of the differences",
  "lower limit of agreement", "upper limit of agreement",
  "MOVER CI lower LoA", "MOVER CI upper LoA",
  "BA CI lower LoA", "BA CI upper LoA",
  "Within-subject variance (WSV)", "Between-subject variance (BSV)"))
colnames(analysis_results_m) = (c("value", " ", "+/- SE"))
analysis_results_m["Bias",1]=olofsen_result$bv$d
analysis_results_m[,1] = c(olofsen_result$bv$d, olofsen_result$var_tvv$sd_d,
  olofsen_result$loa$loa_l, olofsen_result$loa$loa_u, olofsen_result$loa_mover$ci_l_loa_l_mover,
  olofsen_result$loa_mover$ci_l_loa_u_mover, olofsen_result$loa_ba$ci_l_loa_l_ba,
  olofsen_result$loa_ba$ci_l_loa_u_ba, olofsen_result$var_tvv$wsv, olofsen_result$var_tvv$bsv)

analysis_results_m[,2]=c(NA,NA,NA,NA,olofsen_result$loa_mover$ci_u_loa_l_mover,
  olofsen_result$loa_mover$ci_u_loa_u_mover, olofsen_result$loa_ba$ci_u_loa_l_ba,
  olofsen_result$loa_ba$ci_u_loa_u_ba, NA, NA)
test_m <- xtable(analysis_results_m, digits = 7, NA.string = "-")
print(test_m, type="latex")

## ---- individualMeans_table

# using data.table and xtable

library(xtable)
ind_means <- copy(olofsen_result$bv$outputSubjects)
# remove column "var_d_i"
ind_means[, var_d_i:=NULL]
setnames(ind_means,"d_i", "Mean")
setnames(ind_means,"m_i", "M")
ind_means <- xtable(ind_means, digits = 7, NA.string = "-")
print(ind_means, type="latex", include.rownames=FALSE)

