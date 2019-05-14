# Advanced Bland Altman-analysis of data from Olofsen et al. (2015).
# source (web): https://sec.lumc.nl/method_agreement_analysis/sim.txt

# bias_alt = FALSE
# modified analysis method

# ---------------------

context("blandxtr-olofsen")

# ---------------------
# get input data

load_RData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
input_dt <- load_RData("olofsen.RData")

# ----------------------
# prepare_data (function): check columns

colnames(input_dt) = (c("patient", "measurement_x", "measurement_y"))
test_that("check for column 'subject' works", {
  expect_error(prepare_data(input_dt),
    regexp="Error in input dataset: No column named 'subject' found.")

})

colnames(input_dt) = (c("subject", "measurementX", "measurement_y"))
test_that("check for column 'measurement_x' works", {
  expect_error(prepare_data(input_dt),
    regexp="Error in input dataset: No column named 'measurement_x' found.")
})

colnames(input_dt) = (c("subject", "measurement_x", "measurementY"))
test_that("check for column 'measurement_y' works", {
  expect_error(prepare_data(input_dt),
    regexp="Error in input dataset: No column named 'measurement_y' found.")
})

colnames(input_dt) = (c("subject", "measurement_x", "measurement_y"))

# prepare_data: check removal of rows with missing values
input_dt_na <- input_dt
input_dt_na[1,2] <- NA
test_that("missing measurements create warning", {
  expect_message(prepare_data(input_dt_na),
    regexp="Input dataset contains rows with empty values
      which will be automatically removed for analysis.")
})
rm(input_dt_na)

# -------------------------------
# -------------------------------
# test blandxtr (main method) with data from Olofsen et al. 2015

# blandxtr (main function): warning if default values are given
test_that("missing values for alpha and beta create warning", {
  expect_warning(blandxtr(input_dt, 0, F, beta=0.05),
    regexp="Variable `alpha` is missing. Setting to 0.05.")
  expect_warning(blandxtr(input_dt, 0, F, alpha=0.05),
    regexp="Variable `beta` is missing. Setting to 0.05.")
  expect_warning(blandxtr(input_dt, 0, F),
    regexp="Variable `alpha` is missing. Setting to 0.05.")
})

test_that("negative value for bt creates warning", {
  expect_warning(blandxtr(input_dt, -1, F, 0.05, 0.05),
    regexp="'bt' has been given a negative value.")
})

# perform modified Bland Altman-analysis
olofsen_result <- blandxtr(input_dt, bt=2000, bias_alt=F, alpha=0.05, beta=0.05)

# is.blandxtr (function)
test_that("'is.blandxtr' returns correct logical values", {
  expect_true (is.blandxtr(olofsen_result))
  expect_false (is.blandxtr(1))
})

# -----------------------------------------
# test results of basic_variables

test_that("number of subjects is 20", {
  expect_equal(olofsen_result$bv$n,20)
})

test_that("number of measurements is 300", {
  expect_equal(olofsen_result$bv$n_obs,300)
})

test_that("mean of all differences (bias) is 0.497674", {
  expect_equal(olofsen_result$bv$d,0.497674)
  expect_equal(olofsen_result$bv$d_a,0.497674)
})

test_that("difference of measurements (d_15) is correct", {
  expect_equal(olofsen_result$bv$output_measurements$d_ij[5],
    (olofsen_result$bv$output_measurements$measurement_x[5]-
        olofsen_result$bv$output_measurements$measurement_y[5]))
})

test_that("mean of measurements (m_13) is correct", {
  expect_equal(olofsen_result$bv$output_measurements$m_ij[3],
    mean(c(olofsen_result$bv$output_measurements$measurement_x[3],
      olofsen_result$bv$output_measurements$measurement_y[3])))
})

test_that("number of measurements per subject (m_i): m_1 = 15", {
  expect_equal(olofsen_result$bv$output_subjects$m_i[1],15)
})


test_that("mean of differences per subject (d_i): d_1 = 0.879", {
  expect_equal(olofsen_result$bv$output_subjects$d_i[1], 0.879, tolerance=1e-3)
})

# -----------------------------------------
# test modified analysis of variances (var_tvv)
test_that("sd of the differences is 0.8229147", {
  expect_equal(olofsen_result$var_tvv$sd_d_mod, 0.8229147, tolerance=1e-4)
})

test_that("within-subject variance (wsv) is 0.1861722", {
  expect_equal(olofsen_result$var_tvv$wsv_mod, 0.1861722, tolerance=1e-4)
})

test_that("between-subject variance (bsv) is 0.4910164", {
  expect_equal(olofsen_result$var_tvv$bsv_mod, 0.4910164, tolerance=1e-4)
})

test_that("SE of sd of differences is 0.0996412", {
  expect_equal(olofsen_result$var_tvv$se_sd_d_mod, 0.0996412, tolerance=1e-4)
})

test_that("SE of between-subject variance (bsv) is 0.1633369", {
  expect_equal(olofsen_result$var_tvv$se_bsv_mod, 0.1633369, tolerance=1e-4)
})

test_that("SE of within-subject variance (wsv) is 0.0157344", {
  expect_equal(olofsen_result$var_tvv$se_wsv_mod, 0.0157344, tolerance=1e-4)
})

# -----------------------------------------
# test calculation of limits of agreement (loa) (mod)

test_that("lower limit of agreement is -1.1152388", {
  expect_equal(olofsen_result$loa_mod$loa_l, -1.1152388, tolerance=1e-4)
})

test_that("upper limit of agreement is 2.1105868", {
  expect_equal(olofsen_result$loa_mod$loa_u, 2.1105868, tolerance=1e-4)
})

# -----------------------------------------
# test calculation of SE of bias (variance of limits of agreement (loa))

test_that("standard error of bias is 0.1586550", {
  expect_equal(olofsen_result$var_loa_mod$se_d, 0.1586550, tolerance=1e-4)
})

# -----------------------------------------
# test calculation of CI of limits of agreement (loa): Bland Altman-method (mod)

test_that("lower limit of 95%-CI of lower loa is -1.608 (Bland Altman)", {
  expect_equal(olofsen_result$ci_loa_ba_mod$ci_l_loa_l_ba, -1.608,
    tolerance=1e-2)
})

test_that("upper limit of 95%-CI of lower loa is -0.622 (Bland Altman)", {
  expect_equal(olofsen_result$ci_loa_ba_mod$ci_u_loa_l_ba, -0.622,
    tolerance=1e-2)
})

test_that("lower limit of 95%-CI of upper loa is 1.617 (Bland Altman)", {
  expect_equal(olofsen_result$ci_loa_ba_mod$ci_l_loa_u_ba, 1.617,
    tolerance=1e-2)
})

test_that("upper limit of 95%-CI of upper loa is 2.604 (Bland Altman)", {
  expect_equal(olofsen_result$ci_loa_ba_mod$ci_u_loa_u_ba, 2.604,
    tolerance=1e-2)
})

# -----------------------------------------
# test claculation of  CI of limits of agreement (loa): MOVER-method (mod)

test_that("lower limit of 95%-CI of lower loa is -1.771 (MOVER)", {
  expect_equal(olofsen_result$ci_loa_mover_mod$ci_l_loa_l_mover, -1.771,
    tolerance=1e-3)
})

test_that("upper limit of 95%-CI of lower loa is -0.698 (MOVER)", {
  expect_equal(olofsen_result$ci_loa_mover_mod$ci_u_loa_l_mover, -0.698,
    tolerance=1e-3)
})

test_that("lower limit of 95%-CI of upper loa is 1.693 (MOVER)", {
  expect_equal(olofsen_result$ci_loa_mover_mod$ci_l_loa_u_mover, 1.693,
    tolerance=1e-3)
})

test_that("upper limit of 95%-CI of upper loa is 2.766 (MOVER)", {
  expect_equal(olofsen_result$ci_loa_mover_mod$ci_u_loa_u_mover, 2.766,
    tolerance=1e-3)
})

# -----------------------------------------
# test calculation of CI of limits of agreement (loa):
# parametric bootstrap-t (mod)
check_bt <- function(){
  if(olofsen_result$bt<1000) {
    skip("bootstrapping sample too small (<1000)")
  }
}

test_that("lower limit of 95%-CI of lower loa is -1.815 (bootstrapping)", {
  check_bt()
  expect_equal(olofsen_result$ci_loa_bt_mod$ci_l_loa_l_bt, -1.815,
    tolerance=1e-2)
})

test_that("upper limit of 95%-CI of lower loa is -0.727 (bootstrapping)", {
  check_bt()
  expect_equal(olofsen_result$ci_loa_bt_mod$ci_u_loa_l_bt, -0.727,
    tolerance=1e-2)
})

test_that("lower limit of 95%-CI of upper loa is 1.714 (bootstrapping)", {
  check_bt()
  expect_equal(olofsen_result$ci_loa_bt_mod$ci_l_loa_u_bt, 1.714,
    tolerance=1e-2)
})

test_that("upper limit of 95%-CI of upper loa is 2.788 (bootstrapping)", {
  check_bt()
  expect_equal(olofsen_result$ci_loa_bt_mod$ci_u_loa_u_bt, 2.788,
    tolerance=1e-2)
})

# -----------------------------------------
# test calculation of paramerters for calculation of repeatability coefficients
test_that("repeatability coefficients are correct", {
  expect_equal(olofsen_result$bv$mean_x, 6.6060381, tolerance=1e-3)
  expect_equal(olofsen_result$bv$mean_y, 6.1083641, tolerance=1e-3)
  expect_equal(olofsen_result$bv$param_rep_coeff$mssr_x, 2.4654073,
    tolerance=1e-3)
  expect_equal(olofsen_result$bv$param_rep_coeff$mssr_y, 2.4016111,
    tolerance=1e-3)

  expect_equal(olofsen_result$bv$param_rep_coeff$s_x, 1.570, tolerance=1e-3)
  expect_equal(olofsen_result$bv$param_rep_coeff$s_y, 1.550, tolerance=1e-3)
  expect_equal(olofsen_result$bv$param_rep_coeff$s_x_s_y, 1.013,
    tolerance=1e-3)

})

# -----------------------------------------
# test calculation of residuals
test_that("residual (d_21) is correct (-0.511)", {
  expect_equal(olofsen_result$bv$output_measurements$r_ij[16], -0.511,
    tolerance=1e-3)

})

# -----------------------------------------
# test calculation of tau_mod
test_that("tau_mod is correct (0.7250807)", {
  expect_equal(olofsen_result$var_tvv$tau_mod, 0.7250807,
    tolerance=1e-3)

})

# ----------------------
# ----------------------
# test output of 'print'-function

test_that("'print' generates non-standard output if bias_alt=F", {
  expect_output(print(olofsen_result),
    regexp="Main results from advanced Bland Altman-analysis:")
})

# ----------------------
# test 'plot'-function

test_that("'plot' gives warning if type is missing", {
  expect_warning(plot(olofsen_result), regexp="Variable `type` is missing.")
})
test_that("'plot' gives warning if type is given an invalid value", {
  expect_warning(plot(olofsen_result, 2.1),
    regexp="Variable `type` has been given an invalid value.")
})

# -----------------------
# test generate_tables-function (bt>0: tables include bootstrapping-output)

tab <- generate_tables(olofsen_result)

# convert to data frame for subsetting by name in tests
analysis_results_df <- as.data.frame(tab$analysis_results_m)
test_that("table of basic variables contains correct values", {
  expect_equal(olofsen_result$bv$d,
    analysis_results_df["Bias (mean of all differences)", 1])
  expect_equal(olofsen_result$var_loa$se_d,
    analysis_results_df["Bias (mean of all differences)", 3])
  expect_equal(olofsen_result$var_tvv$sd_d,
    analysis_results_df["SD of the differences", 1])
  expect_equal(olofsen_result$var_tvv$se_sd_d,
    analysis_results_df["SD of the differences", 3])
  expect_equal(olofsen_result$loa$loa_l,
    analysis_results_df["Lower limit of agreement", 1])
  expect_equal(olofsen_result$loa$loa_u,
    analysis_results_df["Upper limit of agreement", 1])
  expect_equal(olofsen_result$ci_loa_mover$ci_l_loa_l_mover,
    analysis_results_df["CI lower LoA (MOVER)", 1])
  expect_equal(olofsen_result$ci_loa_mover$ci_u_loa_l_mover,
    analysis_results_df["CI lower LoA (MOVER)", 2])
  expect_equal(olofsen_result$ci_loa_mover$ci_l_loa_u_mover,
    analysis_results_df["CI upper LoA (MOVER)", 1])
  expect_equal(olofsen_result$ci_loa_mover$ci_u_loa_u_mover,
    analysis_results_df["CI upper LoA (MOVER)", 2])
  expect_equal(olofsen_result$ci_loa_bt$ci_l_loa_l_bt,
    analysis_results_df["CI lower LoA (BT)", 1])
  expect_equal(olofsen_result$ci_loa_bt$ci_u_loa_l_bt,
    analysis_results_df["CI lower LoA (BT)", 2])
  expect_equal(olofsen_result$ci_loa_bt$ci_l_loa_u_bt,
    analysis_results_df["CI upper LoA (BT)", 1])
  expect_equal(olofsen_result$ci_loa_bt$ci_u_loa_u_bt,
    analysis_results_df["CI upper LoA (BT)", 2])
  expect_equal(olofsen_result$ci_loa_ba$ci_l_loa_l_ba,
    analysis_results_df["CI lower LoA (BA)", 1])
  expect_equal(olofsen_result$ci_loa_ba$ci_u_loa_l_ba,
    analysis_results_df["CI lower LoA (BA)", 2])
  expect_equal(olofsen_result$ci_loa_ba$ci_l_loa_u_ba,
    analysis_results_df["CI upper LoA (BA)", 1])
  expect_equal(olofsen_result$ci_loa_ba$ci_u_loa_u_ba,
    analysis_results_df["CI upper LoA (BA)", 2])
  expect_equal(olofsen_result$var_tvv$wsv,
    analysis_results_df["Within-subject variance", 1])
  expect_equal(olofsen_result$var_tvv$se_wsv,
    analysis_results_df["Within-subject variance", 3])
  expect_equal(olofsen_result$var_tvv$bsv,
    analysis_results_df["Between-subjects variance", 1])
  expect_equal(olofsen_result$var_tvv$se_bsv,
    analysis_results_df["Between-subjects variance", 3])
})

analysis_results_mod_df <- as.data.frame(tab$analysis_results_mod_m)
test_that("table of basic variables (mod) contains correct values", {
  expect_equal(olofsen_result$bv$d,
    analysis_results_mod_df["Bias (mean of all differences)", 1])
  expect_equal(olofsen_result$var_loa_mod$se_d,
    analysis_results_mod_df["Bias (mean of all differences)", 3])
  expect_equal(olofsen_result$var_tvv$sd_d_mod,
    analysis_results_mod_df["SD of the differences", 1])
  expect_equal(olofsen_result$var_tvv$se_sd_d_mod,
    analysis_results_mod_df["SD of the differences", 3])
  expect_equal(olofsen_result$loa_mod$loa_l,
    analysis_results_mod_df["Lower limit of agreement", 1])
  expect_equal(olofsen_result$loa_mod$loa_u,
    analysis_results_mod_df["Upper limit of agreement", 1])
  expect_equal(olofsen_result$ci_loa_mover_mod$ci_l_loa_l_mover,
    analysis_results_mod_df["CI lower LoA (MOVER)", 1])
  expect_equal(olofsen_result$ci_loa_mover_mod$ci_u_loa_l_mover,
    analysis_results_mod_df["CI lower LoA (MOVER)", 2])
  expect_equal(olofsen_result$ci_loa_mover_mod$ci_l_loa_u_mover,
    analysis_results_mod_df["CI upper LoA (MOVER)", 1])
  expect_equal(olofsen_result$ci_loa_mover_mod$ci_u_loa_u_mover,
    analysis_results_mod_df["CI upper LoA (MOVER)", 2])
  expect_equal(olofsen_result$ci_loa_bt_mod$ci_l_loa_l_bt,
    analysis_results_mod_df["CI lower LoA (BT)", 1])
  expect_equal(olofsen_result$ci_loa_bt_mod$ci_u_loa_l_bt,
    analysis_results_mod_df["CI lower LoA (BT)", 2])
  expect_equal(olofsen_result$ci_loa_bt_mod$ci_l_loa_u_bt,
    analysis_results_mod_df["CI upper LoA (BT)", 1])
  expect_equal(olofsen_result$ci_loa_bt_mod$ci_u_loa_u_bt,
    analysis_results_mod_df["CI upper LoA (BT)", 2])
  expect_equal(olofsen_result$ci_loa_ba_mod$ci_l_loa_l_ba,
    analysis_results_mod_df["CI lower LoA (BA)", 1])
  expect_equal(olofsen_result$ci_loa_ba_mod$ci_u_loa_l_ba,
    analysis_results_mod_df["CI lower LoA (BA)", 2])
  expect_equal(olofsen_result$ci_loa_ba_mod$ci_l_loa_u_ba,
    analysis_results_mod_df["CI upper LoA (BA)", 1])
  expect_equal(olofsen_result$ci_loa_ba_mod$ci_u_loa_u_ba,
    analysis_results_mod_df["CI upper LoA (BA)", 2])
  expect_equal(olofsen_result$var_tvv$wsv_mod,
    analysis_results_mod_df["Within-subject variance", 1])
  expect_equal(olofsen_result$var_tvv$se_wsv_mod,
    analysis_results_mod_df["Within-subject variance", 3])
  expect_equal(olofsen_result$var_tvv$bsv_mod,
    analysis_results_mod_df["Between-subjects variance", 1])
  expect_equal(olofsen_result$var_tvv$se_bsv_mod,
    analysis_results_mod_df["Between-subjects variance", 3])
})

param_rep_coeff_df <- as.data.frame(tab$param_rep_coeff_m)
test_that("table of repeatability coefficients contains correct values", {
  expect_equal(olofsen_result$bv$param_rep_coeff$s_x,
    param_rep_coeff_df["SX", 1])
  expect_equal(olofsen_result$bv$param_rep_coeff$s_y,
    param_rep_coeff_df["SY", 1])
  expect_equal(olofsen_result$bv$param_rep_coeff$s_x_s_y,
    param_rep_coeff_df["SX/SY", 1])
  expect_equal(olofsen_result$bv$mean_x_a, param_rep_coeff_df["mean X", 1])
  expect_equal(olofsen_result$bv$mean_y_a, param_rep_coeff_df["mean Y", 1])
})

test_that("table of input parameters contains correct values", {
  expect_equal(olofsen_result$bias_alt, tab$input_param[1, "bias_alt"])
  expect_equal(olofsen_result$alpha, tab$input_param[1, "alpha"])
  expect_equal(olofsen_result$beta, tab$input_param[1, "beta"])
  expect_equal(olofsen_result$bt,
    tab$input_param[1, "number of bootstrapping samples (bt)"])
  expect_equal(olofsen_result$var_tvv$tau, tab$input_param[1, "tau"])
  expect_equal(olofsen_result$var_tvv$tau_mod, tab$input_param[1, "tau_mod"])
})

rm(tab)
