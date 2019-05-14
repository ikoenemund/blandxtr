# Advanced Bland Altman-analysis on modified data from Olofsen et al. (2015).
# source (web): https://sec.lumc.nl/method_agreement_analysis/sim.txt
# Reduced original dataset at random by 100 rows

# bias_alt: TRUE
# modified analysis method
# skip bootstrapping

# -----------------------------------------
# get input data
load_RData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
input_dt <- load_RData("olofsen-unbalanced.RData")

# perform Band Altman-analysis on modified data from Olofsen et al.
olofsen_unbalanced_result <- blandxtr(input_dt, bt = 0, bias_alt = T,
  alpha = 0.05, beta = 0.05)

# -----------------------------------------
# test blandxtr with simulated data

context("blandxtr-olofsen-unbalanced")

# -----------------------------------------
# test result of alternative bias calculation with 'basic_variables'

test_that("alternative mean of all differences (bias) is 0.4820496", {
  expect_equal(olofsen_unbalanced_result$bv$d_a, 0.4820496, tolerance=1e-7)
})

# ----------------------
# ----------------------
# print

test_that("'print' generates non-standard output if bias_alt=T", {
  expect_output(print(olofsen_unbalanced_result),
    regexp="Main results from advanced Bland Altman-analysis:")
})

# ----------------------
# generate tables: bt = 0 (bootstrapping skipped: tables should not show
# any bootstrapping results )
tab <- generate_tables(olofsen_unbalanced_result)

# convert to data frame for subsetting by name in tests
analysis_results_df <- as.data.frame(tab$analysis_results_m)
test_that("table of basic variables contains correct values", {
  expect_equal(olofsen_unbalanced_result$bv$d, analysis_results_df
    ["Bias (mean of all differences)", 1])
  expect_equal(olofsen_unbalanced_result$var_loa$se_d, analysis_results_df
    ["Bias (mean of all differences)", 3])
  expect_equal(olofsen_unbalanced_result$var_tvv$sd_d, analysis_results_df
    ["SD of the differences", 1])
  expect_equal(olofsen_unbalanced_result$var_tvv$se_sd_d, analysis_results_df
    ["SD of the differences", 3])
  expect_equal(olofsen_unbalanced_result$loa$loa_l, analysis_results_df
    ["Lower limit of agreement", 1])
  expect_equal(olofsen_unbalanced_result$loa$loa_u, analysis_results_df
    ["Upper limit of agreement", 1])
  expect_equal(olofsen_unbalanced_result$ci_loa_mover$ci_l_loa_l_mover,
    analysis_results_df["CI lower LoA (MOVER)", 1])
  expect_equal(olofsen_unbalanced_result$ci_loa_mover$ci_u_loa_l_mover,
    analysis_results_df["CI lower LoA (MOVER)", 2])
  expect_equal(olofsen_unbalanced_result$ci_loa_mover$ci_l_loa_u_mover,
    analysis_results_df["CI upper LoA (MOVER)", 1])
  expect_equal(olofsen_unbalanced_result$ci_loa_mover$ci_u_loa_u_mover,
    analysis_results_df["CI upper LoA (MOVER)", 2])
  expect_equal(olofsen_unbalanced_result$ci_loa_ba$ci_l_loa_l_ba,
    analysis_results_df["CI lower LoA (BA)", 1])
  expect_equal(olofsen_unbalanced_result$ci_loa_ba$ci_u_loa_l_ba,
    analysis_results_df["CI lower LoA (BA)", 2])
  expect_equal(olofsen_unbalanced_result$ci_loa_ba$ci_l_loa_u_ba,
    analysis_results_df["CI upper LoA (BA)", 1])
  expect_equal(olofsen_unbalanced_result$ci_loa_ba$ci_u_loa_u_ba,
    analysis_results_df["CI upper LoA (BA)", 2])
  expect_equal(olofsen_unbalanced_result$var_tvv$wsv,
    analysis_results_df["Within-subject variance", 1])
  expect_equal(olofsen_unbalanced_result$var_tvv$se_wsv,
    analysis_results_df["Within-subject variance", 3])
  expect_equal(olofsen_unbalanced_result$var_tvv$bsv,
    analysis_results_df["Between-subjects variance", 1])
  expect_equal(olofsen_unbalanced_result$var_tvv$se_bsv,
    analysis_results_df["Between-subjects variance", 3])
})

analysis_results_mod_df <- as.data.frame(tab$analysis_results_mod_m)
test_that("table of basic variables (mod) contains correct values", {
  expect_equal(olofsen_unbalanced_result$bv$d, analysis_results_mod_df
    ["Bias (mean of all differences)", 1])
  expect_equal(olofsen_unbalanced_result$var_loa_mod$se_d,
    analysis_results_mod_df["Bias (mean of all differences)", 3])
  expect_equal(olofsen_unbalanced_result$var_tvv$sd_d_mod,
    analysis_results_mod_df["SD of the differences", 1])
  expect_equal(olofsen_unbalanced_result$var_tvv$se_sd_d_mod,
    analysis_results_mod_df["SD of the differences", 3])
  expect_equal(olofsen_unbalanced_result$loa_mod$loa_l,
    analysis_results_mod_df["Lower limit of agreement", 1])
  expect_equal(olofsen_unbalanced_result$loa_mod$loa_u,
    analysis_results_mod_df["Upper limit of agreement", 1])
  expect_equal(olofsen_unbalanced_result$ci_loa_mover_mod$ci_l_loa_l_mover,
    analysis_results_mod_df["CI lower LoA (MOVER)", 1])
  expect_equal(olofsen_unbalanced_result$ci_loa_mover_mod$ci_u_loa_l_mover,
    analysis_results_mod_df["CI lower LoA (MOVER)", 2])
  expect_equal(olofsen_unbalanced_result$ci_loa_mover_mod$ci_l_loa_u_mover,
    analysis_results_mod_df["CI upper LoA (MOVER)", 1])
  expect_equal(olofsen_unbalanced_result$ci_loa_mover_mod$ci_u_loa_u_mover,
    analysis_results_mod_df["CI upper LoA (MOVER)", 2])
  expect_equal(olofsen_unbalanced_result$ci_loa_ba_mod$ci_l_loa_l_ba,
    analysis_results_mod_df["CI lower LoA (BA)", 1])
  expect_equal(olofsen_unbalanced_result$ci_loa_ba_mod$ci_u_loa_l_ba,
    analysis_results_mod_df["CI lower LoA (BA)", 2])
  expect_equal(olofsen_unbalanced_result$ci_loa_ba_mod$ci_l_loa_u_ba,
    analysis_results_mod_df["CI upper LoA (BA)", 1])
  expect_equal(olofsen_unbalanced_result$ci_loa_ba_mod$ci_u_loa_u_ba,
    analysis_results_mod_df["CI upper LoA (BA)", 2])
  expect_equal(olofsen_unbalanced_result$var_tvv$wsv_mod,
    analysis_results_mod_df["Within-subject variance", 1])
  expect_equal(olofsen_unbalanced_result$var_tvv$se_wsv_mod,
    analysis_results_mod_df["Within-subject variance", 3])
  expect_equal(olofsen_unbalanced_result$var_tvv$bsv_mod,
    analysis_results_mod_df["Between-subjects variance", 1])
  expect_equal(olofsen_unbalanced_result$var_tvv$se_bsv_mod,
    analysis_results_mod_df["Between-subjects variance", 3])
})

# input_param_df <- as.data.frame(tab$input_param_m)
test_that("table of input parameters contains correct values", {
  expect_equal(olofsen_unbalanced_result$bias_alt, tab$input_param[1,
    "bias_alt"])
  expect_equal(olofsen_unbalanced_result$alpha, tab$input_param[1, "alpha"])
  expect_equal(olofsen_unbalanced_result$beta, tab$input_param[1, "beta"])
  expect_equal("no bootstrapping", tab$input_param[1,
    "number of bootstrapping samples (bt)"])
  expect_equal(olofsen_unbalanced_result$var_tvv$tau, tab$input_param[1, "tau"])
  expect_equal(olofsen_unbalanced_result$var_tvv$tau_mod, tab$input_param[1,
    "tau_mod"])
})
