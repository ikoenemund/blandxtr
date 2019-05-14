# Advanced Bland Altman-analysis on simulated data (repeated measurements with
# same number of measurements (10) for each subject (100))

# unmodified analysis method
# skip bootstrapping

# -----------------------------------------
# get input data

load_RData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
input_dt <- load_RData("test-data-balanced.RData")

# -----------------------------------------
# perform Bland Altman-analysis

test_data_balanced_result <- blandxtr(input_dt, bt = 0, bias_alt = F,
  alpha = 0.05, beta = 0.05)

# -----------------------------------------
# test blandxtr with simulated data

context("blandxtr-test-data-balanced")

# -----------------------------------------
# test analysis of variances (var.tvv) (unmodified)
test_that("sd of the differences is 0.1961057", {
  expect_equal(test_data_balanced_result$var_tvv$sd_d, 0.1961057,
    tolerance=1e-4)
})

test_that("within-subject variance (wsv) is 0.0384300", {
  expect_equal(test_data_balanced_result$var_tvv$wsv, 0.0384300, tolerance=1e-4)
})

test_that("between-subject variance (bsv) is 0.0000275", {
  expect_equal(test_data_balanced_result$var_tvv$bsv, 0.0000275, tolerance=1e-4)
})

test_that("SE of sd of differences is 0.0043873", {
  expect_equal(test_data_balanced_result$var_tvv$se_sd_d, 0.0043873,
    tolerance=1e-4)
})

test_that("SE of between-subject variance (bsv) is 0.0005792", {
  expect_equal(test_data_balanced_result$var_tvv$se_bsv, 0.0005792,
    tolerance=1e-4)
})

test_that("SE of within-subject variance (wsv) is 0.0018116", {
  expect_equal(test_data_balanced_result$var_tvv$se_wsv, 0.0018116,
    tolerance=1e-4)
})

# -----------------------------------------
# var-_loa-function: test SE of bias

test_that("standard error of bias is 0.0062213", {
  expect_equal(test_data_balanced_result$var_loa$se_d, 0.0062213,
    tolerance=1e-4)
})

# -----------------------------------------
# test calculation of tau (unmodified)
test_that("tau is correct (0.0007147)", {
  expect_equal(test_data_balanced_result$var_tvv$tau, 0.0007147,
    tolerance=1e-3)

})
