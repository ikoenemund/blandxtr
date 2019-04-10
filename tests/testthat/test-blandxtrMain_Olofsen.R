
# -----------------------------------------
# test blandxtrMain with data from Olofsen et al. 2015

context("blandxtrMain.Olofsen")

# -----------------------------------------
# test results of basicVariables

test_that("number of subjects is 20", {
  expect_equal(olofsen_result$res$bv$n,20)
})

test_that("number of measurements is 300", {
  expect_equal(olofsen_result$res$bv$n_obs,300)
})

test_that("mean of all differences (bias) is 0.497674", {
  expect_equal(olofsen_result$res$bv$d,0.497674)
  expect_equal(olofsen_result$res$bv$d_a,0.497674)
})

test_that("difference of measurements (d_15) is correct", {
  expect_equal(olofsen_result$res$bv$outputMeasurements$d_ij[5],
    (olofsen_result$res$bv$outputMeasurements$measurementX[5]-
        olofsen_result$res$bv$outputMeasurements$measurementY[5]))
})

test_that("mean of measurements (m_13) is correct", {
  expect_equal(olofsen_result$res$bv$outputMeasurements$m_ij[3],
    mean(c(olofsen_result$res$bv$outputMeasurements$measurementX[3],
      olofsen_result$res$bv$outputMeasurements$measurementY[3])))
})

# -----------------------------------------
# test analysis of variances (var.tvv) (mod)
test_that("sd of the differences is 0.8229147", {
  expect_equal(olofsen_result$res$var_tvv$sd_d_mod, 0.8229147, tolerance=1e-4)
})

test_that("within-subject variance (wsv) is 0.1861722", {
  expect_equal(olofsen_result$res$var_tvv$wsv_mod, 0.1861722, tolerance=1e-4)
})

test_that("between-subject variance (bsv) is 0.4910164", {
  expect_equal(olofsen_result$res$var_tvv$bsv_mod, 0.4910164, tolerance=1e-4)
})

test_that("SE of sd of differences is 0.0996412", {
  expect_equal(olofsen_result$res$var_tvv$se_sd_d_mod, 0.0996412, tolerance=1e-4)
})

test_that("SE of between-subject variance (bsv) is 0.1633369", {
  expect_equal(olofsen_result$res$var_tvv$se_bsv_mod, 0.1633369, tolerance=1e-4)
})

test_that("SE of within-subject variance (wsv) is 0.0157344", {
  expect_equal(olofsen_result$res$var_tvv$se_wsv_mod, 0.0157344, tolerance=1e-4)
})

# -----------------------------------------
# test limits of agreement (loa) (mod)

test_that("lower limit of agreement is -1.1152388", {
  expect_equal(olofsen_result$res$loa_mod$loa_l, -1.1152388, tolerance=1e-4)
})

test_that("upper limit of agreement is 2.1105868", {
  expect_equal(olofsen_result$res$loa_mod$loa_u, 2.1105868, tolerance=1e-4)
})

# -----------------------------------------
# test variance of limits of agreement (loa): Bland Altman-method (mod)

test_that("lower limit of 95%-CI of lower loa is -1.608 (Bland Altman)", {
  expect_equal(olofsen_result$res$loa_ba_mod$ci_l_loa_l_ba, -1.608, tolerance=1e-2)
})

test_that("upper limit of 95%-CI of lower loa is -0.622 (Bland Altman)", {
  expect_equal(olofsen_result$res$loa_ba_mod$ci_u_loa_l_ba, -0.622, tolerance=1e-2)
})

test_that("lower limit of 95%-CI of upper loa is 1.617 (Bland Altman)", {
  expect_equal(olofsen_result$res$loa_ba_mod$ci_l_loa_u_ba, 1.617, tolerance=1e-2)
})

test_that("upper limit of 95%-CI of upper loa is 2.604 (Bland Altman)", {
  expect_equal(olofsen_result$res$loa_ba_mod$ci_u_loa_u_ba, 2.604, tolerance=1e-2)
})

# -----------------------------------------
# test variance of limits of agreement (loa): MOVER-method (mod)

test_that("lower limit of 95%-CI of lower loa is -1.771 (MOVER)", {
  expect_equal(olofsen_result$res$loa_mover_mod$ci_l_loa_l_mover, -1.771, tolerance=1e-3)
})

test_that("upper limit of 95%-CI of lower loa is -0.698 (MOVER)", {
  expect_equal(olofsen_result$res$loa_mover_mod$ci_u_loa_l_mover, -0.698, tolerance=1e-3)
})

test_that("lower limit of 95%-CI of upper loa is 1.693 (MOVER)", {
  expect_equal(olofsen_result$res$loa_mover_mod$ci_l_loa_u_mover, 1.693, tolerance=1e-3)
})

test_that("upper limit of 95%-CI of upper loa is 2.766 (MOVER)", {
  expect_equal(olofsen_result$res$loa_mover_mod$ci_u_loa_u_mover, 2.766, tolerance=1e-3)
})

# -----------------------------------------
# test variance of limits of agreement (loa): parametric bootstrap-t (mod)

test_that("lower limit of 95%-CI of lower loa is -1.815 (bootstrapping)", {
  expect_equal(olofsen_result$res$loa_bt_mod$ci_l_loa_l_bt, -1.815, tolerance=1e-1)
})

test_that("upper limit of 95%-CI of lower loa is -0.727 (bootstrapping)", {
  expect_equal(olofsen_result$res$loa_bt_mod$ci_u_loa_l_bt, -0.727, tolerance=1e-1)
})

test_that("lower limit of 95%-CI of upper loa is 1.714 (bootstrapping)", {
  expect_equal(olofsen_result$res$loa_bt_mod$ci_l_loa_u_bt, 1.714, tolerance=1e-1)
})

test_that("upper limit of 95%-CI of upper loa is 2.788 (bootstrapping)", {
  expect_equal(olofsen_result$res$loa_bt_mod$ci_u_loa_u_bt, 2.788, tolerance=1e-1)
})

# -----------------------------------------
# repeatability coefficients
test_that("repeatability coefficients are correct", {
  expect_equal(olofsen_result$res$bv$mean_x, 6.6060381, tolerance=1e-3)
  expect_equal(olofsen_result$res$bv$mean_y, 6.1083641, tolerance=1e-3)
  expect_equal(olofsen_result$res$bv$rep_coeff$mssr_x, 2.4654073, tolerance=1e-3)
  expect_equal(olofsen_result$res$bv$rep_coeff$mssr_y, 2.4016111, tolerance=1e-3)

  expect_equal(olofsen_result$res$bv$rep_coeff$s_x, 1.570, tolerance=1e-3)
  expect_equal(olofsen_result$res$bv$rep_coeff$s_y, 1.550, tolerance=1e-3)
  expect_equal(olofsen_result$res$bv$rep_coeff$s_x_s_y, 1.013, tolerance=1e-3)

})

# -----------------------------------------
# residuals
test_that("residual (d_21) is correct (-0.511)", {
  expect_equal(olofsen_result$res$bv$outputMeasurements$r_ij[16], -0.511,
    tolerance=1e-3)

})

# -----------------------------------------
# tau_mod
test_that("tau_mod is correct (0.7250807)", {
  expect_equal(olofsen_result$res$var_tvv$tau_mod, 0.7250807,
    tolerance=1e-3)

})
