# modified calculation of variances with true value varying (tvv) for calculation of limits of agreement (LoA)
# Olofsen et al. 2015

# -------------------------------------
# modified tvv
# -------------------------------------

calc_var_tvv_mod <- function (n, n_obs, d_a, outputSubjects, outputMeasurements){

  # alternative lambda
  ans <- 0
  for(i in 1:n) {
    m_i <- outputSubjects[subject == i,
      m_i]
    ans <- ans + (1/m_i)
  }

  lambda_mod <- (1/n)*ans
  rm(ans, i, m_i)
  # -------------------------------------

  # within subject-variance (wsv) based on mssr
  # like standard version

  # mssr

  ans <- 0
  for(i in 1:n) {
    m_i <- outputSubjects[subject == i,
      m_i]
    d_i <- outputSubjects[subject == i,
      d_i]
    for(j in 1:m_i) {
      d_ij <- outputMeasurements[subject == i & measurement_id==j,
        d_ij]
      ans <- ans + (d_ij-d_i)^2
    }
  }

  mssr <- (1/(n_obs-n))*ans
  rm(ans, i, j, d_i, m_i, d_ij)

  # within subject-variance (wsv)

  wsv_mod <- mssr

  # -------------------------------------
  # modified between subject-variance (bsv_mod) based on mssi_mod

  # mssi_mod

  ans <- 0
  for(i in 1:n) {
    d_i <- outputSubjects[subject == i,
      d_i]
    m_i <- outputSubjects[subject == i,
      m_i]
    ans <- ans + ((d_i-d_a)^2)
  }

  mssi_mod <- (1/(n-1))*ans

  rm(ans, i, d_i, m_i)

  # modified between subject-variance (bsv)

  bsv_mod <- mssi_mod

  # -------------------------------------

  # modified variance of all differences (var_d_mod)

  var_d_mod <- ((1-lambda_mod)*mssr)+mssi_mod

  # -------------------------------------

  # modified standard deviation of all differences (sd_d_mod)

  sd_d_mod <- sqrt (var_d_mod)

  # -------------------------------------

  # modified variance of variance of differences

  var_var_d_mod <- ((2*(((1-lambda_mod)*wsv_mod)^2))/(n_obs-n)) + ((2*(((wsv_mod*lambda_mod)+bsv_mod)^2))/(n-1))

  # -------------------------------------
  return(
    list(
      wsv_mod = wsv_mod,
      bsv_mod = bsv_mod,
      var_d_mod = var_d_mod,
      sd_d_mod = sd_d_mod,
      var_var_d_mod = var_var_d_mod
    )
  )
}
