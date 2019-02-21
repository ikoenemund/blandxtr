# modified calculation of variances with true value varying (tvv) for calculation of limits of agreement (LoA)
# Olofsen et al. 2015

# -------------------------------------
# modified tvv
# -------------------------------------

# lambda alternative

ans <- 0
for(i in 1:n) {
  m_i <- outputSubjects[subject == i,
    m_i]
  ans <- ans + (1/m_i)
}

lambda_a <- (1/n)*ans
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

wsv_a <- mssr

# -------------------------------------
# modified between subject-variance (bsv_a) based on mssi_a

# mssi_a

ans <- 0
for(i in 1:n) {
  d_i <- outputSubjects[subject == i,
    d_i]
  m_i <- outputSubjects[subject == i,
    m_i]
  ans <- ans + ((d_i-d_a)^2)
}

mssi_a <- (1/(n-1))*ans

rm(ans, i, d_i, m_i)

# modified between subject-variance (bsv)

bsv_a <- mssi_a

# -------------------------------------

# modified variance of all differences (var_d_a)

var_d_a <- ((1-lambda_a)*mssr)+mssi_a

# -------------------------------------

# modified standard deviation of all differences (sd_d_a)

sd_d_a <- sqrt (var_d_a)

# -------------------------------------

# modified variance of variance of differences

var_var_d_a <- ((2*(((1-lambda_a)*wsv)^2))/(n_obs-n)) + ((2*(((wsv*lambda_a)+bsv_a)^2))/(n-1))
