# standard calculation of variances with true value varying (tvv) for calculation of limits of agreement (LoA)
# Olofsen et al. 2015

# -------------------------------------
# standard tvv
# -------------------------------------

# lambda

ans <- 0
for(i in 1:n) {
  m_i <- outputSubjects[subject == i,
    m_i]
  ans <- ans + (m_i^2)
}

lambda <- ((n_obs^2)-ans)/((n-1)*n_obs)
rm(ans)
# -------------------------------------

# within subject-variance (wsv) based on mssr

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

rm(ans)
# within subject-variance (wsv)

wsv <- mssr

# -------------------------------------

# between subject-variance (bsv) based on mssi

# mssi
ans <- 0
for(i in 1:n) {
  d_i <- outputSubjects[subject == i,
    d_i]
  m_i <- outputSubjects[subject == i,
    m_i]
  ans <- ans + m_i*((d_i-d)^2)
}

mssi <- (1/(n-1))*ans
rm(ans)
# between subject-variance (bsv)

bsv <- (mssi-wsv)/lambda

# -------------------------------------

# variance of all differences (var_d)

var_d <- ((1-(1/lambda))*mssr)+((1/lambda)*mssi)

    # TO DO: create test (see if formula above is equal to following, change text if not!)
    # var_d <- bsv + wsv

# -------------------------------------

# standard deviation of all differences (sd_d)

sd_d <- sqrt (var_d)

# -------------------------------------

# variance of variance of differences

var_var_d <- ((2*(((1-(1/lambda))*wsv)^2))/(n_obs-n)) + ((2*(((wsv/lambda)+bsv)^2))/(n-1))
rm(i, j, d_i, d_ij, m_i)
