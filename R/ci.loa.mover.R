# Mover method (Zou 2013): calculation of confidence intervals (CI) for limits of agreement

ci_loa_mover <- function (n, n_obs, outputSubjects, bsv, wsv, loa_l, loa_u) {
  # harmonic mean (m_h)
  for(i in 1:n) {
    m_i <- outputSubjects[subject == i,
      m_i]
    ans <- ans + (1/m_i)
  }
  m_h <- n/ans

  rm(ans, i)

  # s ((s_tot)^2)
  s= bsv+((1-(1/m_h))*wsv)

  # l
  chi1 <- qchisq(0.975, df=19, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  chi2 <- qchisq(0.975, df=280, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  l <- s - ((((bsv*(1-((n-1)/(chi1))))^2)+(((1-(1/m_h))*wsv*(1-((n_obs-n)/(chi2))))^2))^(1/2))

  rm (chi1, chi2)

  # u
  chi3 <- qchisq(0.025, df=19, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  chi4 <- qchisq(0.025, df=280, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  u <- s+((((bsv*(((n-1)/chi3)-1))^2)+(((1-m_h)*wsv*((((n_obs-n)/chi4))-1))^2))^(1/2))

  rm (chi3, chi4)

  #rme
  z1 <- qnorm(0.025, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  z2 <- qnorm(0.475, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

  rme <- (((z1^2)*(bsv/n))+((z2^2)*(((sqrt(s)-(sqrt(l))))^2)))^(1/2)

  #lme
  lme <- (((z1^2)*(bsv/n))+((z2^2)*(((sqrt(u)-(sqrt(s))))^2)))^(1/2)

  rm (z1, z2)

  # CI lower LoA
  ci_l_loa_l_mover <- loa_l-lme
  ci_u_loa_l_mover <- loa_l+rme

  # CI upper LoA
  ci_l_loa_u_mover <- loa_u-rme
  ci_u_loa_u_mover <- loa_u+lme

  return(
        list(
          ci_l_loa_l_mover = ci_l_loa_l_mover,
          ci_u_loa_l_mover = ci_u_loa_l_mover,

          ci_l_loa_u_mover = ci_l_loa_u_mover,
          ci_u_loa_u_mover = ci_u_loa_u_mover

        )
      )
}
