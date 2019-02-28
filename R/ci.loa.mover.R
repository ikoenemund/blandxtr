#' @title 95\%-confidence intervals for LoA (MOVER)
#'
#' @description \code{calc_ci_loa_mover} returns 95\%-confidence intervals
#' (95\%-CI) for limits of agreement (LoA) based on MOVER-method
#' (Methods of Variance Estimates Recovery) proposed
#' by Zou (2013).
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param n number of subjects
#' @param n_obs number of measurements
#' @param outputSubjects data.table containing subject ID and
#' number of measurements of each subject (m_i)
#' @param bsv_mod between-subject variance (modified calculation)
#' @param wsv within-subject variance
#' @param loa_l lower limit of agreement
#' @param loa_u upper limit of agreement
#'
#' @return A list with the following elements is returned
#' \itemize{
#'  \item{\code{ci_l_loa_l_mover}} {lower limit of 95\%-CI for lower LoA}
#'  \item{\code{ci_u_loa_l_mover}} {upper limit of 95\%-CI for lower LoA}
#'  \item{\code{ci_l_loa_u_mover}} {lower limit of 95\%-CI for upper LoA}
#'  \item{\code{ci_u_loa_u_mover}} {upper limit of 95\%-CI for upper LoA}
#' }
#'

calc_ci_loa_mover <- function (n, n_obs, outputSubjects, bsv_mod, wsv, loa_l,
  loa_u) {
  # harmonic mean (m_h)
  ans <- 0
  for(i in 1:n) {
    m_i <- outputSubjects[subject == i,
      m_i]
    ans <- ans + (1/m_i)
  }
  m_h <- n/ans

  rm(ans, i)


  # s ((s_tot)^2)
  s <- bsv_mod+((1-(1/m_h))*wsv)

  # l
  chi1 <- qchisq(0.975, df=19, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  chi2 <- qchisq(0.975, df=280, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  l <- s - ((((bsv_mod*(1-((n-1)/(chi1))))^2)+(((1-(1/m_h))*wsv*
      (1-((n_obs-n)/(chi2))))^2))^(1/2))

  rm (chi1, chi2)

  # u
  chi3 <- qchisq(0.025, df=19, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  chi4 <- qchisq(0.025, df=280, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  u <- s+((((bsv_mod*(((n-1)/chi3)-1))^2)+(((1-(1/m_h))*wsv*
      ((((n_obs-n)/chi4))-1))^2))^(1/2))

  rm (chi3, chi4)

  #rme
  z1 <- qnorm(0.025, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  z2 <- qnorm(0.475, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

  rme <- (((z1^2)*(bsv_mod/n))+((z2^2)*(((sqrt(s)-(sqrt(l))))^2)))^(1/2)

  #lme
  lme <- (((z1^2)*(bsv_mod/n))+((z2^2)*(((sqrt(u)-(sqrt(s))))^2)))^(1/2)

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
          ci_u_loa_u_mover = ci_u_loa_u_mover,

          # TEST
          lme = lme,
          rme = rme,
          l = l,
          u = u

        )
      )
}
