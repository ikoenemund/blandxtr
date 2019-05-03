#' @title Confidence intervals for LoA (MOVER)
#'
#' @description \code{ci_loa_mover} returns confidence intervals
#' (CI) for limits of agreement (LoA) based on MOVER-method
#' (Methods of Variance Estimates Recovery) proposed
#' by Zou (2013).
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param n number of subjects
#' @param n_obs number of observations
#' @param output_subjects data.table containing subject ID and
#' number of measurements of each subject (m_i)
#' @param mssi_mod mssi (modified calculation)
#' @param wsv within-subject variance
#' @param loa_l lower limit of agreement
#' @param loa_u upper limit of agreement
#' @param alpha for 100*(1-alpha)\%-confidence interval around LoA
#' @param beta for 100*(1-beta)\%-confidence interval around bias
#'
#' @return A list with the following elements is returned
#' \itemize{
#'  \item{\code{ci_l_loa_l_mover}} {lower limit of 95\%-CI for lower LoA}
#'  \item{\code{ci_u_loa_l_mover}} {upper limit of 95\%-CI for lower LoA}
#'  \item{\code{ci_l_loa_u_mover}} {lower limit of 95\%-CI for upper LoA}
#'  \item{\code{ci_u_loa_u_mover}} {upper limit of 95\%-CI for upper LoA}
#' }
#'
#' @export
#'

ci_loa_mover <- function (n, n_obs, output_subjects, mssi_mod, wsv, loa_l,
  loa_u, alpha, beta) {

  # -----------------------------------------
  # check input
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_integer(n, add = coll)
  checkmate::assert_integer(n_obs, add = coll)
  checkmate::assert_data_table(output_subjects, add = coll)
  checkmate::assert_numeric(mssi_mod, add = coll)
  checkmate::assert_numeric(wsv, add = coll)
  checkmate::assert_numeric(loa_l, add = coll)
  checkmate::assert_numeric(loa_u, add = coll)
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, add = coll)
  checkmate::assert_numeric(beta, lower = 0, upper = 1, add = coll)
  checkmate::reportAssertions(coll)
  # -----------------------------------------

  # harmonic mean (m_h)
  helper <- 0
  helper <- 1/(output_subjects[, m_i])
  ans <- sum(helper)
  m_h <- n/ans
  rm(ans, helper)


  # s ((s_tot)^2)
  s <- mssi_mod+((1-(1/m_h))*wsv)

  # l
  chi1 <- stats::qchisq((1-(alpha/2)), df=n-1, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  chi2 <- stats::qchisq((1-(alpha/2)), df=n_obs-n, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  l <- s - ((((mssi_mod*(1-((n-1)/(chi1))))^2)+(((1-(1/m_h))*wsv*
      (1-((n_obs-n)/(chi2))))^2))^(1/2))

  rm (chi1, chi2)

  # u
  chi3 <- stats::qchisq(alpha/2, df=n-1, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  chi4 <- stats::qchisq(alpha/2, df=n_obs-n, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  u <- s+((((mssi_mod*(((n-1)/chi3)-1))^2)+(((1-(1/m_h))*wsv*
      ((((n_obs-n)/chi4))-1))^2))^(1/2))

  rm (chi3, chi4)

  #rme
  z1 <- stats::qnorm(alpha/2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  z2 <- stats::qnorm(beta/2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

  rme <- (((z1^2)*(mssi_mod/n))+((z2^2)*(((sqrt(s)-(sqrt(l))))^2)))^(1/2)

  #lme
  lme <- (((z1^2)*(mssi_mod/n))+((z2^2)*(((sqrt(u)-(sqrt(s))))^2)))^(1/2)

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
