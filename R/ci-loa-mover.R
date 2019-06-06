#' @title Confidence intervals for limits of agreement (using MOVER method)
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
#' @param lambda_mod helper variable for calculation of modified
#'  between-subjects variance, see Olofsen et al. 2015
#' @param output_subjects data.table containing subject ID and
#' number of measurements of each subject (m_i)
#' @param mssi_mod mssi (modified calculation)
#' @param wsv within-subject variance (unmodified)
#' @param var_d_mod {modified variance of mean of all differences}
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

ci_loa_mover <- function (n, n_obs, lambda_mod, output_subjects, mssi_mod, wsv,
  var_d_mod, loa_l, loa_u, alpha, beta) {

  # -----------------------------------------
  # check input
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_int(n, add = coll)
  checkmate::assert_int(n_obs, add = coll)
  checkmate::assert_numeric(lambda_mod, add = coll)
  checkmate::assert_data_table(output_subjects, add = coll)
  checkmate::assert_numeric(mssi_mod, add = coll)
  checkmate::assert_numeric(wsv, add = coll)
  checkmate::assert_numeric(var_d_mod, add = coll)
  checkmate::assert_numeric(loa_l, add = coll)
  checkmate::assert_numeric(loa_u, add = coll)
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, add = coll)
  checkmate::assert_numeric(beta, lower = 0, upper = 1, add = coll)
  checkmate::reportAssertions(coll)

  # -----------------------------------------
  # calculate some helper-variables for CI-calculation
  # (naming of the variables derived from Zou 2013)

  # l
  chi1 <- stats::qchisq((1-(alpha/2)), df=n-1, ncp = 0, lower.tail = TRUE,
    log.p = FALSE)
  chi2 <- stats::qchisq((1-(alpha/2)), df=n_obs-n, ncp = 0, lower.tail = TRUE,
    log.p = FALSE)
  l <- var_d_mod - ((((mssi_mod*(1-((n-1)/(chi1))))^2)+(((1-lambda_mod)*wsv*
      (1-((n_obs-n)/(chi2))))^2))^(1/2))
  rm (chi1, chi2)

  # u
  chi3 <- stats::qchisq(alpha/2, df=n-1, ncp = 0, lower.tail = TRUE,
    log.p = FALSE)
  chi4 <- stats::qchisq(alpha/2, df=n_obs-n, ncp = 0, lower.tail = TRUE,
    log.p = FALSE)
  u <- var_d_mod + ((((mssi_mod*(((n-1)/chi3)-1))^2)+(((1-lambda_mod)*wsv*
      ((((n_obs-n)/chi4))-1))^2))^(1/2))
  rm (chi3, chi4)

  #rme
  z1 <- stats::qnorm(alpha/2, mean = 0, sd = 1, lower.tail = TRUE,
    log.p = FALSE)
  z2 <- stats::qnorm(beta/2, mean = 0, sd = 1, lower.tail = TRUE,
    log.p = FALSE)
  rme <- (((z1^2)*(mssi_mod/n))+((z2^2)*(((sqrt(var_d_mod)-(sqrt(l))))^2)))^(1/2)

  #lme
  lme <- (((z1^2)*(mssi_mod/n))+((z2^2)*(((sqrt(u)-(sqrt(var_d_mod))))^2)))^(1/2)

  rm (z1, z2)

  # -----------------------------------------
  # calculation of CI of LoA

  # CI lower LoA
  ci_l_loa_l_mover <- loa_l-lme
  ci_u_loa_l_mover <- loa_l+rme

  # CI upper LoA
  ci_l_loa_u_mover <- loa_u-rme
  ci_u_loa_u_mover <- loa_u+lme

  # -----------------------------------------
  return(
    list(
      ci_l_loa_l_mover = ci_l_loa_l_mover,
      ci_u_loa_l_mover = ci_u_loa_l_mover,

      ci_l_loa_u_mover = ci_l_loa_u_mover,
      ci_u_loa_u_mover = ci_u_loa_u_mover

    )
  )
}
