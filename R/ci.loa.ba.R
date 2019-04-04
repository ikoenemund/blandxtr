#' @title Confidence intervals for LoA (Bland Altman)
#'
#' @description \code{calc_ci_loa_ba} returns confidence intervals (CI)
#' for limits of agreement (LoA) based on a method proposed
#' by Bland and Altman (1999).
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param loa_l lower limit of agreement
#' @param loa_u upper limit of agreement
#' @param var_loa variance of limits of agreement
#' @param alpha for 100*(1-alpha)%-confidence interval around LoA
#'
#' @return A list with the following elements is returned
#' \itemize{
#'  \item{\code{ci_l_loa_l_ba}} {lower limit of 95\%-CI for lower LoA}
#'  \item{\code{ci_u_loa_l_ba}} {upper limit of 95\%-CI for lower LoA}
#'  \item{\code{ci_l_loa_u_ba}} {lower limit of 95\%-CI for upper LoA}
#'  \item{\code{ci_u_loa_u_ba}} {upper limit of 95\%-CI for upper LoA}
#' }

calc_ci_loa_ba <- function(loa_l, loa_u, var_loa, alpha) {
  z <- qnorm(1-alpha/2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  ci_l_loa_l_ba <- loa_l-(z*(sqrt(var_loa)))
  ci_u_loa_l_ba <- loa_l+(z*(sqrt(var_loa)))
  ci_l_loa_u_ba <- loa_u-(z*(sqrt(var_loa)))
  ci_u_loa_u_ba <- loa_u+(z*(sqrt(var_loa)))
  rm(z)

  return(
    list(
      ci_l_loa_l_ba = ci_l_loa_l_ba,
      ci_u_loa_l_ba = ci_u_loa_l_ba,
      ci_l_loa_u_ba = ci_l_loa_u_ba,
      ci_u_loa_u_ba = ci_u_loa_u_ba
      )
  )
}
