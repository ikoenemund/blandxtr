#' @title 95\%-confidence intervals for LoA (Bland Altman)
#'
#' @description \code{calc_ci_loa_ba} returns 95\%-confidence intervals (95\%-CI)
#' for limits of agreement (LoA) based on a method proposed
#' by Bland and Altman (1999).
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param loa_l lower limit of agreement
#' @param loa_u upper limit of agreement
#' @param var_loa variance of limits of agreement
#'
#' @return A list with the following elements is returned
#' \itemize{
#'  \item{\code{ci_l_loa_l_ba}} {lower limit of 95\%-CI for lower LoA}
#'  \item{\code{ci_u_loa_l_ba}} {upper limit of 95\%-CI for lower LoA}
#'  \item{\code{ci_l_loa_u_ba}} {lower limit of 95\%-CI for upper LoA}
#'  \item{\code{ci_u_loa_u_ba}} {upper limit of 95\%-CI for upper LoA}
#' }

calc_ci_loa_ba <- function(loa_l, loa_u, var_loa) {
  ci_l_loa_l_ba <- loa_l-(1.96*(sqrt(var_loa)))
  ci_u_loa_l_ba <- loa_l+(1.96*(sqrt(var_loa)))
  ci_l_loa_u_ba <- loa_u-(1.96*(sqrt(var_loa)))
  ci_u_loa_u_ba <- loa_u+(1.96*(sqrt(var_loa)))

  return(
    list(
      ci_l_loa_l_ba = ci_l_loa_l_ba,
      ci_u_loa_l_ba = ci_u_loa_l_ba,
      ci_l_loa_u_ba = ci_l_loa_u_ba,
      ci_u_loa_u_ba = ci_u_loa_u_ba
      )
  )
}
#
# calc_ci_loa_u_ba <- function(loa_u, var_loa) {
#   ci_l_loa_u_ba <- loa_u-(1.96*(sqrt(var_loa)))
#   ci_u_loa_u_ba <- loa_u+(1.96*(sqrt(var_loa)))
#
#   return(
#     list(
#       ci_l_loa_u_ba <- ci_l_loa_u_ba,
#       ci_u_loa_u_ba <- ci_u_loa_u_b
#     )
#   )
# }


# # without functions
#
# ci_l_loa_l_ba <- loa_l-(1.96*(sqrt(var_loa)))
# ci_u_loa_l_ba <- loa_l+(1.96*(sqrt(var_loa)))
#
# ci_l_loa_u_ba <- loa_u-(1.96*(sqrt(var_loa)))
# ci_u_loa_u_ba <- loa_u+(1.96*(sqrt(var_loa)))

# # TEST
# ci_l_loa_l_ba <- loa_l-(1.96*(sqrt(var_loa_a)))
# ci_u_loa_l_ba <- loa_l+(1.96*(sqrt(var_loa_a)))
#
# ci_l_loa_u_ba <- loa_u-(1.96*(sqrt(var_loa_a)))
# ci_u_loa_u_ba <- loa_u+(1.96*(sqrt(var_loa_a)))
