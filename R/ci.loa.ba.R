# Bland Altman method: calculation of confidence intervals (CI) for limits of agreement

# using function
calc_ci_loa_l_ba <- function(loa_l, loa_u, var_loa) {
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
