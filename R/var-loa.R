#' @title Variance of limits of agreement
#'
#' @description \code{calc_var_loa} returns variance of
#' limits of agreement (LoA) based on a method proposed
#' by Bland and Altman (1999).
#'
#' @note function calculates other variables as well (but does not return)
#' \itemize{
#'  \item{\code{ev_var_d}} {expected value of variance of all differnces}
#'  \item{\code{var_d}} {variance of mean of all differences}
#'  \item{\code{var_d_mod}} {modified variance of mean of all differences}
#'  \item{\code{var_sd_d}} {variance of the standard deviation of the differences}
#' }
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param n number of subjects
#' @param n_obs number of observations
#' @param bsv between-subject variance
#' @param wsv within-subject variance
#' @param output_subjects data.table containing subject ID and
#' number of measurements of each subject (m_i)
#' @param var_var_d variance of the variance of mean of all differences
#' @param bias_mod set FALSE for standard calculation of the variance (small bsv),
#' set TRUE for modified calculation of the variance (small wsv)
#' @param beta for 100*(1-beta)\%-confidence interval around bias
#'
#' @return \code{var_loa} variance of limits of agreement
#'
#' @export

var_loa <- function (n, n_obs, bsv, wsv, output_subjects, var_var_d,
  bias_mod, beta){

  # -----------------------------------------
  # check input
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_integer(n, add = coll)
  checkmate::assert_integer(n_obs, add = coll)
  checkmate::assert_numeric(bsv, add = coll)
  checkmate::assert_numeric(wsv, add = coll)
  checkmate::assert_data_table(output_subjects, add = coll)
  checkmate::assert_numeric(var_var_d, add = coll)
  checkmate::assert_logical(bias_mod, add = coll)
  checkmate::assert_numeric(beta, lower = 0, upper = 1, add = coll)
  checkmate::reportAssertions(coll)
  # -----------------------------------------

  ans1 <- 0
  ans2 <- 0
  helper <- 0
  ans1 <- 0
  helper <- ((output_subjects[, m_i])^2)
  ans1 <- sum(helper)

  helper <- 0
  helper <- 1/(output_subjects[, m_i])
  ans2 <- sum(helper)

  ev_var_d <- ((1- (1/n_obs))*wsv)+((1-(ans1/(n_obs^2)))*bsv)

  if (bias_mod) {
    var_bias <- ((1/(n^2))*ans2*wsv)+(bsv/n)
  } else {
    var_bias <- (wsv/n_obs)+((ans1/(n_obs^2))*bsv)
  }

  var_sd_d <- var_var_d/(4*ev_var_d)

  z <- qnorm(beta/2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  var_loa <- var_bias+((z^2)*var_sd_d)

}