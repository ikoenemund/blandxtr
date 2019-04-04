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
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param n number of subjects
#' @param n_obs number of observations
#' @param bsv between-subject variance
#' @param wsv within-subject variance
#' @param outputSubjects data.table containing subject ID and
#' number of measurements of each subject (m_i)
#' @param var_var_d variance of the variance of mean of all differences
#' @param biasMod set FALSE for standard calculation of the variance (small bsv),
#' set TRUE for modified calculation of the variance (small wsv)
#'
#' @return \code{var_loa} variance of limits of agreement
#'

calc_var_loa <- function (n, n_obs, bsv, wsv, outputSubjects, var_var_d,
  biasMod, beta){

  ### TEST
  ans1 <- 0
  ans2 <- 0
  helper <- 0
  ans1 <- 0
  helper <- ((outputSubjects[, m_i])^2)
  ans1 <- sum(helper)

  helper <- 0
  helper <- 1/(outputSubjects[, m_i])
  ans2 <- sum(helper)
  ###

  # ans1 <- 0
  # ans2 <- 0
  # for(i in 1:n) {
  #   m_i <- outputSubjects[subject == i,
  #     m_i]
  #   ans1 <- ans1 + (m_i^2)
  #   ans2 <- ans2 + (1/m_i)
  # }

  ev_var_d <- ((1- (1/n_obs))*wsv)+((1-(ans1/(n_obs^2)))*bsv)

  if (biasMod) {
    var_bias <- ((1/(n^2))*ans2*wsv)+(bsv/n)
  } else {
    var_bias <- (wsv/n_obs)+((ans1/(n_obs^2))*bsv)
  }

  var_sd_d <- var_var_d/(4*ev_var_d)

  z <- qnorm(beta/2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  var_loa <- var_bias+((z^2)*var_sd_d)

}

# ---------------------------
# # variance of mean of all differences (bias)
# ans <- 0
# calc_var_d <- function (n, n_obs, bsv, wsv, outputSubjects){
#   for(i in 1:n) {
#     m_i <- outputSubjects[subject == i,
#       m_i]
#     ans <- ans + (m_i^2)
#   }
#   var_d <- (wsv/n_obs)+((ans/(n_obs^2))*bsv)
# }
#
# # ---------------------------
# # alternative variance of mean of all differences (bias)
# ans <- 0
# calc_var_d_mod <- function (n, bsv, wsv, outputSubjects){
#   for(i in 1:n) {
#     m_i <- outputSubjects[subject == i,
#       m_i]
#     ans <- ans + (1/m_i)
#   }
#   var_d_mod <- ((1/(n^2))*ans*wsv)+(bsv/n)
# }
#
# # ---------------------------
# # variance of the standard deviation of the differences
#
# calc_var_sd_d <- function (var_var_d, ev_var_d){
#   var_sd_d <- var_var_d/(4*ev_var_d)
# }
#
# # ---------------------------
# # variance of limits of agreement (var_loa)
#
# calc_var_loa <- function(var_d, var_sd_d){
#   var_loa <- var_d+((1.96^2)*var_sd_d)
# }

