#' @title Limits of agreement (LoA)
#'
#' @description \code{calc_loa} returns limits of agreement (LoA)
#' based on a method proposed by Bland and Altman (1999).
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param d mean of all differences
#' @param sd_d standard deviation of d
#'
#' @return A list with the following elements is returned
#' \itemize{
#'  \item{\code{loa_l}} {lower limit of agreement}
#'  \item{\code{loa_u}} {upper limit of agreement}
#' }
#'

# ---------------------------
# limits of agreement (loa)
 calc_loa <- function(d, sd_d){

   # lower
  loa_l <- d-(1.96*sd_d)

   # upper
  loa_u <- d+(1.96*sd_d)

   return(
     list(
       loa_l = loa_l,
       loa_u = loa_u
     )
   )
 }


# # ---------------------------
# # expected value (ev) of variance of all differnces (var_d)
# ans <- 0
# calc_ev_var_d <- function (n, n_obs, bsv, wsv, outputSubjects){
#   for(i in 1:n) {
#     m_i <- outputSubjects[subject == i,
#       m_i]
#     ans <- ans + (m_i^2)
#   }
#   ev_var_d <- ((1- (1/n_obs))*wsv)+((1-(ans/(n_obs^2)))*bsv)
# }
#
# # ---------------------------
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
# calc_var_d_a <- function (n, bsv, wsv, outputSubjects){
#   for(i in 1:n) {
#     m_i <- outputSubjects[subject == i,
#       m_i]
#     ans <- ans + (1/m_i)
#   }
#   var_d_a <- ((1/(n^2))*ans*wsv)+(bsv/n)
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
