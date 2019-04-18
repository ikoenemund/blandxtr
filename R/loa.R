#' @title Limits of agreement (LoA)
#'
#' @description \code{calc_loa} returns limits of agreement (LoA)
#' based on a method proposed by Bland and Altman (1999).
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param d mean of all differences
#' @param sd_d standard deviation of d
#' @param beta for 100*(1-beta)\%-confidence interval around bias
#'
#' @return A list with the following elements is returned
#' \itemize{
#'  \item{\code{loa_l}} {lower limit of agreement}
#'  \item{\code{loa_u}} {upper limit of agreement}
#' }
#'
#' @export
#'

# ---------------------------
# limits of agreement (loa)
 calc_loa <- function(d, sd_d, beta){

   # lower
   z <- qnorm(1-beta/2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
   loa_l <- d-(z*sd_d)

   # upper
  loa_u <- d+(z*sd_d)
  rm(z)

   return(
     list(
       loa_l = loa_l,
       loa_u = loa_u
     )
   )
 }
