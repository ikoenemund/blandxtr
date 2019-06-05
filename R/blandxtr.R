#' \code{blandxtr}: A package for advanced Bland Altman-analysis
#'
#' @docType package
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @import data.table
#' @import ggplot2
#' @import checkmate
#' @import knitr
#' @import rmarkdown
#' @importFrom stats na.omit qchisq qnorm quantile rnorm
#'
#' @aliases blandxtr-package
#'
"_PACKAGE"

#' @title Main method for blandxtr-package
#'
#' @description \code{blandxtr} performs advanced Bland Altman-analysis as
#' proposed by Olofsen et al. (2015). Uses the following functions from
#' blandxtr-package: \code{basic_variables}, \code{var_tvv}, \code{loa},
#' \code{var_loa}, \code{ci_loa_ba},  \code{ci_loa_bt} and \code{ci_loa_mover}.
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param bt number of bootstrap samples (no bootstrapping if bt <= 0)
#' @param input_data data.frame or data.table with input dataset
#' @param bias_alt set TRUE for alternative calculation of bias (small
#' within-subject variance) and its variance, set FALSE for standard calculation
#' of bias (small between-subjects variance) and its variance
#' @param alpha for 100*(1-alpha)\%-confidence interval around LoA
#' (default: 0.05)
#' @param beta for 100*(1-beta)\%-confidence interval around bias
#' (default: 0.05)
#'
#' @note '_mod' labels results based on modified analysis of variance
#' @note Bootstrapping affects runtime severely. Set bt<=0
#' if you want to skip bootstrapping.
#'
#' @return A list (blandxtr S3 object) containing the return values of all used
#' functions.
#' @export

blandxtr <- function(input_data, bt, bias_alt, alpha, beta){

  # -----------------------------------------
  # check input

  if (missing(alpha)) {
    alpha <- 0.05
    warning("Variable `alpha` is missing. Setting to 0.05.")
  }
  if (missing(beta)) {
    beta <- 0.05
    warning("Variable `beta` is missing. Setting to 0.05.")
  }

  if(bt<0) {
    warning("'bt' has been given a negative value.
      It has been automatically set 0 and
      bootstrapping has been skipped.")
    bt <- 0
  }

  coll <- checkmate::makeAssertCollection()
  checkmate::assert_data_table(input_data, add = coll)
  checkmate::assert_int(bt, add = coll)
  checkmate::assert_logical(bias_alt, add = coll)
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, add = coll)
  checkmate::assert_numeric(beta, lower = 0, upper = 1, add = coll)
  checkmate::reportAssertions(coll)

  # -----------------------------------------
  # prepare input data for analysis
  input_dt <- prepare_data(input_data)

  # -----------------------------------------
  # use 'main_pre'- and 'main_ci'-function for calling all further functions
  # necessary for analysis

  pre <- main_pre (input_dt, bt, bias_alt, beta)
  ci <- main_ci(bt, input_dt, bias_alt, pre$bv, pre$var_tvv, pre$loa,
    pre$loa_mod, pre$var_loa$var_loa, pre$var_loa_mod$var_loa, alpha, beta)

  res <- c(bt = bt, bias_alt = bias_alt, alpha = alpha, beta = beta, pre, ci)

  # -----------------------------------------
  # class definition
  class(res) <- c("blandxtr", class(res))
  return(res)
}

#' Reports whether x is a blandxtr object
#' @param x An object to test
#' @keywords internal
#' @export

is.blandxtr <- function(x) inherits(x, "blandxtr")

# -----------------------------------------
# CRAN note avoidance
utils::globalVariables(c("measurement_id", ".", "subject", "d_ij",
  "measurement_x", "measurement_y", "m_ij", "d_i", "r_ij", "m_i", "bt",
  "i.d_i", "coll", "plot"))
