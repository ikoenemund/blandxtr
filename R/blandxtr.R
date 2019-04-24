#' \code{blandxtr}: A package for advanced Bland Altman analysis
#'
#' @docType package
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @import data.table
#' @import ggplot2
#'
#' @aliases blandxtr-package
#'
"_PACKAGE"

#' @title Main method for blandxtr
#'
#' @description \code{blandxtr} performs modified Bland Altman-analysis as
#' proposed by Olofsen et al. (2015). Uses the following functions from
#' blandxtr-package: \code{basicVariables}, \code{var.tvv}, \code{loa},
#' \code{var.loa}, \code{ci.loa.ba} and \code{ci.loa.mover}.
#'
#' @author Inga Koenemund \email{inga.koenemund(at)web.de}
#'
#' @param bt number of bootstrap samples (no bootstrapping if bt <= 0)
#' @param input_dt data.table with input dataset
#' @param bias_mod set TRUE for modified calculation of bias (small wsv) and
#' its variance, set FALSE for standard calculation of bias (small bsv) and
#' its variance
#' @param alpha for 100*(1-alpha)\%-confidence interval around LoA
#' @param beta for 100*(1-beta)\%-confidence interval around bias
#'
#' @note \code{bias_mod} is automatically set TRUE for
#' different number of measurements in each subject (unbalanced case)
#' @note "_mod" labels results based on modified true value varies-method
#' @note Bootstrapping affects runtime severely. Set bt<=0
#' if you want to skip bootstrapping.
#'
#' @return A list (blandxtr S3 object) containing the return values of all used
#' functions and a report showing the main results (as pdf/ LaTex).
#' @export

blandxtr <- function(input_dt, bt, bias_mod, alpha, beta){

  # -----------------------------------------
  # check input
  # if (!(is.data.table(input_dt)))
  #   stop("'input_dt' is not a data.table.")
  if (!(all.equal(bt, as.integer(bt))))
    stop("'bt' is not an integer.")
  if(bt<0) {
    warning("'bt' has been given a negative value.
    It has been automatically set 0 and
    bootstrapping has been skipped.")
    bt <- 0
  }
  if (!(is.logical(bias_mod)))
    stop("'bias_mod' is not logical.")

  # -----------------------------------------
  # prepare input data for analysis
  input_dt <- blandxtr_prepareData(input_dt)

  # -----------------------------------------

  pre <- blandxtrMain_pre (input_dt, bt, bias_mod, beta)
  ci <- blandxtr_ci(bt, input_dt, bias_mod, pre$bv, pre$var_tvv, pre$loa, pre$loa_mod,
    pre$var_loa, pre$var_loa_mod, alpha, beta)

  res <- c(pre, ci)

  tab <- blandxtr_results_table(res, bt, bias_mod, alpha, beta)
  fig <- blandxtr_results_plot(res)

  # -----------------------------------------
  # # create report (in latex) as pdf file
  # library(knitr)
  # setwd('./report/')
  # options(tinytex.verbose = TRUE)
  # knit2pdf(input = "report.blandxtr.Rnw")
  # setwd('..')

  # # create report (in markdown) as html file
  # output_format <- "html"
  #
  # renderMyReport <- function(res, tab, fig) {
  #   setwd('./report/')
  #   rmarkdown::render("blandxtr-report.Rmd",
  #     output_format = output_format,
  #     params = list(res = res, tab = tab, fig = fig))
  #   setwd('..')
  # }
  # renderMyReport(res, tab, fig)

  # -----------------------------------------

  result <-
    list(
      res = res,
      tab = tab,
      fig = fig
    )
  # class definition
  class(result) <- c("blandxtr", class(result))
  return(result)
}
