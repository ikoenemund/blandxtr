#' blandxtr: A package for advanced Bland Altman analysis
#'
#' @docType package
#' @author Inga Koenemund \email{inga.koenemund(at)web.de}
#'
#' @import data.table
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
#' @param biasMod set TRUE for modified calculation of bias (small wsv) and
#' its variance, set FALSE for standard calculation of bias (small bsv) and
#' its variance
#' @param alpha for 100*(1-alpha)\%-confidence interval around LoA
#' @param beta for 100*(1-beta)\%-confidence interval around bias
#'
#' @note \code{biasMod} is automatically set TRUE for
#' different number of measurements in each subject (unbalanced case)
#' @note "_mod" labels results based on modified true value varies-method
#' @note Bootstrapping affects runtime severely. Set bt<=0
#' if you want to skip bootstrapping.
#'
#' @return A list (blandxtr S3 object) containing the return values of all used
#' functions and a report showing the main results (as pdf/ LaTex).

blandxtr <- function(input_dt, bt, biasMod, alpha, beta){

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
  if (!(is.logical(biasMod)))
    stop("'biasMod' is not logical.")

  # -----------------------------------------
  # prepare input data for analysis
  source("R/blandxtr.prepareData.R")
  input_dt <- blandxtr_prepareData(input_dt)

  # -----------------------------------------
  source("R/blandxtrMain.pre.R")
  source("R/blandxtr.ci.R")

  source("R/blandxtr.results.plot.R")
  source("R/blandxtr.results.table.R")

  pre <- blandxtrMain_pre (input_dt, bt, biasMod, beta)
  ci <- blandxtr_ci(bt, input_dt, biasMod, pre$bv, pre$var_tvv, pre$loa, pre$loa_mod,
    pre$var_loa, pre$var_loa_mod, alpha, beta)

  res <- c(pre, ci)

  tab <- blandxtr_results_table(res, bt, biasMod, alpha, beta)
  fig <- blandxtr_results_plot(res)

  # -----------------------------------------
  # create report (in latex) as pdf file
  library(knitr)
  setwd('./report/')
  options(tinytex.verbose = TRUE)
  knit2pdf(input = "report.blandxtr.Rnw")
  setwd('..')

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
