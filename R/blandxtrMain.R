#' @title Main method for blandxtr
#'
#' @description \code{blandxtrMain} performs modified Bland Altman-analysis as
#' proposed by Olofsen et al. (2015). Uses the following functions from
#' blandxtr-package: \code{basicVariables}, \code{var.tvv}, \code{loa},
#' \code{var.loa}, \code{ci.loa.ba} and \code{ci.loa.mover}.
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param bt number of bootstrap samples (no bootstrapping if bt <= 0)
#' @param input_dt data.table with input dataset
#' @param biasMod set TRUE for modified calculation of bias (small wsv),
#' set FALSE for standard calculation of bias (small bsv)
#'
#' @note \code{biasMod} is automatically set TRUE for
#' different number of measurements in each subject (unbalanced case)
#' @note "_mod" labels results based on modified true value varies-method
#' @note Bootstrapping affects runtime severely. Set bt<=0
#' if you want to skip bootstrapping.
#'
#' @return A list containing the return values of all used functions.

blandxtrMain <- function(input_dt, bt, biasMod){

  # -----------------------------------------
  # check input
  if(bt<0) {
    warning("'bt' has been given a negative value.
    It has been automatically set 0 and
    bootstrapping has been skipped.")
    bt <- 0
  }
  if (!(is.data.table(input_dt)))
    stop("'input_dt' is not a data.table.")
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

  pre <- blandxtrMain_pre (input_dt, bt, biasMod)
  ci <- blandxtr_ci(bt, input_dt, biasMod, pre$bv, pre$var_tvv, pre$loa, pre$loa_mod,
    pre$var_loa, pre$var_loa_mod)

  res <- c(pre, ci)

  start_time <- Sys.time()
  tab <- blandxtr_results_table(res)
  fig <- blandxtr_results_plot(res)

  # -----------------------------------------
  # create report (in latex) as pdf file
  library(knitr)
  setwd('./report/')
  options(tinytex.verbose = TRUE)
  knit2pdf(input = "report.blandxtr.Rnw")
  setwd('..')

  end_time <- Sys.time()
  time_report <- end_time - start_time
  # -----------------------------------------
  return(
    list(
      # bv = pre$bv,
      # var_tvv = pre$var_tvv,
      # loa = pre$loa,
      # loa_mod = pre$loa_mod,
      # var_loa = pre$var_loa,
      # var_loa_mod = pre$var_loa_mod,
      # loa_ba = ci$loa_ba,
      # loa_ba_mod = ci$loa_ba_mod,
      # loa_mover = ci$loa_mover,
      # loa_mover_mod = ci$loa_mover_mod,
      # loa_bt = ci$loa_bt,
      # loa_bt_mod = ci$loa_bt_mod,
      res = res,
      tab = tab,
      fig = fig,
      time_report = time_report

    )
  )
}
