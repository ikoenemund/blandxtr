#' @title Print main results of modified Bland Altman-analysis
#'
#' @description \code{print.blandxtr} prints main results of
#' modified Bland Altman-analysis performed with \code{blandxtr} to the console
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param x list with results from \code{blandxtr}
#' @param ... other arguments not used by this method
#'
#' @method print blandxtr
#'
#' @export

print.blandxtr <- function (x, ...) {
  cat("Main results from modified Bland Altman-analysis: \n
    Bias: ", x$bv$d, "(SE: ", x$bv$d, ")\n",
    sprintf("Limits of agreement: [%.2f, %.2f]", x$loa$loa_l, x$loa$loa_u))
  invisible(x)
}
