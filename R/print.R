#' @title Print main results of advanced Bland Altman-analysis
#'
#' @description \code{print.blandxtr} prints main results of
#' advanced Bland Altman-analysis performed with \code{blandxtr} to the console
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
  if(x$bias_alt){
    cat("Main results from advanced Bland Altman-analysis: \n
    Using standard true value varies method \n
    Bias: ", x$bv$d_a, "(SE: ", x$var_loa$se_d, ")\n",
      sprintf("Limits of agreement: [%.2f, %.2f]", x$loa$loa_l, x$loa$loa_u),
    "\n\n  (Using modified true value varies method) \n
    Bias: ", x$bv$d_a, "(SE: ", x$var_loa_mod$se_d, ")\n",
      sprintf("Limits of agreement: [%.2f, %.2f]", x$loa_mod$loa_l,
        x$loa_mod$loa_u))
    invisible(x)
  } else {
    cat("Main results from advanced Bland Altman-analysis: \n
    Using standard true value varies method \n
    Bias: ", x$bv$d, "(SE: ", x$var_loa$se_d, ")\n",
      sprintf("Limits of agreement: [%.2f, %.2f]", x$loa$loa_l, x$loa$loa_u),
      "\n (Using modified true value varies method) \n
    Bias: ", x$bv$d, "(SE: ", x$var_loa_mod$se_d, ")\n",
      sprintf("Limits of agreement: [%.2f, %.2f]", x$loa_mod$loa_l,
        x$loa_mod$loa_u))
    invisible(x)
  }

}
