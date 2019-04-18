#' @title Analysis of variance
#'
#' @description \code{calc_var_tvv} executes analysis of variance for
#' quantities measured with true value varying
#' (includes standard and modified version)
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param n number of subjects
#' @param n_obs number of observations
#' @param d mean of all differences
#' @param d_a modified mean of all differences
#' @param outputSubjects data.table containing subject ID and
#' number of measurements of each subject (m_i)
#' @param outputMeasurements data.table containing
#'
#' @return A list with the following elements is returned
#' \itemize{
#'  \item{\code{wsv}} {within-subjects variance}
#'  \item{\code{bsv}} {between-subjects variance}
#'  \item{\code{bsv_mod}} {modified between-subjects variance}
#'  \item{\code{wsv_mod}} {modified within-subjects variance}
#'  \item{\code{var_d}} {variance of mean of all differences}
#'  \item{\code{sd_d}} {standard deviation of all differences}
#'  \item{\code{var_var_d}} {variance of the variance of mean
#'  of all differences}
#'  \item{\code{var_d_mod}} {modified variance of mean of all differences}
#'  \item{\code{sd_d_mod}} {modified standard deviation of all differences}
#'  \item{\code{var_var_d_mod}} {modified variance of the variance of
#'  mean of all differences}
#'  \item{\code{tau}} {if \code{tau} < 1/3 use unmodified analysis of variance,
#'  if \code{tau} > 1/3 look at \code{tau_mod}}
#'  \item{\code{tau_mod}} {look at \code{tau} first, if \code{tau_mod} < 1/3 use
#'  unmodified analysis of variance, if \code{tau_mod} > 1/3 use
#'  modified analysis of variance}
#'  \item{\code{se_sd_d}} {standard error of sd of all differences}
#'  \item{\code{se_sd_d_mod}} {standard error of modified sd of all differences}
#'  \item{\code{se_bsv}} {standard error of between-subjects variance}
#'  \item{\code{se_bsv_mod}} {standard error of modified between-subjects
#'  variance}
#'  \item{\code{se_wsv}} {standard error of within-subjects variance}
#'  \item{\code{se_wsv_mod}} {standard error of modified within-subjects
#'  variance}
#' }
#'
#' @export

calc_var_tvv <- function (n, n_obs, d, d_a, outputSubjects, outputMeasurements){

  # -------------------------------------
  # standard tvv
  # -------------------------------------
  # lambda
  helper <- 0
  ans <- 0
  helper <- (outputSubjects[, m_i])^2
  ans <- sum(helper)

  lambda <- ((n_obs^2)-ans)/((n-1)*n_obs)
  rm(ans, helper)

  # -------------------------------------
  # within subject-variance (wsv) based on mssr
  # mssr

  ans <- sum((outputMeasurements$d_ij - outputMeasurements$d_i)^2)
  outputMeasurements[, d_i:=NULL]

  mssr <- (1/(n_obs-n))*ans
  rm(ans)

  # within subject-variance (wsv)
  wsv <- mssr

  # -------------------------------------
  # between subject-variance (bsv) based on mssi
  # mssi

  helper <- 0
  ans <- 0
  helper <- (outputSubjects[, m_i])*(((outputSubjects[, d_i])-d)^2)
  ans <- sum(helper)
  rm(helper)

  mssi <- (1/(n-1))*ans
  rm(ans)

  # between subject-variance (bsv)

  bsv <- (mssi-wsv)/lambda

  # -------------------------------------
  # variance of all differences (var_d)

  var_d <- ((1-(1/lambda))*mssr)+((1/lambda)*mssi)

  # -------------------------------------
  # standard deviation of all differences (sd_d)

  sd_d <- sqrt (var_d)

  # -------------------------------------
  # variance of variance of differences

  var_var_d <- ((2*(((1-(1/lambda))*wsv)^2))/(n_obs-n)) + ((2*(((wsv/lambda)+bsv)^2))/(n-1))

  # -------------------------------------
  # modified tvv
  # -------------------------------------
  # alternative lambda

  helper <- 0
  ans <- 0
  helper <- 1/(outputSubjects[, m_i])
  ans <- sum(helper)

  lambda_mod <- (1/n)*ans
  rm(ans)

  # -------------------------------------
  # modified within subject-variance (wsv_mod)
  # no changes (compared to standard tvv)

  mssr_mod <- mssr
  wsv_mod <- wsv

  # -------------------------------------
  # modified between subject-variance (bsv_mod) based on mssi_mod
  # mssi_mod

  helper <- 0
  ans <- 0
  helper <- ((outputSubjects[, d_i])-d_a)^2
  ans <- sum(helper)
  rm(helper)

  mssi_mod <- (1/(n-1))*ans

  rm(ans)

  # modified between subject-variance (bsv)

  bsv_mod <- (mssi_mod - (lambda_mod*mssr_mod))

  # -------------------------------------
  # modified variance of all differences (var_d_mod)

  var_d_mod <- ((1-lambda_mod)*mssr)+mssi_mod

  # -------------------------------------
  # modified standard deviation of all differences (sd_d_mod)

  sd_d_mod <- sqrt (var_d_mod)

  # -------------------------------------
  # modified variance of variance of differences

  var_var_d_mod <- ((2*(((1-lambda_mod)*wsv)^2))/(n_obs-n)) + ((2*(((wsv*lambda_mod)+bsv_mod)^2))/(n-1))

  # -------------------------------------
  # tau
  tau <- (bsv)/(bsv + wsv)

  # -------------------------------------
  # tau_mod
  tau_mod <- (bsv_mod)/(bsv_mod + wsv_mod)

  # -------------------------------------
  # standard error of standard deviation of differences

  v1 <- ((mssr/lambda+bsv)^2)/(n-1)
  v2 <- (((1-(1/lambda))*mssr)^2)/(n_obs-n)
  vrv <- v1 + v2
  vsd <- vrv/2/var_d
  se_sd_d <- sqrt(vsd)

  rm(vrv, vsd)

  # modified
  v1_mod <- ((lambda_mod*mssr+bsv_mod)^2)/(n-1)
  v2_mod <- (((1-lambda_mod)*mssr_mod)^2)/(n_obs-n)
  vrv <- v1_mod + v2_mod
  vsd <- vrv/2/var_d_mod
  se_sd_d_mod <- sqrt(vsd)

  rm(v1, v2, v1_mod, v2_mod, vrv, vsd)

  # -------------------------------------
  # standard error of between subject variance (bsv)
  vmssr <- 2*(mssr^2)/(n_obs-n)
  vvrr <- (2*((mssr+(lambda*bsv))^2)/(n-1) + vmssr)/(lambda^2)
  se_bsv <- sqrt(vvrr)
  rm(vvrr)

  # modified
  vmssr_mod <- 2*(mssr_mod^2)/(n_obs-n)
  v1 <- ((lambda_mod*mssr+bsv_mod)^2)/(n-1)
  vvrr_mod <- (2*v1) + (lambda_mod^2)*vmssr_mod
  se_bsv_mod <- sqrt(vvrr_mod)
  rm(v1, vvrr_mod)

  # -------------------------------------
  # standard error of within subject variance (wsv)
  se_wsv <- sqrt(vmssr)
  se_wsv_mod <- sqrt(vmssr_mod)

  rm(vmssr, vmssr_mod)

  return(
    list(
      wsv = wsv,
      bsv = bsv,
      bsv_mod = bsv_mod,
      wsv_mod = wsv_mod,
      var_d = var_d,
      sd_d = sd_d,
      var_var_d = var_var_d,
      var_d_mod = var_d_mod,
      sd_d_mod = sd_d_mod,
      var_var_d_mod = var_var_d_mod,
      tau = tau,
      tau_mod = tau_mod,
      mssi_mod = mssi_mod,

      se_sd_d = se_sd_d,
      se_sd_d_mod = se_sd_d_mod,

      se_bsv = se_bsv,
      se_bsv_mod = se_bsv_mod,

      se_wsv = se_wsv,
      se_wsv_mod = se_wsv_mod
    )
  )
}
