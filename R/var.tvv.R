#' @title Analysis of variance
#'
#' @description \code{calc_var_tvv} executes analysis of variance for
#' quantities measured with true value varying
#' (includes standard and modified version)
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
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
#' }
#'

calc_var_tvv <- function (n, n_obs, d, d_a, outputSubjects, outputMeasurements){

  # -------------------------------------
  # standard tvv
  # -------------------------------------

  # lambda
  ans <- 0
  for(i in 1:n) {
    m_i <- outputSubjects[subject == i,
      m_i]
    ans <- ans + (m_i^2)
  }

  lambda <- ((n_obs^2)-ans)/((n-1)*n_obs)
  rm(ans)

  # -------------------------------------
  # within subject-variance (wsv) based on mssr

  # mssr
  ans <- 0
  for(i in 1:n) {
    m_i <- outputSubjects[subject == i,
      m_i]
    d_i <- outputSubjects[subject == i,
      d_i]
    for(j in 1:m_i) {
      d_ij <- outputMeasurements[subject == i & measurement_id==j,
        d_ij]
      ans <- ans + (d_ij-d_i)^2
    }
  }

  mssr <- (1/(n_obs-n))*ans
  rm(ans)

  # within subject-variance (wsv)
  wsv <- mssr

  # -------------------------------------

  # between subject-variance (bsv) based on mssi

  # mssi
  ans <- 0
  for(i in 1:n) {
    d_i <- outputSubjects[subject == i,
      d_i]
    m_i <- outputSubjects[subject == i,
      m_i]
    ans <- ans + m_i*((d_i-d)^2)
  }

  mssi <- (1/(n-1))*ans
  rm(ans)
  # between subject-variance (bsv)

  bsv <- (mssi-wsv)/lambda

  # -------------------------------------

  # variance of all differences (var_d)

  var_d <- ((1-(1/lambda))*mssr)+((1/lambda)*mssi)

  # TO DO: create test (see if formula above is equal to following, change text if not!)
  # var_d <- bsv + wsv

  # -------------------------------------

  # standard deviation of all differences (sd_d)

  sd_d <- sqrt (var_d)

  # -------------------------------------

  # variance of variance of differences

  var_var_d <- ((2*(((1-(1/lambda))*wsv)^2))/(n_obs-n)) + ((2*(((wsv/lambda)+bsv)^2))/(n-1))
  rm(i, j, d_i, d_ij, m_i)

  # -------------------------------------
  # modified tvv
  # -------------------------------------
  # alternative lambda
  ans <- 0
  for(i in 1:n) {
    m_i <- outputSubjects[subject == i,
      m_i]
    ans <- ans + (1/m_i)
  }

  lambda_mod <- (1/n)*ans
  rm(ans, i, m_i)

  # -------------------------------------
  # modified between subject-variance (bsv_mod) based on mssi_mod

  # mssi_mod

  ans <- 0
  for(i in 1:n) {
    d_i <- outputSubjects[subject == i,
      d_i]
    m_i <- outputSubjects[subject == i,
      m_i]
    ans <- ans + ((d_i-d_a)^2)
  }

  mssi_mod <- (1/(n-1))*ans

  rm(ans, i, d_i, m_i)

  # modified between subject-variance (bsv)

  bsv_mod <- mssi_mod

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
  return(
    list(
      wsv = wsv,
      bsv = bsv,
      bsv_mod = bsv_mod,
      wsv_mod = wsv,
      var_d = var_d,
      sd_d = sd_d,
      var_var_d = var_var_d,
      var_d_mod = var_d_mod,
      sd_d_mod = sd_d_mod,
      var_var_d_mod = var_var_d_mod
    )
  )
}
