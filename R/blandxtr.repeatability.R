#' @title Calculation of residuals
#'
#' @description \code{calc_repeat} returns repeatability coefficients.
#' Calculation is based on one-way analysis of variance with subject
#' as the factor as proposed by Bland and Altman (1999).
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param mean_x mean of all measurements with X
#' @param mean_y mean of all measurements with Y
#' @param measurementX results of measurement with X
#' @param measurementY results of measurement with Y
#'
#' @return Repeatability coefficients (SX, SY, SX/SY)
#'
calc_repeat <- function (mean_x, mean_y, measurementX, measurementY){

  x <- 0
  for (i in 1:length(measurementX)){
    x <- x + (measurementX[i]-mean_x)^2
  }
  mssr_x <- (1/(length(measurementX)-1))*x
  rm(i, x)

  y <- 0
  for (i in 1:length(measurementY)){
    y <- y + (measurementY[i]-mean_y)^2
  }
  mssr_y <- (1/(length(measurementY)-1))*y
  rm(i, y)

  s_x <- sqrt(mssr_x)
  s_y <- sqrt(mssr_y)
  s_x_s_y <- s_x/s_y

  return(
    list(
      s_x = s_x,
      s_y = s_y,
      s_x_s_y = s_x_s_y
    )
  )

}
