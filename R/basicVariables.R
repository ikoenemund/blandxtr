#' @title Basic variables
#'
#' @description \code{basicVariables} returns basic variables of an input
#' data.table containing three columns (subject, measurementX and measurementY).
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param dt input data.table
#' @return A list with the following elements is returned
#'  \item{outputMeasurements}{data.table with}
#'  \itemize{
#'      \item{\code{subject}} {subject ID}
#'      \item{\code{measurementX}} {results of measurement with X}
#'      \item{\code{measurementY}} {results of measurement with Y}
#'      \item{\code{d_ij}} {difference of each pair of measurement}
#'      \item{\code{m_ij}} {mean of each pair of measurement}
#'  }
#'  \item{outputSubjects}{data.table with}
#'  \itemize{
#'      \item{\code{subject}} {subject ID}
#'      \item{\code{m_i}} {number of measurements of each subject}
#'      \item{\code{d_i}} {mean of differences for each subject}
#'      \item{\code{var_d_i}} {variance of d_i for each subject}
#'  }
#'   \item{n}{number of subjects}
#'   \item{n_obs}{number of observations}
#'   \item{d}{mean of all differences}
#'   \item{d_a}{modified mean of all differences}
#'   \item{mean_x}{mean of all measurements with X}
#'   \item{mean_y}{mean of all measurements with Y}
#'   \item{rep_coeff}{repeatability coefficients}
#'

basicVariables <- function(dt){

  library(data.table)

  # ----------------------------
  start_time <- Sys.time()
  # some preparation

  # copy input data for modification as output
  outputMeasurements <- data.table(dt)
  # add measurement IDs in outputMeasurements
  outputMeasurements[, measurement_id:= rowid(outputMeasurements$subject)]

  outputSubjects <- dt[, .(.N), by = .(subject)]
  # rename column
  setnames(outputSubjects,"N", "m_i")

  end_time <- Sys.time()
  time_prep <- end_time - start_time
  # ----------------------------

  # number of subjects (n)
  n <- uniqueN(dt, by="subject")

  # ----------------------------

  # total number of observations (pairs of values, n_obs)
  n_obs <-nrow(dt)

  # ----------------------------

  # difference_ij for all measurements
  outputMeasurements[, d_ij := (measurementX-measurementY)]

  # ----------------------------

  # mean_ij for all measurements
  outputMeasurements[, m_ij := (measurementX+measurementY)/2]

  # ------------------------------

  start_time <- Sys.time()
  # all subjects (each subject):
  # mean of differences between measurements (each subject)
  ans <- outputMeasurements[, mean(d_ij), by = .(subject)]
  setnames(ans,"V1", "d_i")

  outputSubjects <- merge(ans, outputSubjects, by="subject")
  rm(ans)

        # TO DO: make nicer (above), some ideas:
        # works
        # outputSubjects[, mean_diff:=outputMeasurements[, .(mean(difference_mk)), by = .(subject)]]
        # test
        #outputSubjects[, mean_diff:=[, outputMeasurements[, .(mean(difference_mk)), by =.(subject)]]

  end_time <- Sys.time()
  time_d_i <- end_time - start_time
  # -------------------------------

  # variance of differences for each subject
  # TO DO: leave out? (not needed), make nicer (sum)

  # start_time <- Sys.time()
  # ### TEST
  # ans2 <- 0
  # ans3 <- 0
  # helper <- 0
  # helper <- setDT(outputMeasurements, key = "subject")[outputSubjects, d_i := i.d_i]
  # setkey(outputMeasurements, NULL)
  # helper[, ans := (d_ij-d_i)^2]
  #
  # ans2 <- helper[, sum(ans), by = .(subject)]
  # setnames(ans2,"V1", "ans2")
  # outputMeasurements[, ans:=NULL]
  # ans3 <- merge(ans2, outputSubjects, by="subject")
  # ans3[, var_d_i := ans2/(m_i-1)]
  # outputSubjects <- merge(ans3[, c("subject", "var_d_i")], outputSubjects, by="subject")
  #
  # rm(helper, ans2, ans3)
  # ###
  #
  # end_time <- Sys.time()
  # time_var_d_i <- end_time - start_time

  # # # TO DO: leave out? (not needed), make nicer (sum)
  # # # i: number of subject
  # #
  # # variance_diff <- function(i) {
  # #   d_i <- outputSubjects[subject == i,
  # #     d_i]
  # #   m_i <- outputSubjects[subject == i,
  # #     m_i]
  # #   ans <- (outputMeasurements[subject == i,
  # #     d_ij]-d_i)^2
  # #
  # #   var_d_i <- sum(ans)/(m_i-1)
  # # }
  # #
  # # start_time <- Sys.time()
  # # # variance of differences for each subject (calculation for all subjects)
  # # ans <- outputSubjects[, variance_diff(subject), by = .(subject)]
  # # setnames(ans,"V1", "var_d_i")
  # #
  # # outputSubjects <- merge(ans, outputSubjects, by="subject")
  # # rm(ans)
  # # rm(variance_diff)
  #
  # -------------------------------------

  # mean of all differences/ bias (D/ B)
  d <- mean(outputMeasurements[, d_ij])

  # -------------------------------------

  # alternative mean of all differences/ bias (D_a/ B_a)
  d_a <- mean(outputSubjects[, d_i])

  # -------------------------------------

  # mean of measurementX
  mean_x <- mean(outputMeasurements[, measurementX])

  # -------------------------------------

  # mean of measurementY
  mean_y <- mean(outputMeasurements[, measurementY])

  # -------------------------------------

  # residuals (r_ij = d_ij - d_i) using left-join
  helper <- 0
  helper <- setDT(outputMeasurements, key = "subject")[outputSubjects, d_i := i.d_i]
  setkey(outputMeasurements, NULL)
  outputMeasurements[, r_ij := helper$d_ij - helper$d_i]
  # outputMeasurements[, d_i:=NULL]
  rm(helper)

  # -------------------------------------
  # repeatability coefficients
  start_time <- Sys.time()
  # source("R/blandxtr.repeatability.R")
  # rep_coeff <- calc_repeat(mean_x, mean_y, dt$measurementX, dt$measurementY)

  ans <- 0
  ans <- sum((dt$measurementX-mean_x)^2)
  mssr_x <- (1/(length(dt$measurementX)-1))*ans
  rm(ans)

  ans <- 0
  ans <- sum((dt$measurementY-mean_y)^2)
  mssr_y <- (1/(length(dt$measurementY)-1))*ans
  rm(ans)

  s_x <- sqrt(mssr_x)
  s_y <- sqrt(mssr_y)
  s_x_s_y <- s_x/s_y

  rep_coeff <- list(
    mssr_x = mssr_x,
    mssr_y = mssr_y,
    s_x = s_x,
    s_y = s_y,
    s_x_s_y = s_x_s_y
  )
  end_time <- Sys.time()
  time_repeat <- end_time - start_time
  # -------------------------------------
  return(
    list(
      outputMeasurements = outputMeasurements,
      outputSubjects = outputSubjects,
      n = n,
      n_obs = n_obs,
      d = d,
      d_a = d_a,
      mean_x = mean_x,
      mean_y = mean_y,
      rep_coeff = rep_coeff,

      time_prep = time_prep,
      time_d_i = time_d_i,
      time_repeat = time_repeat
      # time_var_d_i = time_var_d_i
    )
  )
}
