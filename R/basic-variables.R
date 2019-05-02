#' @title Basic variables
#'
#' @description \code{basic_variables} returns basic variables of an input
#' data.table containing three columns (subject, measurement_x and measurement_y).
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param dt input data.table
#' @return A list with the following elements is returned
#'  \item{output_measurements}{data.table with}
#'  \itemize{
#'      \item{\code{subject}} {subject ID}
#'      \item{\code{measurement_x}} {results of measurement with X}
#'      \item{\code{measurement_y}} {results of measurement with Y}
#'      \item{\code{d_ij}} {difference of each pair of measurement}
#'      \item{\code{m_ij}} {mean of each pair of measurement}
#'  }
#'  \item{output_subjects}{data.table with}
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
#' @export

basic_variables <- function(dt){

  # check input
  if (!(is.data.table(dt))){
    stop("'input_dt' is not a data.table.")
  }

  # ----------------------------
  # some preparation

  # copy input data for modification as output
  output_measurements <- data.table(dt)
  # add measurement IDs in output_measurements
  output_measurements[, measurement_id:= rowid(output_measurements$subject)]

  output_subjects <- dt[, .(.N), by = .(subject)]
  # rename column
  setnames(output_subjects,"N", "m_i")

  # ----------------------------

  # number of subjects (n)
  n <- data.table::uniqueN(dt, by="subject")

  # ----------------------------

  # total number of observations (pairs of values, n_obs)
  n_obs <- nrow(dt)

  # ----------------------------

  # difference_ij for all measurements
  output_measurements[, d_ij := (measurement_x-measurement_y)]

  # ----------------------------

  # mean_ij for all measurements
  output_measurements[, m_ij := (measurement_x+measurement_y)/2]

  # ------------------------------

  # all subjects (each subject):
  # mean of differences between measurements (each subject)
  ans <- output_measurements[, mean(d_ij), by = .(subject)]
  setnames(ans,"V1", "d_i")

  output_subjects <- merge(ans, output_subjects, by="subject")
  rm(ans)

  # -------------------------------

  # mean of all differences/ bias (D/ B)
  d <- mean(output_measurements[, d_ij])

  # -------------------------------------

  # alternative mean of all differences/ bias (D_a/ B_a)
  d_a <- mean(output_subjects[, d_i])

  # -------------------------------------

  # mean of measurement_x
  helper <- output_measurements[, mean(measurement_x), by = .(subject)]
  setnames(helper,"V1", "mean_x_helper")
  mean_x <- mean(helper$mean_x_helper)
  rm(helper)

  # -------------------------------------

  # mean of measurement_y
  helper <- output_measurements[, mean(measurement_y), by = .(subject)]
  setnames(helper,"V1", "mean_y_helper")
  mean_y <- mean(helper$mean_y_helper)
  rm(helper)

  # -------------------------------------

  # residuals (r_ij = d_ij - d_i) using left-join
  helper <- 0
  helper <- data.table::setDT(output_measurements, key = "subject")[output_subjects, d_i := i.d_i]
  setkey(output_measurements, NULL)
  output_measurements[, r_ij := helper$d_ij - helper$d_i]
  rm(helper)

  # -------------------------------------
  # repeatability coefficients

  ans <- 0
  ans <- dt[, mean(measurement_x), by = .(subject)]
  setnames(ans,"V1", "x_i")

  helper <- 0
  helper <- merge(dt, ans, by="subject")
  rm(ans)

  ans <- dt[, mean(measurement_y), by = .(subject)]
  setnames(ans,"V1", "y_i")
  helper <- merge(helper, ans, by="subject")
  rm(ans)

  ans1 <- sum((helper$measurement_x - helper$x_i)^2)
  ans2 <- sum((helper$measurement_y - helper$y_i)^2)
  rm(helper)

  mssr_x <- (1/(length(dt$measurement_x)-n))*ans1
  rm(ans1)

  mssr_y <- (1/(length(dt$measurement_y)-n))*ans2
  rm(ans2)

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
  # -------------------------------------
  return(
    list(
      output_measurements = output_measurements,
      output_subjects = output_subjects,
      n = n,
      n_obs = n_obs,
      d = d,
      d_a = d_a,
      mean_x = mean_x,
      mean_y = mean_y,
      rep_coeff = rep_coeff
    )
  )
}
