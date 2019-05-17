#' @title Basic variables
#'
#' @description \code{basic_variables} returns basic variables of an input
#' data.table containing three columns (subject, measurement_x and
#' measurement_y).
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
#'      \item{\code{measurement_id}} {measurement ID}
#'      \item{\code{d_ij}} {difference of each pair of measurements}
#'      \item{\code{m_ij}} {mean of each pair of measurements}
#'  }
#'  \item{output_subjects}{data.table with}
#'  \itemize{
#'      \item{\code{subject}} {subject ID}
#'      \item{\code{m_i}} {number of measurements of each subject}
#'      \item{\code{d_i}} {mean of differences for each subject}
#'  }
#'   \item{n}{number of subjects}
#'   \item{n_obs}{number of observations}
#'   \item{d}{mean of all differences}
#'   \item{d_a}{alternative mean of all differences}
#'   \item{mean_x}{mean of all measurements with X}
#'   \item{mean_x_a}{alternative mean of all measurements with X}
#'   \item{mean_y}{mean of all measurements with Y}
#'   \item{mean_y_a}{alternative mean of all measurements with Y}
#'   \item{param_rep_coeff}{list with parameters for calculation of
#'   repeatability coefficients}
#'
#' @note 'Alternative' means are calculated with mean for each subject
#' and then the overall mean of all subjects (every subject has the same impact
#' on the overall mean).
#'
#' @export

basic_variables <- function(dt){

  # ----------------------------
  # check input

  checkmate::assert_data_table(dt)

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

  # alternative mean of all differences/ bias (D_a/ B_a)
  d_a <- mean(output_subjects[, d_i])

  # -------------------------------------
  # mean of measurement_x
  mean_x <- mean(dt$measurement_x)

  # alternative mean of measurement_x
  ind_mean_x <- output_measurements[, mean(measurement_x), by = .(subject)]
  setnames(ind_mean_x,"V1", "ind_mean_x")
  mean_x_a <- mean(ind_mean_x$ind_mean_x)
  rm(ind_mean_x)

  # -------------------------------------
  # mean of measurement_y
  mean_y <- mean(dt$measurement_y)

  # alternative mean of measurement_y
  ind_mean_y <- output_measurements[, mean(measurement_y), by = .(subject)]
  setnames(ind_mean_y,"V1", "ind_mean_y")
  mean_y_a <- mean(ind_mean_y$ind_mean_y)
  rm(ind_mean_y)

  # -------------------------------------
  # calculate residuals (r_ij = d_ij - d_i) using left-join
  helper <- 0
  helper <- data.table::setDT(output_measurements, key = "subject")[
    output_subjects, d_i := i.d_i]
  setkey(output_measurements, NULL)
  output_measurements[, r_ij := helper$d_ij - helper$d_i]
  rm(helper)

  # -------------------------------------
  # parameters for calculation of repeatability coefficients

  # add mean of measurement_x for each subject (x_i) to copy of input_dt ('helper')
  ans <- 0
  ans <- dt[, mean(measurement_x), by = .(subject)]
  setnames(ans,"V1", "x_i")

  helper <- 0
  helper <- merge(dt, ans, by="subject")
  rm(ans)

  # add mean of measurement_y for each subject (y_i) to copy of input_dt ('helper')
  ans <- dt[, mean(measurement_y), by = .(subject)]
  setnames(ans,"V1", "y_i")
  helper <- merge(helper, ans, by="subject")
  rm(ans)

  # calculate square sums (necessary for calculation of mssr_x/ mssr_y)
  sq_sum_1 <- sum((helper$measurement_x - helper$x_i)^2)
  sq_sum_2 <- sum((helper$measurement_y - helper$y_i)^2)
  rm(helper)

  # mssr_x
  mssr_x <- (1/(length(dt$measurement_x)-n))*sq_sum_1
  rm(sq_sum_1)

  # mssr_y
  mssr_y <- (1/(length(dt$measurement_y)-n))*sq_sum_2
  rm(sq_sum_2)

  s_x <- sqrt(mssr_x)
  s_y <- sqrt(mssr_y)
  s_x_s_y <- s_x/s_y

  param_rep_coeff <- list(
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
      mean_x_a = mean_x_a,
      mean_y = mean_y,
      mean_y_a = mean_y_a,
      param_rep_coeff = param_rep_coeff
    )
  )
}
