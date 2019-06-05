#' @title Preparation of data for Bland Altman analysis
#'
#' @description \code{prepare_data} checks input data.table for
#' common errors (missing columns that are required, wrong type of input data)
#' and reduces it to the columns necessary for analysis with
#' \code{blandxtr}.
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param input_data data.frame or data.table with input dataset
#'
#' @note Rows with missing values will be removed from analysis.
#'
#' @return Prepared input data as data.table
#'
#' @export
#'

# Check and prepare input data

prepare_data <- function (input_data){

  # -----------------------------------------
  # check input

  checkmate::assert_data_frame(input_data, add = coll)

  # -----------------------------------------
  # check columns

  if (!(any(colnames(input_data)=="subject"))){
    stop("Error in input dataset: No column named 'subject' found.")
  } else if (!(any(colnames(input_data)=="measurement_x"))) {
    stop("Error in input dataset: No column named 'measurement_x' found.")
  } else if (!(any(colnames(input_data)=="measurement_y"))){
    stop("Error in input dataset: No column named 'measurement_y' found.")
  }

  # -----------------------------------------
  # convert input data to data.table

  input_dt <- data.table::as.data.table(input_data)
  # -----------------------------------------
  # select only columns necessary for analysis

  input_dt <- input_dt[, list(subject, measurement_x, measurement_y)]

  # -----------------------------------------
  # delete rows with empty values

  if (any(is.na(input_dt)))
    message("Input dataset contains rows with empty values
      which will be automatically removed for analysis.")
  input_dt <- stats::na.omit(input_dt)

  # -----------------------------------------
  # check input data

  coll <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(input_dt$measurement_x, add = coll)
  checkmate::assert_numeric(input_dt$measurement_y, add = coll)
  checkmate::reportAssertions(coll)

  # -----------------------------------------
  return(
    input_dt = input_dt
  )
}
