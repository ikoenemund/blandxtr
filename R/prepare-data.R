#' @title Preparation of data for Bland Altman analysis
#'
#' @description \code{blandxtr.prepareData} checks input data.table for
#' common errors (missing columns that are required, wrong type of input data)
#' and reduces it to the columns necessary for analysis with \code{blandxtrMain}.
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param input_dt data.table with input dataset
#'
#' @note Rows with missing values will be removed from analysis.
#'
#' @return Prepared input data as data.table
#'
#' @export
#'

# Check and prepare input data

prepare_data <- function (input_dt){

  # -----------------------------------------
  # check input
  checkmate::assert_data_table(input_dt, add = coll)
  # -----------------------------------------

  # check columns
  if (!(any(colnames(input_dt)=="subject"))){
    stop("Error in input dataset: No column named 'subject' found.")
  } else if (!(any(colnames(input_dt)=="measurement_x"))) {
    stop("Error in input dataset: No column named 'measurement_x' found.")
  } else if (!(any(colnames(input_dt)=="measurement_y"))){
    stop("Error in input dataset: No column named 'measurement_y' found.")
  }

  # select only columns necessary for analysis
  input_dt <- input_dt[, list(subject, measurement_x, measurement_y)]

  # delete rows with empty values
  if (any(is.na(input_dt)))
    message("Input dataset contains rows with empty values
      which will be automatically removed for analysis.")
  input_dt <- stats::na.omit(input_dt)

  # check input data

  coll <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(input_dt$measurement_x, add = coll)
  checkmate::assert_numeric(input_dt$measurement_y, add = coll)
  checkmate::reportAssertions(coll)

  # if(!(is.numeric(input_dt$measurement_x)))
  #   stop("Error in input dataset: Column 'measurement_x' is not numeric.")
  # if(!(is.numeric(input_dt$measurement_y)))
  #   stop("Error in input dataset: Column 'measurement_y' is not numeric.")

  return(
    input_dt = input_dt
  )
}
