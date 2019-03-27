#' @title Preparation of data for Bland Altman analysis
#'
#' @description \code{blandxtr.prepareData} checks input data.table for
#' common errors (missing columns that are required, wrong type of input data)
#' and reduces it to the columns necessary for analysis with \code{blandxtrMain}.
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param input_dt data.table with input dataset
#'
#' @note Rows with missing values will be removed from analysis.
#'
#' @return Prepared input data as data.table
#'

# Check and prepare input data

blandxtr_prepareData <- function (input_dt){

  # check columns
  if (!(any(colnames(input_dt)=="subject"))){
    stop("Error in input dataset: No column named 'subject' found.")
  } else if (!(any(colnames(input_dt)=="measurementX"))) {
    stop("Error in input dataset: No column named 'measurementX' found.")
  } else if (!(any(colnames(input_dt)=="measurementY"))){
    stop("Error in input dataset: No column named 'measurementX' found.")
  }

  # select only columns necessary for analysis
  input_dt <- input_dt[, list(subject, measurementX, measurementY)]

  # delete rows with empty values
  if (any(is.na(input_dt)))
    message("Input dataset contains rows with empty values
      which will be automatically removed for analysis.")
  input_dt <- na.omit(input_dt)

  # check input data
  if(!(is.numeric(input_dt$measurementX)))
    stop("Error in input dataset: Column 'measurementX' is not numeric.")
  if(!(is.numeric(input_dt$measurementY)))
    stop("Error in input dataset: Column 'measurementY' is not numeric.")

  return(
    input_dt = input_dt
  )
}
