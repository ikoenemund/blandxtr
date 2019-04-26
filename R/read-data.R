#' @title Read input dataset for blandxtr
#'
#' @description \code{blandxtr_readData} reads input dataset (as csv) from
#' given path and returns a data.table for further analysis
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param path path where input dataset is stored (as csv)
#'
#' @return A data.table containing input data for further analysis
#'
#' @export

read_data <- function(path){

  # check input
  if (!(is.character(path)))
    stop("'path' is not a character.")

  # -----------------------------------------
  # read data from given path
  dt <- fread(path)
  # data.frame to data.table
  setDT(dt)

}


