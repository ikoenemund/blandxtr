#' @title Read input dataset for blandxtr
#'
#' @description \code{blandxtr_readData} reads input dataset (as csv) from
#' given path and returns a data.table for further analysis
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param path path where input dataset is stored (as csv)
#'
#' @return A data.table containing input data for further analysis

blandxtr_readData <- function(path){

  library(data.table)

  # -----------------------------------------
  # read data from given path
  dt <- fread(path)
  # data.frame to data.table
  setDT(dt)

}


