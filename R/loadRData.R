#' @title Load RData-files
#'
#' @description \code{loadRData} returns a data.frame from a RData-file.
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param fileName name of the rData-file
#' @return A data.frame containing data from fileName.
#'

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
