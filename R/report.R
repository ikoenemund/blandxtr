#' @title Create report for modified Bland Altman-analysis
#'
#' @description \code{report} returns a report of results (tables and figures)
#' of modified Bland Altman-analysis as html-, pdf- or word-document.
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param tab A list of tables to be displayed (generated with
#' \code{generate-tables}-function from \code{blandxtr}-package
#' @param fig A list of plots to be displayed (generated with
#' \code{plot}-function from \code{blandxtr}-package (\code{plot.blandxtr})
#' @param output_format 'html_document' (default), 'pdf_document' or
#' 'word_document'
#' @param output_dir String specifying the directory for storing the output file
#'
#' @return A report of modified Bland Altman-analysis' results.
#'
#' @export

report <- function(tab, fig, output_format, output_dir){
  # -----------------------
  # setting default values for 'output_format' and 'output_dir'

  if (missing(output_format)) {
    output_format <- 'html_document'
    warning("Variable `output_format` is missing. Setting to 'html_document'.")
  }

  if (missing(output_dir)) {
    output_dir <- NULL
    warning("Variable `output_dir` is missing. Setting to NULL. Check your
      directory for installed packages and see 'blandxtr/rmd' to find the
      report.")
  }
  # -----------------------
  rmarkdown::render(input = system.file("rmd", "report.rmd",
      package = "blandxtr"), output_format = output_format,
      params = list(tab = tab, fig = fig),
      output_dir = output_dir)
}
