% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.blandxtr}
\alias{plot.blandxtr}
\title{Visualizationof the results of advanced Bland Altman-analysis}
\usage{
\method{plot}{blandxtr}(x, type, ...)
}
\arguments{
\item{x}{list with results from \code{blandxtr}}

\item{type}{which plot to produce (1: unmodified analysis results,
2: modified analysis results, 3: list with all plots available)}

\item{...}{other arguments not used by this method}
}
\value{
A list with the following elements (ggplot2-objects) is returned
(for type=3)
\itemize{
 \item{\code{plot_res}} {Bland Altman-plot showing unmodified
 analysis results}
 \item{\code{plot_res_mod}} {Bland Altman-plot showing modified
 analysis results}
 \item{\code{qq_ind_means}} {QQ-plot of individual means of differnces}
 \item{\code{qq_resid}} {QQ-plot of individual residuals}
 \item{\code{plot_res_means}} {Plot of residuals vs means of each
 measurement}
 \item{\code{plot_res_id}} {Plot of residuals vs subject ID}
 }
 type = 1 only \code{plot_res} or type = 2 only \code{plot_res_mod} will
 be returned.
}
\description{
\code{plot.blandxtr} visualizes the results of
advanced Bland Altman-analysis performed with \code{blandxtr}.
}
\author{
Inga Koenemund \email{inga.koenemund@web.de}
}
