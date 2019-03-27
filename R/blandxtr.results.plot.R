#' @title Visualization (plot) of the results of modified Bland Altman-analysis
#'
#' @description \code{blandxtr.results.plot} visualizes (plot) the results of
#' modified Bland Altman-analysis performed with \code{blandxtrMain}
#'
#' @author Inga Koenemund \email{inga.koenemund@web.de}
#'
#' @param res list with results from \code{blandxtrMain}
#'
#' @return Plot showing analysis results
#' @return Plot showing modified analysis results
#' @return QQ-plot of individual means
#' @return QQ-plot of individual residuals
#' @return Plot of residuals vs mean
#' @return Plot of residuals vs ID

blandxtr_results_plot <- function (res) {

  # Bland Altman-plot
  library(ggplot2)
  library(ggrepel)

  plot_res <- ggplot(data = res$bv$outputMeasurements) +
    geom_point(mapping = aes(x = m_ij, y = d_ij)) +
    # Add labels at points (without overlap) (increases runtime!)
    # geom_text_repel(label=res$bv$outputMeasurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
    # Add a horizontal line at y = 0
    geom_hline(aes(yintercept=0), colour='black', size=1) +
    # Add a horizontal line at y = mean of all differences (d)
    geom_hline(aes(yintercept=res$bv$d), colour='green', size=1) +
    # Add horizontal lines at limits of agreement
    geom_hline(aes(yintercept=res$loa$loa_l), colour='darkred', size=0.5) +
    geom_hline(aes(yintercept=res$loa$loa_u), colour='darkred', size=0.5) +
    # Add horizontal lines at 95%-CI of limits of agreement
    geom_hline(aes(yintercept=res$loa_mover$ci_l_loa_l_mover), colour='red', size=0.5) +
    geom_hline(aes(yintercept=res$loa_mover$ci_u_loa_l_mover), colour='red', size=0.5) +
    geom_hline(aes(yintercept=res$loa_mover$ci_l_loa_u_mover), colour='red', size=0.5) +
    geom_hline(aes(yintercept=res$loa_mover$ci_u_loa_u_mover), colour='red', size=0.5)

  ggsave("report/plot_res.pdf", plot = plot_res, device="pdf")

  # ----------------------------------

  plot_res_mod <- ggplot(data = res$bv$outputMeasurements) +
    geom_point(mapping = aes(x = m_ij, y = d_ij)) +
    # Add labels at points (without overlap) (increases runtime!)
    # geom_text_repel(label=res$bv$outputMeasurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
    # Add a horizontal line at y = 0
    geom_hline(aes(yintercept=0), colour='black', size=1) +
    # Add a horizontal line at y = mean of all differences (d)
    geom_hline(aes(yintercept=res$bv$d), colour='green', size=1) +
    # Add horizontal lines at limits of agreement
    geom_hline(aes(yintercept=res$loa_mod$loa_l), colour='darkred', size=0.5) +
    geom_hline(aes(yintercept=res$loa_mod$loa_u), colour='darkred', size=0.5) +
    # Add horizontal lines at 95%-CI of limits of agreement
    geom_hline(aes(yintercept=res$loa_mover_mod$ci_l_loa_l_mover), colour='red', size=0.5) +
    geom_hline(aes(yintercept=res$loa_mover_mod$ci_u_loa_l_mover), colour='red', size=0.5) +
    geom_hline(aes(yintercept=res$loa_mover_mod$ci_l_loa_u_mover), colour='red', size=0.5) +
    geom_hline(aes(yintercept=res$loa_mover_mod$ci_u_loa_u_mover), colour='red', size=0.5)

  ggsave("report/plot_res_mod.pdf", plot = plot_res_mod, device="pdf")

  # ----------------------------------
  # QQ plot of individual means

  p <- ggplot (data = res$bv$outputSubjects, aes(sample=d_i))
  qq_ind_means <- p + stat_qq(col="blue") + stat_qq_line(col="green")

  ggsave("report/qq_ind_means.pdf", plot = qq_ind_means, device="pdf")

  # ----------------------------------
  # QQ plot of residuals


  p <- ggplot (data = res$bv$outputMeasurements, aes(sample=r_ij))
  qq_resid <- p + stat_qq(col="blue") + stat_qq_line(col="green")

  ggsave("report/qq_resid.pdf", plot = qq_resid, device="pdf")

  # ----------------------------------
  # plot of residuals vs mean

  plot_res_means <- ggplot(data = res$bv$outputMeasurements) +
    geom_point(mapping = aes(x = m_ij, y = r_ij)) +
    # Add a horizontal line at y = 1.96
    geom_hline(aes(yintercept=1.96), colour='red', size=1) +
    # Add a horizontal line at y = -1.96
    geom_hline(aes(yintercept=-1.96), colour='red', size=1)

  ggsave("report/plot_res_means.pdf", plot = plot_res_means, device="pdf")
  # ----------------------------------
  # plot of residuals vs ID

  plot_res_id <- ggplot(data = res$bv$outputMeasurements) +
    geom_point(mapping = aes(x = subject, y = r_ij)) +
    # Add a horizontal line at y = 1.96
    geom_hline(aes(yintercept=1.96), colour='red', size=1) +
    # Add a horizontal line at y = -1.96
    geom_hline(aes(yintercept=-1.96), colour='red', size=1)

  ggsave("report/plot_res_id.pdf", plot = plot_res_id, device="pdf")

  # ----------------------------------

  return(
    list(
      plot_res = plot_res,
      plot_res_mod = plot_res_mod,
      qq_ind_means = qq_ind_means,
      qq_resid = qq_resid,
      plot_res_means = plot_res_means,
      plot_res_id = plot_res_id
    )
  )

}
