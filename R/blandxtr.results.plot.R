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

  library(ggplot2)
  library(ggrepel)

  # Bland Altman-plot
  # black/white

  # ---- plot_res

  plot_res <- ggplot(data = res$bv$outputMeasurements) +
    geom_point(mapping = aes(x = m_ij, y = d_ij)) +
    # Add labels at points (without overlap) (increases runtime!)
    # geom_text_repel(label=res$bv$outputMeasurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
    # Add a horizontal line at y = 0
    geom_hline(aes(yintercept=0, linetype = "zeroline"), size=1) +
    # Add a horizontal line at y = mean of all differences (d)
    geom_hline(aes(yintercept=res$bv$d, linetype = "bias"), size=1) +
    # Add horizontal lines at limits of agreement
    geom_hline(aes(yintercept=res$loa$loa_l, linetype = "limit of agreement"), size=0.5) +
    geom_hline(aes(yintercept=res$loa$loa_u, linetype = "limit of agreement"), size=0.5) +
    # Add horizontal lines at 95%-CI of limits of agreement
    geom_hline(aes(yintercept=res$loa_mover$ci_l_loa_l_mover, linetype = "CI of LoA"), size=0.5) +
    geom_hline(aes(yintercept=res$loa_mover$ci_u_loa_l_mover, linetype = "CI of LoA"), size=0.5) +
    geom_hline(aes(yintercept=res$loa_mover$ci_l_loa_u_mover, linetype = "CI of LoA"), size=0.5) +
    geom_hline(aes(yintercept=res$loa_mover$ci_u_loa_u_mover, linetype = "CI of LoA"), size=0.5) +
    labs(x="mean value of X_ij and Y_ij", y="difference (X_ij - Y_ij)") +
    scale_linetype_manual(name="Legend", breaks = c("zeroline", "bias", "limit of agreement", "CI of LoA"), values = c("zeroline" = 1, "bias" = 2, "limit of agreement" = 3, "CI of LoA" = 4)) +
    theme(legend.direction = "horizontal", legend.position = "bottom",
      legend.key.size = unit(3, "lines"))
  # ----

  ggsave("report/plots-png/plot-res.png", width = 12, height = 12, plot = plot_res,
    device="png")
  ggsave("report/plots-svg/plot-res.svg", width = 12, height = 12, plot = plot_res,
    device="svg")

  # ----------------------------------
  # Bland Altman-plot (modified analysis)
  # black/white

  plot_res_mod <- ggplot(data = res$bv$outputMeasurements) +
    geom_point(mapping = aes(x = m_ij, y = d_ij)) +
    # Add labels at points (without overlap) (increases runtime!)
    # geom_text_repel(label=res$bv$outputMeasurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
    # Add a horizontal line at y = 0
    geom_hline(aes(yintercept=0, linetype = "zeroline"), size=1) +
    # Add a horizontal line at y = mean of all differences (d)
    geom_hline(aes(yintercept=res$bv$d, linetype = "bias"), size=1) +
    # Add horizontal lines at limits of agreement
    geom_hline(aes(yintercept=res$loa_mod$loa_l, linetype = "limit of agreement"), size=0.5) +
    geom_hline(aes(yintercept=res$loa_mod$loa_u, linetype = "limit of agreement"), size=0.5) +
    # Add horizontal lines at 95%-CI of limits of agreement
    geom_hline(aes(yintercept=res$loa_mover_mod$ci_l_loa_l_mover, linetype = "CI of LoA"), size=0.5) +
    geom_hline(aes(yintercept=res$loa_mover_mod$ci_u_loa_l_mover, linetype = "CI of LoA"), size=0.5) +
    geom_hline(aes(yintercept=res$loa_mover_mod$ci_l_loa_u_mover, linetype = "CI of LoA"), size=0.5) +
    geom_hline(aes(yintercept=res$loa_mover_mod$ci_u_loa_u_mover, linetype = "CI of LoA"), size=0.5) +
    labs(x="mean value of X_ij and Y_ij", y="difference (X_ij - Y_ij)") +
    scale_linetype_manual(name="Legend", breaks = c("zeroline", "bias", "limit of agreement", "CI of LoA"), values = c("zeroline" = 1, "bias" = 2, "limit of agreement" = 3, "CI of LoA" = 4)) +
    theme(legend.direction = "horizontal", legend.position = "bottom",
      legend.key.size = unit(3, "lines"))

  ggsave("report/plots-png/plot-res-mod.png", width = 12, height = 12,
    plot = plot_res_mod, device="png")
  ggsave("report/plots-svg/plot-res-mod.svg", width = 12, height = 12,
    plot = plot_res_mod, device="svg")

  # ----------------------------------
  # QQ plot of individual means

  p <- ggplot (data = res$bv$outputSubjects, aes(sample=d_i))
  qq_ind_means <- p + stat_qq(col="black") + stat_qq_line(col="black")

  ggsave("report/plots-png/qq-ind-means.png", plot = qq_ind_means, device="png")
  ggsave("report/plots-svg/qq-ind-means.svg", plot = qq_ind_means, device="svg")

  # ----------------------------------
  # QQ plot of residuals


  p <- ggplot (data = res$bv$outputMeasurements, aes(sample=r_ij))
  qq_resid <- p + stat_qq(col="black") + stat_qq_line(col="black")

  ggsave("report/plots-png/qq-resid.png", plot = qq_resid, device="png")
  ggsave("report/plots-svg/qq-resid.svg", plot = qq_resid, device="svg")

  # ----------------------------------
  # plot of residuals vs mean

  plot_res_means <- ggplot(data = res$bv$outputMeasurements) +
    geom_point(mapping = aes(x = m_ij, y = r_ij)) +
    # Add a horizontal line at y = 1.96
    geom_hline(aes(yintercept=1.96), size=1, linetype = "dashed") +
    # Add a horizontal line at y = -1.96
    geom_hline(aes(yintercept=-1.96), size=1, linetype = "dashed") +
    labs(x="mean value of X_ij and Y_ij", y="residual of X_ij and Y_ij")

  ggsave("report/plots-png/plot-res-means.png", plot = plot_res_means, device="png")
  ggsave("report/plots-svg/plot-res-means.svg", plot = plot_res_means, device="svg")
  # ----------------------------------
  # plot of residuals vs ID

  plot_res_id <- ggplot(data = res$bv$outputMeasurements) +
    geom_point(mapping = aes(x = subject, y = r_ij)) +
    # Add a horizontal line at y = 1.96
    geom_hline(aes(yintercept=1.96), size=1, linetype = "dashed") +
    # Add a horizontal line at y = -1.96
    geom_hline(aes(yintercept=-1.96), size=1, linetype = "dashed") +
    labs(x="ID", y="residual of X_ij and Y_ij")

  ggsave("report/plots-png/plot-res-id.png", plot = plot_res_id, device="png")
  ggsave("report/plots-svg/plot-res-id.svg", plot = plot_res_id, device="svg")

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
