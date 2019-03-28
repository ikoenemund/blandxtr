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

  # plot_res <- ggplot(data = res$bv$outputMeasurements) +
  #   geom_point(mapping = aes(x = m_ij, y = d_ij)) +
  #   # Add labels at points (without overlap) (increases runtime!)
  #   # geom_text_repel(label=res$bv$outputMeasurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
  #   # Add a horizontal line at y = 0
  #   geom_hline(aes(yintercept=0), colour='black', size=1) +
  #   # Add a horizontal line at y = mean of all differences (d)
  #   geom_hline(aes(yintercept=res$bv$d), colour='green', size=1) +
  #   # Add horizontal lines at limits of agreement
  #   geom_hline(aes(yintercept=res$loa$loa_l), colour='darkred', size=0.5) +
  #   geom_hline(aes(yintercept=res$loa$loa_u), colour='darkred', size=0.5) +
  #   # Add horizontal lines at 95%-CI of limits of agreement
  #   geom_hline(aes(yintercept=res$loa_mover$ci_l_loa_l_mover), colour='red', size=0.5) +
  #   geom_hline(aes(yintercept=res$loa_mover$ci_u_loa_l_mover), colour='red', size=0.5) +
  #   geom_hline(aes(yintercept=res$loa_mover$ci_l_loa_u_mover), colour='red', size=0.5) +
  #   geom_hline(aes(yintercept=res$loa_mover$ci_u_loa_u_mover), colour='red', size=0.5) +
  # labs(x="mean value of X_ij and Y_ij", y="difference (X_ij - Y_ij)")
  # ggsave("report/plot_res.pdf", plot = plot_res, device="pdf")

  #### black/white

  plot_res <- ggplot(data = res$bv$outputMeasurements) +
    geom_point(mapping = aes(x = m_ij, y = d_ij)) +
    # Add labels at points (without overlap) (increases runtime!)
    # geom_text_repel(label=res$bv$outputMeasurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
    # Add a horizontal line at y = 0
    geom_hline(aes(yintercept=0), size=0.5, linetype = "solid") +
    # Add a horizontal line at y = mean of all differences (d)
    geom_hline(aes(yintercept=res$bv$d), size=0.5, linetype = "dashed") +
    # Add horizontal lines at limits of agreement
    geom_hline(aes(yintercept=res$loa$loa_l), size=0.5, linetype = "dotdash") +
    geom_hline(aes(yintercept=res$loa$loa_u), size=0.5, linetype = "dotdash") +
    # Add horizontal lines at 95%-CI of limits of agreement
    geom_hline(aes(yintercept=res$loa_mover$ci_l_loa_l_mover), size=0.5, linetype = "dotted") +
    geom_hline(aes(yintercept=res$loa_mover$ci_u_loa_l_mover), size=0.5, linetype = "dotted") +
    geom_hline(aes(yintercept=res$loa_mover$ci_l_loa_u_mover), size=0.5, linetype = "dotted") +
    geom_hline(aes(yintercept=res$loa_mover$ci_u_loa_u_mover), size=0.5, linetype = "dotted") +
    labs(x="mean value of X_ij and Y_ij", y="difference (X_ij - Y_ij)")

  ggsave("report/plot_res.pdf", plot = plot_res, device="pdf")
  ####

  # ----------------------------------
  # Bland Altman-plot (modified analysis)

  # plot_res_mod <- ggplot(data = res$bv$outputMeasurements) +
  #   geom_point(mapping = aes(x = m_ij, y = d_ij)) +
  #   # Add labels at points (without overlap) (increases runtime!)
  #   # geom_text_repel(label=res$bv$outputMeasurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
  #   # Add a horizontal line at y = 0
  #   geom_hline(aes(yintercept=0), colour='black', size=1) +
  #   # Add a horizontal line at y = mean of all differences (d)
  #   geom_hline(aes(yintercept=res$bv$d), colour='green', size=1) +
  #   # Add horizontal lines at limits of agreement
  #   geom_hline(aes(yintercept=res$loa_mod$loa_l), colour='darkred', size=0.5) +
  #   geom_hline(aes(yintercept=res$loa_mod$loa_u), colour='darkred', size=0.5) +
  #   # Add horizontal lines at 95%-CI of limits of agreement
  #   geom_hline(aes(yintercept=res$loa_mover_mod$ci_l_loa_l_mover), colour='red', size=0.5) +
  #   geom_hline(aes(yintercept=res$loa_mover_mod$ci_u_loa_l_mover), colour='red', size=0.5) +
  #   geom_hline(aes(yintercept=res$loa_mover_mod$ci_l_loa_u_mover), colour='red', size=0.5) +
  #   geom_hline(aes(yintercept=res$loa_mover_mod$ci_u_loa_u_mover), colour='red', size=0.5) +
  # labs(x="mean value of X_ij and Y_ij", y="difference (X_ij - Y_ij)")
  #
  # ggsave("report/plot_res_mod.pdf", plot = plot_res_mod, device="pdf")


  #### black/white
  plot_res_mod <- ggplot(data = res$bv$outputMeasurements) +
    geom_point(mapping = aes(x = m_ij, y = d_ij)) +
    # Add labels at points (without overlap) (increases runtime!)
    # geom_text_repel(label=res$bv$outputMeasurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
    # Add a horizontal line at y = 0
    geom_hline(aes(yintercept=0), size=0.5, linetype = "solid") +
    # Add a horizontal line at y = mean of all differences (d)
    geom_hline(aes(yintercept=res$bv$d), size=0.5, linetype = "dashed") +
    # Add horizontal lines at limits of agreement
    geom_hline(aes(yintercept=res$loa_mod$loa_l), size=0.5, linetype = "dotdash") +
    geom_hline(aes(yintercept=res$loa_mod$loa_u), size=0.5, linetype = "dotdash") +
    # Add horizontal lines at 95%-CI of limits of agreement
    geom_hline(aes(yintercept=res$loa_mover_mod$ci_l_loa_l_mover), size=0.5, linetype = "dotted") +
    geom_hline(aes(yintercept=res$loa_mover_mod$ci_u_loa_l_mover), size=0.5, linetype = "dotted") +
    geom_hline(aes(yintercept=res$loa_mover_mod$ci_l_loa_u_mover), size=0.5, linetype = "dotted") +
    geom_hline(aes(yintercept=res$loa_mover_mod$ci_u_loa_u_mover), size=0.5, linetype = "dotted") +
    labs(x="mean value of X_ij and Y_ij", y="difference (X_ij - Y_ij)")

  ggsave("report/plot_res_mod.pdf", plot = plot_res_mod, device="pdf")
  ####

  # ----------------------------------
  # QQ plot of individual means

  p <- ggplot (data = res$bv$outputSubjects, aes(sample=d_i))
  qq_ind_means <- p + stat_qq(col="black") + stat_qq_line(col="black")

  ggsave("report/qq_ind_means.pdf", plot = qq_ind_means, device="pdf")

  # ----------------------------------
  # QQ plot of residuals


  p <- ggplot (data = res$bv$outputMeasurements, aes(sample=r_ij))
  qq_resid <- p + stat_qq(col="black") + stat_qq_line(col="black")

  ggsave("report/qq_resid.pdf", plot = qq_resid, device="pdf")

  # ----------------------------------
  # plot of residuals vs mean

  plot_res_means <- ggplot(data = res$bv$outputMeasurements) +
    geom_point(mapping = aes(x = m_ij, y = r_ij)) +
    # Add a horizontal line at y = 1.96
    geom_hline(aes(yintercept=1.96), size=1, linetype = "dashed") +
    # Add a horizontal line at y = -1.96
    geom_hline(aes(yintercept=-1.96), size=1, linetype = "dashed") +
    labs(x="mean value of X_ij and Y_ij", y="residual of X_ij and Y_ij")

  ggsave("report/plot_res_means.pdf", plot = plot_res_means, device="pdf")
  # ----------------------------------
  # plot of residuals vs ID

  plot_res_id <- ggplot(data = res$bv$outputMeasurements) +
    geom_point(mapping = aes(x = subject, y = r_ij)) +
    # Add a horizontal line at y = 1.96
    geom_hline(aes(yintercept=1.96), size=1, linetype = "dashed") +
    # Add a horizontal line at y = -1.96
    geom_hline(aes(yintercept=-1.96), size=1, linetype = "dashed") +
    labs(x="ID", y="residual of X_ij and Y_ij")

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

#### TEST

# plot_res <- ggplot(data = res$bv$outputMeasurements) +
#   geom_point(mapping = aes(x = m_ij, y = d_ij)) +
#   # Add labels at points (without overlap) (increases runtime!)
#   # geom_text_repel(label=res$bv$outputMeasurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
#   # Add a horizontal line at y = 0
#   geom_hline(aes(yintercept=0, linetype = "zeroline"), color='black', size=1) +
#   # Add a horizontal line at y = mean of all differences (d)
#   geom_hline(aes(yintercept=res$bv$d, linetype = "bias"), color='green', size=1) +
#   # Add horizontal lines at limits of agreement
#   geom_hline(aes(yintercept=res$loa$loa_l, linetype = "limit of agreement"), color='darkred', size=0.5) +
#   geom_hline(aes(yintercept=res$loa$loa_u, linetype = "limit of agreement"), color='darkred', size=0.5) +
#   # Add horizontal lines at 95%-CI of limits of agreement
#   geom_hline(aes(yintercept=res$loa_mover$ci_l_loa_l_mover, linetype = "CI of LoA"), color='red', size=0.5) +
#   geom_hline(aes(yintercept=res$loa_mover$ci_u_loa_l_mover, linetype = "CI of LoA"), color='red', size=0.5) +
#   geom_hline(aes(yintercept=res$loa_mover$ci_l_loa_u_mover, linetype = "CI of LoA"), color='red', size=0.5) +
#   geom_hline(aes(yintercept=res$loa_mover$ci_u_loa_u_mover, linetype = "CI of LoA"), color='red', size=0.5) +
#   labs(x="mean", y="difference") +
#   # scale_linetype_manual(name = "legend", values = c("zeroline"= 1 ,"bias" = 1,
#   #   "limit of agreement" = 1, "CI of LoA" = 1))
#   # guides (linetype = guide_legend(override.aes = list(color = c("green", "darkred", "red", "black"))))
# scale_linetype_manual(name="legend", values = c(1,1,1,1), guide = guide_legend(override.aes = list(color = c("green", "red", "darkred", "black"))))
#
# ggsave("report/plot_res.pdf", plot = plot_res, device="pdf")



#  plot_res <- ggplot(data = res$bv$outputMeasurements) +
#   geom_point(mapping = aes(x = m_ij, y = d_ij)) +
#   # Add labels at points (without overlap) (increases runtime!)
#   # geom_text_repel(label=res$bv$outputMeasurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
#   # Add a horizontal line at y = 0
#   geom_hline(aes(yintercept=0, colour="black"), size=1) +
#   # Add a horizontal line at y = mean of all differences (d)
#   geom_hline(aes(yintercept=res$bv$d, colour="green"), size=1) +
#   # Add horizontal lines at limits of agreement
#   geom_hline(aes(yintercept=res$loa$loa_l, colour="darkred"), size=0.5) +
#   geom_hline(aes(yintercept=res$loa$loa_u, colour="darkred"), size=0.5) +
#   # Add horizontal lines at 95%-CI of limits of agreement
#   geom_hline(aes(yintercept=res$loa_mover$ci_l_loa_l_mover, colour="red"), size=0.5) +
#   geom_hline(aes(yintercept=res$loa_mover$ci_u_loa_l_mover, colour="red"), size=0.5) +
#   geom_hline(aes(yintercept=res$loa_mover$ci_l_loa_u_mover, colour="red"), size=0.5) +
#   geom_hline(aes(yintercept=res$loa_mover$ci_u_loa_u_mover, colour="red")
#     , size=0.5) +
#   scale_color_manual(values = c("green", "darkred", "red", "black"))
#
# ggsave("report/plot_res.pdf", plot = plot_res, device="pdf")
####

#### TEST2: shorter code
# res <- olofsen_result$res
#
# test_data <- data.frame(linetype = c(1,1,1,1,1,1,1,1), color = c('black', 'green', 'darkred', 'darkred', 'red', 'red', 'red', 'red'), y = c(0, res$bv$d, res$loa$loa_l, res$loa$loa_u, res$loa_mover$ci_l_loa_l_mover, res$loa_mover$ci_u_loa_l_mover, res$loa_mover$ci_l_loa_u_mover, res$loa_mover$ci_u_loa_u_mover), legend = c('zeroline', 'bias', 'LoA', 'LoA', 'CI of LoA', 'CI of LoA', 'CI of LoA', 'CI of LoA'))
# plot_res <- ggplot(data = res$bv$outputMeasurements) +
#   geom_point(mapping = aes(x = m_ij, y = d_ij)) +
#   geom_hline(data = test_data, aes(yintercept = y), color = test_data$color)
####
