# function for visualization (plot) of the results of Olofsen

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

  return(
    list(
      plot_res = plot_res
    )
  )

}
