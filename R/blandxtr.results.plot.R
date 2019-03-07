# function for visualization (plot) of the results of Olofsen

## ---- analysisResults

source("R/blandxtrMain.Olofsen.R")

## ---- analysisResults_plot

# Bland Altman-plot
library(ggplot2)
library(ggrepel)

ggplot(data = olofsen_result$bv$outputMeasurements) +
geom_point(mapping = aes(x = m_ij, y = d_ij)) +
# Add labels at points (without overlap) (increases runtime!)
# geom_text_repel(label=olofsen_result$bv$outputMeasurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
# Add a horizontal line at y = 0
geom_hline(aes(yintercept=0), colour='black', size=1) +
# Add a horizontal line at y = mean of all differences (d)
geom_hline(aes(yintercept=olofsen_result$bv$d), colour='green', size=1) +
# Add horizontal lines at limits of agreement
geom_hline(aes(yintercept=olofsen_result$loa$loa_l), colour='darkred', size=0.5) +
geom_hline(aes(yintercept=olofsen_result$loa$loa_u), colour='darkred', size=0.5) +
# Add horizontal lines at 95%-CI of limits of agreement
geom_hline(aes(yintercept=olofsen_result$loa_mover$ci_l_loa_l_mover), colour='red', size=0.5) +
geom_hline(aes(yintercept=olofsen_result$loa_mover$ci_u_loa_l_mover), colour='red', size=0.5) +
geom_hline(aes(yintercept=olofsen_result$loa_mover$ci_l_loa_u_mover), colour='red', size=0.5) +
geom_hline(aes(yintercept=olofsen_result$loa_mover$ci_u_loa_u_mover), colour='red', size=0.5)

